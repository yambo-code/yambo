!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine ELPH_gkkp_analysis(k,en,q)
 !
 use pars,                ONLY:SP,schlen
 use units,               ONLY:HA2EV
 use drivers,             ONLY:l_real_time
 use com,                 ONLY:msg,of_open_close
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp,qindx_S
 use ELPH,                ONLY:ph_modes,elph_gkkp,ph_freqs_sq,&
&                              elph_nDBs,elph_use_q_grid,ELPH_global_free,&
&                              E_k_plus_q,setup_k_plus_q_levels,ph_qpt
 use IO_m,                ONLY:io_control,OP_RD,RD_CL_IF_END,DUMP,RD_CL
 use QP_m,                ONLY:QP_n_states,QP_table,SC_band_mixing
 use timing,              ONLY:live_timing
 use functions,           ONLY:Fermi_fnc_derivative
 use vec_operate,         ONLY:iku_v_norm,sort
 use stderr,              ONLY:set_real_printed_length,intc
 !
 implicit none
 type(levels) ::en
 type(bz_samp)::k,q
 !
 ! Work Space 
 !
 integer               :: nq_todo,iq,ik_ibz,i_qp,ib1,ib2,ik_bz,ik_m_q,im,i_f,i1,i2,i_o
 integer,    parameter :: n_p=1000
 real(SP),   parameter :: Q_broad=0.02,E_broad=0.1
 real(SP)              :: q_weight,E_nk_m_np_kmq,ph_E,&
&                         Q_range(n_p),E_range(n_p),Q_step,E_step
 real(SP), allocatable :: gkkp_sq(:,:,:,:),Q_mod(:),Q_data(:,:),E_data(:,:),dV_average(:,:),dV_spread(:,:)
 integer , allocatable :: Q_sort_index(:),dE_sort_index(:)
 character(schlen)     :: o_file_name(2)
 !
 !I/O
 !
 integer           ::io_err,ID
 integer, external ::ioELPH
 !
 call section("*","== Electron-Phonon M.E. Analysis ==")
 !------------------------------------------------------
 !
 call io_control(ACTION=OP_RD,SEC=(/1/),MODE=DUMP,ID=ID)
 io_err=ioELPH(ID,'gkkp')
 !
 call msg('s',':: Checking database ...')
 if (io_err<0) then
   call msg('l','not found')
   return
 endif
 call msg('l','sane')
 !
 nq_todo=elph_nDBs
 if (elph_use_q_grid) nq_todo=q%nibz
 !
 ! calculate q%weights ...
 !
 call k_expand(q) 
 !
 ! ... and the QP_table
 !
 l_real_time=.TRUE.
 SC_band_mixing=100.
 call QP_state_table_setup(en)
 !
 allocate(gkkp_sq(QP_n_states,ph_modes,nq_todo,3),Q_mod(nq_todo),Q_sort_index(nq_todo))
 allocate(dE_sort_index(nq_todo),Q_data(n_p,ph_modes),E_data(n_p,ph_modes))
 allocate(dV_average(QP_n_states,ph_modes),dV_spread(QP_n_states,ph_modes),E_data(n_p,ph_modes))
 !
 gkkp_sq=0.
 !
 call live_timing('Q-loop',nq_todo,SERIAL=.TRUE.)
 !
 do iq=1,nq_todo 
   !
   ! I/O
   !
   call io_control(ACTION=RD_CL_IF_END,SEC=(/iq+1/),ID=ID)
   io_err=ioELPH(ID,'gkkp')
   !
   if (.not.elph_use_q_grid) call setup_k_plus_q_levels(en%Efermi(1))
   !
   ! Q weight
   !
   q_weight=1./real(elph_nDBs)
   if (elph_use_q_grid) then
     q_weight=q%weights(iq)
     Q_mod(iq)=iku_v_norm(q%pt(iq,:))
   else
     Q_mod(iq)=iku_v_norm(ph_qpt(iq,:))
   endif
   !
   do i_qp=1,QP_n_states
     !
     ! n   =QP_table(i_qp,1)
     ! n'  =QP_table(i_qp,2)
     ! k   =QP_table(i_qp,3)
     !
     ib1   =QP_table(i_qp,1)
     ib2   =QP_table(i_qp,2)
     ik_ibz=QP_table(i_qp,3)
     !
     if (elph_use_q_grid) then
       ik_m_q=qindx_S(ik_ibz,iq,1)
       E_nk_m_np_kmq= en%E(ib2,ik_m_q,1)-en%E(ib1,ik_ibz,1)

     else
       ik_bz=sum(k%nstar(:ik_ibz-1))+1
       E_nk_m_np_kmq=E_k_plus_q(ib2,ik_bz,1)-en%E(ib1,ik_ibz,1)
     endif
     !
     do im=1,ph_modes
       !  
       ph_E = 1.
       !
! sqrt( MAX( ph_freqs_sq(iq,im),0. ) )
       !
       if (ph_E<1.E-10) cycle
       !  
       ! Note that in Yambo elph_gkkp contains <dV/dr>, this the quantity I need to model
       ! is
       !  
       !    |elph_gkkp|^2/(2 * w_qm *Nq)
       !  
       !  
       gkkp_sq(i_qp, 1,iq,1)=E_nk_m_np_kmq
       gkkp_sq(i_qp,im,iq,2)=conjg(elph_gkkp(ik_bz,im,ib2,ib1))*&
&                                  elph_gkkp(ik_bz,im,ib2,ib1)
       !
       if (ib1==ib2) gkkp_sq(i_qp, im,iq,3)=frohlich_g_square(Q_mod(iq),ph_E)
       !
     enddo
     !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 ! Energy and Q ranges
 !
 E_step= ( maxval(gkkp_sq(:,1,:,1))-minval(gkkp_sq(:,1,:,1)) )/float(n_p)
 do i_f=1,n_p
   E_range(i_f)=minval(gkkp_sq(:,1,:,1))+i_f*E_step
 enddo
 !
 Q_step= maxval(Q_mod(:))/float(n_p)
 do i_f=1,n_p
   Q_range(i_f)=i_f*Q_step
 enddo
 !
 ! Output file
 !
 call set_real_printed_length(f_length=20,g_length=20)
 !
 call sort(arrin=Q_mod,indx=Q_sort_index)
 !
 do i_qp=1,QP_n_states
   !
   ib1   =QP_table(i_qp,1)
   ib2   =QP_table(i_qp,2)
   ik_ibz=QP_table(i_qp,3)
   o_file_name(1)='gkkp_sq_k'//trim(intc(ik_ibz))//'_b'//trim(intc(ib1))//'_bp'//trim(intc(ib2))//'_dE'
   o_file_name(2)='gkkp_sq_k'//trim(intc(ik_ibz))//'_b'//trim(intc(ib1))//'_bp'//trim(intc(ib2))//'_Q'
   !
   do i_o=1,2
     call of_open_close(o_file_name(i_o),'ot')
     call msg(o_file_name(i_o),'#','',INDENT=0)
     if (i_o==1) call msg('o _dE','# Gkkp M.E. analysis (vs E-E`)','',INDENT=0)
     if (i_o==2) call msg('o _Q', '# Gkkp M.E. analysis (vs |Q|)','',INDENT=0)
     call msg("o "//o_file_name(i_o),'#','',INDENT=0)
     if (i_o==1) call msg('o _dE','#',(/'dE      [eV]','|g|^2 [eV^2]'/),INDENT=0,USE_TABS=.true.)
     if (i_o==2) call msg('o _Q', '#',(/'|Q|   [a.u.]','|g|^2 [eV^2]'/),INDENT=0,USE_TABS=.true.)
     call msg("o "//o_file_name(i_o),'#','',INDENT=0)
   enddo
   !
   if (.FALSE.) then
     !
     E_data=0.
     Q_data=0.
     !
     do iq=1,nq_todo 
       do im=1,ph_modes 
         !
         do i_f=1,n_p
           E_data(i_f,im)= E_data(i_f,im)+gkkp_sq(i_qp,im,iq,2)*Fermi_fnc_derivative(E_range(i_f)-gkkp_sq(i_qp,1,iq,1),E_broad)
           Q_data(i_f,im)= Q_data(i_f,im)+gkkp_sq(i_qp,im,iq,2)*Fermi_fnc_derivative(Q_range(i_f)-Q_mod(iq),Q_broad)
         enddo
         !
       enddo
     enddo
     do i_f=1,n_p
       call msg('o '//trim(o_file_name(1)),'',(/E_range(i_f),E_data(i_f,:)/)*HA2EV,INDENT=-2,USE_TABS=.true.)
       call msg('o '//trim(o_file_name(2)),'',(/Q_range(i_f),Q_data(i_f,:)*HA2EV/)  ,INDENT=-2,USE_TABS=.true.)
     enddo
     !
   else
     !
     call sort(arrin=gkkp_sq(i_qp,1,:,1),indx=dE_sort_index)
     !
     do iq=1,nq_todo 
       i1=dE_sort_index(iq)
       call msg('o '//trim(o_file_name(1)),'',(/gkkp_sq(i_qp,1,iq,1),gkkp_sq(i_qp,:,i1,2)/)*HA2EV,INDENT=-2,USE_TABS=.true.)
       i1=Q_sort_index(iq)
       call msg('o '//trim(o_file_name(2)),'',(/Q_mod(iq),gkkp_sq(i_qp,:,i1,2)*HA2EV/),INDENT=-2,USE_TABS=.true.)
     enddo
     !
     call of_open_close(o_file_name(1))
     call of_open_close(o_file_name(2))
     !
   endif
   !
 enddo
 !
 call set_real_printed_length()
 !
 ! CLEAN
 !
 deallocate(gkkp_sq,Q_mod,Q_sort_index,dE_sort_index)
 deallocate(Q_data,E_data)
 call elph_global_free()
 !
 contains
   !
   real(SP) function frohlich_g_square(q_mod,ph_E)
   !
   use pars,           ONLY:SP,pi
   use D_lattice,      ONLY:DL_vol
   implicit none
   !
   real(SP)::q_mod,reduced_mass,ph_E,alpha_frohlich
   !
   ! GaAs (adim)
   !
   reduced_mass=.0591
   alpha_frohlich=1.
   !
   frohlich_g_square=alpha_frohlich*4.*pi*ph_E**(5./2.)/ (sqrt(2.*reduced_mass)*DL_vol*q_mod**2.)
   !
   end function 
   !
end subroutine
