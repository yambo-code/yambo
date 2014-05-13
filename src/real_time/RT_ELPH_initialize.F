!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine RT_ELPH_initialize(E,k,q)
 ! 
 use pars,           ONLY:SP,pi
 use units,          ONLY:HA2EV
 use real_time,      ONLY:q_weight_RT,elph_gkkp_expanded,THETA_matrix,&
&                         THETA_matrix_index,Q_io_map,K_io_map,&
&                         l_RT_CCA_Kernel,l_Debye_TBose_update
 use electrons,      ONLY:levels,spin_occ
 use drivers,        ONLY:l_elph_scatt
 use vec_operate,    ONLY:v_norm
 use com,            ONLY:warning
 use D_lattice,      ONLY:Bose_Temp
 use R_lattice,      ONLY:RL_vol,bz_samp,qindx_S
 use IO_m,           ONLY:io_control,REP,OP_RD_CL
 use ELPH,           ONLY:ph_freqs_sq,elph_gkkp,ph_modes,elph_nb,W_debye,elph_branches
 use functions,      ONLY:bose_f
 use memory_m,       ONLY:mem_est
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_nk
 use vec_operate,    ONLY:degeneration_finder
 use parser_m,       ONLY:parser
 use R_lattice,      ONLY:RIM_n_rand_pts
 use parallel_m,     ONLY:PAR_Q_index,PAR_IND_Q,PAR_nQ,PAR_Xk_nibz,PAR_Xk_ibz_index,PAR_IND_Xk_ibz
 use functions,      ONLY:bose_f
 !
 implicit none
 !
 type(levels)             :: E
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 integer             ::iq,ID,io_err,ik,i_counter,alloc_err,iq_io,il,i_s,i_n,i_np,ik_bz,ik_mem,ik_m_q,&
&                      first_el(2,SC_bands(2)),n_of_el(2,SC_bands(2)),n_deg_grp(2),i1,i2,ik_store
 real(SP)            ::RADIUS_sphere,ph_Esq_local(q%nbz,ph_modes),q_mod(q%nbz),q0_mod,&
&                      q_weight_tmp(q%nbz,2),ph_mean_speed(3),ph_E
 complex(SP)         ::gkkp_average
 integer, external   ::ioELPH
 logical             ::l_use_Debye_E
 !
 ! Logicals
 !==========
 !
 if (.not.l_elph_scatt) return
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=ioELPH(ID,'gkkp_expanded')
 !
 if (io_err/=0) then
   l_elph_scatt=.FALSE.
   call warning('ELPH database not found. E-P scattering switched off')
   return
 endif
 !
 call parser('UseDebyeE'   , l_use_Debye_E)
 call parser('UpdtDebyeT', l_Debye_TBose_update)
 !
 ! Q/K grid remapping
 !====================
 !
 call RT_ELPH_gkkp_remap(k,q)
 !
 ! Allocation
 !============
 !
 allocate(elph_gkkp_expanded(PAR_Xk_nibz*PAR_nQ,ph_modes,elph_nb,elph_nb),stat=alloc_err)
 call mem_est("GKKP_expanded",(/size(elph_gkkp_expanded)/),(/2*SP/),errors=(/alloc_err/))
 !
 if (.not.l_RT_CCA_Kernel) then
   allocate(THETA_matrix(2*q%nbz*ph_modes,SC_bands(2)-SC_bands(1)+1,SC_bands(2)-SC_bands(1)+1,QP_nk))
   call mem_est("THETA_matrix",(/size(THETA_matrix)/),(/2*SP/))
   THETA_matrix=(0._SP,0._SP)
   allocate(THETA_matrix_index(q%nbz,ph_modes,2))
 endif
 !
 ! Degeneration Finder
 !====================
 !
 ! I/O
 !=====
 !
 i_counter=0
 !
 do iq=1,q%nbz
   !
   if (.not.PAR_IND_Q%element_1D(iq)) cycle
   !
   iq_io=Q_io_map(iq)
   !
   call io_control(ACTION=OP_RD_CL,SEC=(/iq_io+1/),ID=ID)
   io_err=ioELPH(ID,'gkkp_expanded')
   !
   ph_Esq_local(iq,:)=ph_freqs_sq(iq_io,:)
   !
   do ik=1,k%nibz
     !
     ik_bz    =sum(k%nstar(:ik-1))+1
     ik_m_q   =k%sstar(qindx_S(ik,iq,1),1)
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     call degeneration_finder(E%E(:SC_bands(2),ik,1),    SC_bands(2),first_el(1,:),n_of_el(1,:),n_deg_grp(1),0.0001/HA2EV)
     call degeneration_finder(E%E(:SC_bands(2),ik_m_q,1),SC_bands(2),first_el(2,:),n_of_el(2,:),n_deg_grp(2),0.0001/HA2EV)
     !
     ik_mem  =PAR_Xk_ibz_index(ik)
     ik_store=(PAR_Q_index(iq)-1)*PAR_Xk_nibz+ik_mem
     !
     do i_n=1,elph_nb
       do i_np=1,elph_nb
         !
         ! There has been a key error in the definition of the el-ph SE of the BKE (Eq. 30_9_11.18)
         ! where the band indexes have been exchanged. Instead of doing a global change of the notes
         ! and of the code I decided, here, to exchange the indexes:
         !
         elph_gkkp_expanded(ik_store,:,i_n,i_np)=elph_gkkp(K_io_map(ik_bz),:,i_np,i_n)
         !
       enddo
     enddo
     !
     ! Feb 2014. Here I symmetrize the gkkp m.e. with respect to denerated bands. This procedure is not
     ! needed in the case the RIM is not used. But when the RIM is used gkkp m.e. corresponding to transitions between degenerate
     ! states can be exported to non-degenerate bands belonging to a k of the small grid. In this way
     ! the detailled balance condition is not satisfied anymore.
     !
     do il=1,ph_modes
       do i1=1,n_deg_grp(1)
         do i_np=SC_bands(1),SC_bands(2)
           do i2=1,2
             if (i2==1) gkkp_average=0._SP
             do i_n=first_el(1,i1),first_el(1,i1)+n_of_el(1,i1)-1
               if (i2==1) gkkp_average=gkkp_average+abs(elph_gkkp_expanded(ik_store,il,i_n,i_np))**2.
               if (i2==2) elph_gkkp_expanded(ik_store,il,i_n,i_np)=sqrt(gkkp_average/real(n_of_el(1,i1)))
             enddo
           enddo
         enddo
       enddo
       do i1=1,n_deg_grp(2)
         do i_n=SC_bands(1),SC_bands(2)
           do i2=1,2
             if (i2==1) gkkp_average=0._SP
             do i_np=first_el(2,i1),first_el(2,i1)+n_of_el(2,i1)-1
               if (i2==1) gkkp_average=gkkp_average+abs(elph_gkkp_expanded(ik_store,il,i_n,i_np))**2.
               if (i2==2) elph_gkkp_expanded(ik_store,il,i_n,i_np)=sqrt(gkkp_average/real(n_of_el(2,i1)))
             enddo
           enddo
         enddo
       enddo
     enddo
     !
   enddo
   !
 enddo
 !
 ph_freqs_sq=ph_Esq_local
 !
 if(any(ph_freqs_sq<0.)) call warning('Imaginary phonon frequencies found. Please check.')
 !
 ! Test of the effect of using a single optical mode
 !
 if (l_use_Debye_E) ph_freqs_sq=W_debye**2.
 !
 ! RIM support
 !=============
 !
 ! Here It would be possible to have one different weight for each elph_mode.
 ! However in practice I assume there are two possibilities:
 ! 1 - acustic phonons w_ph(i) ~ \alpha(i)*|q|
 ! 2 - optical phonons w_ph(i) ~ const(i)
 !
 allocate(q_weight_RT(q%nbz,elph_branches(1):elph_branches(2),2))
 q0_mod=(3.*RL_vol/4./4./pi)**(1./3.)
 i1=0
 ph_mean_speed=0._SP
 q_mod=0._SP
 q_weight_RT=0._SP
 !
 if (RIM_n_rand_pts>0) then
   !
   call k_ibz2bz(q,'c',.TRUE.)  
   !
   do iq=1,q%nbz
     !
     q_mod(iq)=v_norm(q%ptbz(iq,:))
     !
     if (q_mod(iq)>q0_mod.or.iq==1) cycle
     !
     i1=i1+1
     !
     do il=1,3
       ph_mean_speed(il)=ph_mean_speed(il)+sqrt(ph_freqs_sq(iq,il))/q_mod(iq)
     enddo
     !
   enddo
   !
   ph_mean_speed=ph_mean_speed/float(i1)
   !
   call k_ibz2bz(q,'i',.TRUE.)  
   !
 endif
 !
 RADIUS_sphere=(3.*RL_vol/q%nbz/4./pi)**(1./3.)
 !
 call rim_spherical(q%nbz,q%ptbz,q_weight_tmp(:,1),RADIUS_sphere,1,.FALSE.)
 call rim_spherical(q%nbz,q%ptbz,q_weight_tmp(:,2),RADIUS_sphere,2,.FALSE.)
 !
 do iq=1,q%nbz
   !
   do il=elph_branches(1),elph_branches(2)
     !
     ph_E=sqrt(abs(ph_freqs_sq(iq,il)))
     !
     if (ph_E>0.) then
       q_weight_RT(iq,il,1)=1./ph_E/float(q%nbz)
       q_weight_RT(iq,il,2)=bose_f(ph_E)/ph_E/float(q%nbz)/spin_occ
     endif
     !
     if (RIM_n_rand_pts>0.and.q_mod(iq)<=q0_mod .and. il<=3) then
       q_weight_RT(iq,il,1)=q_weight_tmp(iq,1)/ph_mean_speed(il)
       q_weight_RT(iq,il,2)=Bose_temp*q_weight_tmp(iq,2)/ph_mean_speed(il)**2/spin_occ
     endif
     !
   enddo
   !
 enddo
 !
 ! THETA matrix indexes
 !======================
 !
 if (l_RT_CCA_Kernel) return
 !
 i_counter=0
 !
 do iq=1,q%nbz
   do il=1,ph_modes
     do i_s=1,2
       !
       i_counter=i_counter+1
       !
       THETA_matrix_index(iq,il,i_s)=i_counter
       !
     enddo
   enddo
 enddo
 !
end subroutine RT_ELPH_initialize
