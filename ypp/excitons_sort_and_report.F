!
! Copyright (C) 2000-2010 D. Varsano, A. Marini and the YAMBO team
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
subroutine excitons_sort_and_report(BS_H_dim,Xk,E,BS_R,BS_E,BS_E_degs,&
&                                   A_weight,S_z,S_sq,verbose)
 !
 use pars,          ONLY:SP,schlen,pi
 use units,         ONLY:HA2EV
 use electrons,     ONLY:spin_occ,levels,spin,n_sp_pol
 use stderr,        ONLY:intc
 use BS,            ONLY:BSS_description,BSS_n_descs,BS_eh_table,BS_bands
 use com,           ONLY:msg,of_open_close
 use YPP,           ONLY:deg_energy,lambda,min_weight
 use R_lattice,     ONLY:d3k_factor,q0_def_norm,bz_samp
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species,atom_pos,Z_species
 use vec_operate,   ONLY:sort
 use QP_CTL_m,      ONLY:QP_apply
 !
 implicit none 
 !
 integer            ::BS_H_dim
 complex(SP)        ::BS_R(BS_H_dim),BS_E(BS_H_dim)
 type(bz_samp)      ::Xk
 type(levels)       ::E
 real(SP),optional  ::A_weight(BS_H_dim)
 real(SP),optional  ::S_z(BS_H_dim)
 real(SP),optional  ::S_sq(BS_H_dim)
 integer, optional  ::BS_E_degs(BS_H_dim)
 logical, optional  ::verbose
 !
 ! Work Space
 !
 integer               ::j1,j2,ia,is,i_mode,ikbz,ikibz,iv,ic,neh,i_spin
 integer,  allocatable ::S_indx(:)
 real(SP), allocatable ::R(:),v2sort(:)
 real(SP)              ::Rmax,rv(10),K_weight(Xk%nibz),value
 character(schlen) ::titles(7),ch_dummy(2)
 logical           ::write_widths,write_spin,verbose_
 !
 ! Excitonc amplitude
 !
 character(3), parameter:: R_normalize="yes"
 integer,      parameter:: amp_steps=1000
 integer      :: amp_n_trans
 real(SP)     :: amp_range(2),amp_damping,amp_I(amp_steps)
 real(SP),allocatable :: amp_trans(:,:)
 complex(SP)          :: amp_E(amp_steps) 
 !
 verbose_=.TRUE.
 if (present(verbose)) verbose_=verbose
 !
 if (present(BS_E_degs)) then  
   !
   if (verbose_) call msg('s',':: Sorting energies')
   !==================================
   !
   allocate(v2sort(BS_H_dim),S_indx(BS_H_dim))
   !
   v2sort=real(BS_E)
   call sort(arrin=v2sort,indx=S_indx)
   !
   BS_E_degs=0
   !
   do j1=1,BS_H_dim
     !
     if (BS_E_degs(S_indx(j1))>0) cycle
     !
     BS_E_degs(S_indx(j1))=S_indx(j1)
     !
     do j2=j1+1,BS_H_dim
       if ( abs( real(BS_E(S_indx(j1)))-real(BS_E(S_indx(j2))) )>deg_energy) exit
       BS_E_degs(S_indx(j2))=S_indx(j1)
     enddo
     !
   enddo
   !
   deallocate(v2sort,S_indx)
   !
   return
   !
 endif
 !
 if (present(A_weight)) then  
   !
   if (verbose_) call section('=','Reporting amplitude and weights')
   !==================================================
   !
   allocate(S_indx(BS_H_dim),amp_trans(BS_H_dim,2))
   !
   ! Sort the weights
   !
   call sort(arrin=A_weight,indx=S_indx)
   !
   ! Apply quasi-particle correction if presents
   ! 
   call QP_apply(BS_bands,E,Xk,'G',msg_fmt='s')
   !
   ! report on file the weights and the amplitude
   ! of the excitonic state...
   ! 
   ! ... first open the file.
   ! 
   ch_dummy(1)='exc_weights_at_'//trim(intc(lambda))
   ch_dummy(2)='exc_amplitude_at_'//trim(intc(lambda))
   call of_open_close(ch_dummy(1),'ot')
   call of_open_close(ch_dummy(2),'ot')
   !
   do j1=1,BSS_n_descs
     call msg('o weight amp',"#",trim(BSS_description(j1)),INDENT=0)
   enddo
   call msg('o weight amp',"#")
   call msg('o weight amp',&
&   '# Electron-Hole pairs that contributes to Excitonic State '//&
&   trim(intc(lambda))//' more than 5%')
   call msg('o weight amp','#')
   !
   ! First summarize the total weight vs K-points
   !
   K_weight=0.
   do neh = 1,BS_H_dim
     ikbz  = BS_eh_table(S_indx(neh),1)
     ikibz = Xk%sstar(ikbz,1)
     K_weight(ikibz)=K_weight(ikibz)+A_weight(neh)
   enddo
   K_weight=K_weight/maxval(K_weight)
   titles(1:4)=(/'             ','K-point [iku]','             ','Weight       '/)
   call msg('o weight','#',titles(:4),INDENT=0,USE_TABS=.true.)
   do ikibz=1,Xk%nibz
     if (K_weight(ikibz)>min_weight*5._SP) then
       call msg('o weight','# ',(/Xk%pt(ikibz,:),K_weight(ikibz)/),INDENT=0,USE_TABS=.true.)
     endif
   enddo
   !
   ! Then report the detailed list of transitions & calculate the amplitude
   !
   call msg('o weight','#','',INDENT=0,USE_TABS=.true.)
   titles(1:4)=(/'Band_V','Band_C','K  ibz','Symm. '/)
   ! GPL_EXCLUDE_START SPIN_IGNORE
   if(n_sp_pol==1) then
     ! GPL_EXCLUDE_END SPIN_IGNORE
     titles(5:6)=(/'Weight','Energy'/)
     call msg('o weight','#',titles(:6),INDENT=0,USE_TABS=.true.)
     ! GPL_EXCLUDE_START SPIN_IGNORE
   else
     titles(5:7)=(/'Spin  ','Weight','Energy'/)
     call msg('o weight','#',titles(:7),INDENT=0,USE_TABS=.true.)
   endif
   ! GPL_EXCLUDE_END SPIN_IGNORE
   titles(1:2)=(/'E    [eV]','Amplitude'/)
   call msg('o amp','#',titles(:2),INDENT=0,USE_TABS=.true.)
   call msg('o weight amp','#','',INDENT=0,USE_TABS=.true.)
   !
   amp_n_trans=0
   amp_trans=0.
   !
   do neh = BS_H_dim,1,-1
     !
     if (A_weight(neh)/maxval(A_weight)<min_weight) cycle
     !
     ikbz = BS_eh_table(S_indx(neh),1)
     iv = BS_eh_table(S_indx(neh),2)
     ic = BS_eh_table(S_indx(neh),3)
     ! GPL_EXCLUDE_START SPIN_IGNORE
     !
     i_spin= spin(BS_eh_table(S_indx(neh),:))
     !
     ! GPL_EXCLUDE_END SPIN_IGNORE
     ! GPL_INCLUDE_START SPIN_IGNORE
     !i_spin = 1
     ! GPL_INCLUDE_END SPIN_IGNORE
     !
     ikibz = Xk%sstar(ikbz,1)
     is = Xk%sstar(ikbz,2)
     !
     amp_n_trans=amp_n_trans+1
     amp_trans(amp_n_trans,1)=E%E(ic,ikibz,i_spin)-E%E(iv,ikibz,i_spin)
     amp_trans(amp_n_trans,2)=A_weight(neh)
     !
     if (A_weight(neh)<min_weight*5._SP) cycle
     !
     ! GPL_EXCLUDE_START SPIN_IGNORE
     if(n_sp_pol==1) then
       ! GPL_EXCLUDE_END SPIN_IGNORE
       call msg('o weight','',(/real(iv,SP),real(ic,SP),real(ikibz,SP),real(is,SP),&
&                             A_weight(neh),amp_trans(amp_n_trans,1)*HA2EV/),&
&            INDENT=-2,USE_TABS=.FALSE.)
       ! GPL_EXCLUDE_START SPIN_IGNORE
     else
       if(i_spin==1) value= 1.
       if(i_spin==2) value=-1.
       call msg('o weight','',(/real(iv,SP),real(ic,SP),real(ikibz,SP),real(is,SP),&
&                           value,A_weight(neh),amp_trans(amp_n_trans,1)*HA2EV/),&
&                 INDENT=-2,USE_TABS=.FALSE.)
     endif
     ! GPL_EXCLUDE_END SPIN_IGNORE
   enddo
   !
   ! Excitonic Amplitude
   !
   amp_range=(/minval(amp_trans(:amp_n_trans,1))-0.5/HA2EV,&
&              maxval(amp_trans(:amp_n_trans,1))+0.5/HA2EV/)
   amp_damping=(amp_range(2)-amp_range(1))/100. 
   amp_I=0.
   do j2=1,amp_steps
     amp_E(j2)=amp_range(1)+(j2-1)*(amp_range(2)-amp_range(1))/real(amp_steps,SP)+&
&              (0.,1.)*amp_damping
     do j1=1,amp_n_trans
       amp_I(j2)=amp_I(j2)+amp_damping/pi*amp_trans(j1,2)/( (real(amp_E(j2),SP)-amp_trans(j1,1))**2.+amp_damping**2.)
     enddo
   enddo
   amp_I=amp_I/maxval(amp_I)
   do j1=1,amp_steps
     call msg('o amp','',(/real(amp_E(j1),SP)*HA2EV,amp_I(j1)/),INDENT=-2,USE_TABS=.FALSE.)
   enddo
   !
   call of_open_close(ch_dummy(1))
   call of_open_close(ch_dummy(2))
   !
   deallocate(S_indx,amp_trans)
   !
   return
   !
 endif
 !
 write_spin=present(S_sq)
 write_widths=any(abs(aimag(BS_E))>1.E-5)
 !
 if (write_spin) then
   if (verbose_) call section('=','Reporting sorted Energies, Strengths and Spins')
 else if (write_widths) then
   if (verbose_) call section('=','Reporting sorted Energies, Strengths and widths')
 else
   if (verbose_) call section('=','Reporting sorted Energies and Strengths')
 endif
 !==========================================================
 !
 allocate(R(BS_H_dim),S_indx(BS_H_dim),v2sort(BS_H_dim))
 !
 do i_mode=1,2
   !
   if (i_mode==1) ch_dummy(1)='exc_E_sorted'
   if (i_mode==2) ch_dummy(1)='exc_I_sorted'
   if (write_spin) then
     if (i_mode==1) ch_dummy(1)='exc_E+spin_sorted'
     if (i_mode==2) ch_dummy(1)='exc_I+spin_sorted'
   endif
   !
   call of_open_close(ch_dummy(1),'ot')
   !
   do j1=1,BSS_n_descs
     call msg('o sort',"#",trim(BSS_description(j1)),INDENT=0)
   enddo
   call msg('o sort',"#")
   do is=1,n_atomic_species
     do ia=1,n_atoms_species(is)
       call msg('o sort',"# Atom "//trim(intc(ia))//&
&                        " with Z "//trim(intc(Z_species(is)))//" [cc]:",&
&               atom_pos(:,ia,is),INDENT=0)
     enddo
   enddo
   call msg('o sort','#','',INDENT=0) 
   !
   titles(1)='E [ev]'
   titles(2)='Strength'
   titles(3)='Index'
   if (write_widths) then
     titles(4)='W [meV]'
     call msg('o sort','#',titles(:4),INDENT=0,USE_TABS=.true.)
   else if (write_spin) then
     titles(4)='S_square'
     titles(5)='S_z'
     call msg('o sort','#',titles(:5),INDENT=0,USE_TABS=.true.)
   else
     call msg('o sort','#',titles(:3),INDENT=0,USE_TABS=.true.)
   endif
   call msg('o sort','#','',INDENT=0)
   !
   R(:) = BS_R(:)*conjg(BS_R(:))
   if (write_widths)  R(:) = abs(BS_R(:))
   R(:) = R(:)*real(spin_occ,SP)/(2.*pi)**3.*&
&         d3k_factor*4.*pi/q0_def_norm**2*HA2EV
   Rmax = maxval(R)
   !
   if (trim(R_normalize)=='yes') R=R/Rmax
   !
   if (i_mode==1) v2sort=real(BS_E,SP)
   if (i_mode==2) v2sort=R
   !
   call sort(arrin=v2sort,indx=S_indx)
   !
   do j1=BS_H_dim,1,-1
     !
     if (i_mode == 1) j2=S_indx(BS_H_dim-j1+1)
     if (i_mode == 2) j2=S_indx(j1)
     !
     rv(:3)=(/real(BS_E(j2),SP)*HA2EV,R(j2),real(j2,SP)/)
     !
     if (write_widths) then
       rv(4)=abs(aimag(BS_E(j2)))*HA2EV*1000.
       call msg('o sort','',rv(:4),INDENT=-2,USE_TABS=.TRUE.)
     else if (write_spin) then
       rv(4:5)=(/S_z(j2),S_sq(j2)/)
       call msg('o sort','',rv(:5),INDENT=-2,USE_TABS=.TRUE.)
     else
       call msg('o sort','',rv(:3),INDENT=-2,USE_TABS=.TRUE.)
     endif
     !
   enddo
   !
   call of_open_close(ch_dummy(1))
   !
 enddo
 ! 
 deallocate(v2sort,S_indx,R)
 !
end subroutine
