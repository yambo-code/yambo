!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine Dipole_transverse(Xen,Xk,X)
 !
 ! This routine returns <iR> and <P^2>.
 !
 use pars,                ONLY:SP,schlen,cONE,CZERO
 use drivers,             ONLY:l_real_time
 use LIVE_t,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spin,n_spinor,n_sp_pol
 use parallel_m,          ONLY:PP_redux_wait,PAR_IND_CON_BANDS_X,PAR_IND_DIPk_ibz,&
&                              PAR_DIPk_ibz_index,PAR_IND_VAL_BANDS_X,&
&                              PAR_IND_CON_BANDS_X_ID,PAR_IND_VAL_BANDS_X_ID,PAR_IND_CON_BANDS_X_ID,&
&                              PAR_IND_Xk_ibz_ID,PAR_COM_q_for_Xo,PAR_COM_k_for_P,&
&                              PAR_IND_B_mat,PAR_IND_B_mat_ID,PAR_IND_WF_k
 use pseudo,              ONLY:PP_free,pp_table,Vnl,pp_n_l_comp,  &
&                              pp_kbv_dim,pp_factor
 use R_lattice,           ONLY:bz_samp
 use X_m,                 ONLY:X_t,DIP_iR,Dipole_bands_ordered,P_square,DIP_P,&
&                              Dipole_Energy_treshold,l_rotate_dipoles
 use IO_m,                ONLY:io_control,RD_CL,RD
 use memory_m,            ONLY:mem_est
 use wave_func,           ONLY:wf_ng,WF
 use com,                 ONLY:error,msg,warning
#if defined _KERR
 use X_m,                 ONLY:global_gauge
#endif
#if defined _SC
 use drivers,             ONLY:l_sc_run
 use SC,                  ONLY:B_mat_index,SC_R
#endif
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
 !
 ! Work Space
 !
 character(schlen)            :: live_t_string
 logical                      :: l_eval_DIP_P,l_eval_DIP_P2
 integer                      :: ik,i1,icfft(n_spinor),ivfft(n_spinor),ic,iv,iv_max,ic_min,&
&                                i_sp_pol,i_spinor,i_spin,alloc_err,wf_dim,n_LT_steps,ik_mem
 complex(SP), allocatable     :: kbv(:,:,:,:)
 complex(SP)                  :: rho(4),tmp(3)
 real(SP)                     :: Ev_m_Ec
 !
 !I/O and external functions
 !
 integer                      :: ID,io_err(3),io_KB_abinit_err,io_KB_pwscf_err,io_Vnl_err
 integer, external            :: io_KB_abinit,io_KB_pwscf,io_Vnl
 !
 call DIPOLE_kb_init(Xen,X,io_err,ID)
 !
 io_KB_abinit_err= io_err(1)
 io_KB_pwscf_err = io_err(2)
 io_Vnl_err     = io_err(3)
 !
 if(io_KB_abinit_err==0.or.io_KB_pwscf_err==0) then
   allocate(kbv(wf_ng,pp_kbv_dim,4,n_spinor),stat=alloc_err)
   call mem_est("KBV",(/size(kbv)/),errors=(/alloc_err/))
 endif
 !
 ! Set up band limits
 !
 if (Dipole_bands_ordered.or.Dipole_Energy_treshold<0.) then
   iv_max=Xen%nbm
   ic_min=Xen%nbf+1
 else
   iv_max=X%ib(2)
   ic_min=X%ib(1)
 endif
 !
#if defined _SC
 !
 if (l_rotate_dipoles) then
   if (.not.associated(Xen%E_dip)) call error('Bare dipoles energies not associated')
   if (associated(Xen%E_dip)) call msg('rns','[X] Using Kohn-Sham energies for the dipoles')
 else
   if (associated(Xen%Eo)   ) call msg('rns','[X] Using energies without QP corrections for the dipoles')
   if (associated(Xen%E_dip)) call warning(' Missing non-local Hamiltonian contributions different from Vnl')
 endif
 !
#else
 if (associated(Xen%Eo)   ) call msg('rns','[X] Using energies without QP corrections for the dipoles')
#endif
 !
 ! GPL_EXCLUDE_START
 !
#if !defined _YPP_RT
 !
 ! GPL_EXCLUDE_END
 !
 if (Dipole_bands_ordered) then
   n_LT_steps=PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)*&
&             PAR_IND_CON_BANDS_X(X%whoami)%n_of_elements(PAR_IND_CON_BANDS_X_ID(X%whoami)+1)*&
&             PAR_IND_VAL_BANDS_X(X%whoami)%n_of_elements(PAR_IND_VAL_BANDS_X_ID(X%whoami)+1)*n_sp_pol
 else
   n_LT_steps=PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)*&
&             PAR_IND_B_mat%n_of_elements(PAR_IND_B_mat_ID+1)*n_sp_pol
 endif
 !
 ! GPL_EXCLUDE_START
 !
#else
 n_LT_steps=Xk%nibz*(X%ib(2)-X%ib(1)+1)**2._SP*n_sp_pol
#endif
 !
 ! GPL_EXCLUDE_END
 !
 !
 wf_dim=3
 !
 if (n_LT_steps>0) then
   !
   live_t_string='Dipole (T):'
   l_eval_DIP_P =.false.
   l_eval_DIP_P2=.false.
   !
#if defined _SC
   l_eval_DIP_P =l_sc_run.or.l_real_time
   l_eval_DIP_P2=l_sc_run.or.l_real_time
   if(l_eval_DIP_P) then
     live_t_string='P and P^2 (T):'
     wf_dim=4
   endif
#endif
   !
#if defined _KERR
   l_eval_DIP_P=trim(global_gauge)=='velocity'
   if(l_eval_DIP_P) live_t_string='P (T):'
#endif
   !
   call live_timing(trim(live_t_string),n_LT_steps)
   !
 endif
 !
#if !defined _YPP_RT
 call PP_redux_wait(COMM=PAR_COM_q_for_Xo%COMM)
#endif
 !
 ! Main loop over k in IBZ
 !
 do ik=1,Xk%nibz
   !
   if (n_LT_steps==0) cycle
   ! 
   ! [Vnl,r]
   !
   if (io_KB_pwscf_err==0) then
     if (ik<Xk%nibz  ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz ) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     io_KB_pwscf_err=io_KB_pwscf(ID)
   endif
   !
   if (io_KB_abinit_err==0) then
     if (ik<Xk%nibz  ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz ) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     io_KB_abinit_err=io_KB_abinit(ID)
   endif
   ! 
#if !defined _YPP_RT
   if (Dipole_bands_ordered) then
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
   else
     if (.not.PAR_IND_WF_k%element_1D(ik)) cycle 
   endif
#endif
   !
   ik_mem=PAR_DIPk_ibz_index(ik)
   ! 
   do i_sp_pol=1,n_sp_pol
     ! 
     if (io_KB_abinit_err==0) call DIPOLE_kb_abinit_comp(ik,i_sp_pol,Xk,kbv)
     if (io_KB_pwscf_err ==0) call DIPOLE_kb_pwscf_comp(ik,Xk,kbv)
     !
     do ic=ic_min,X%ib(2)
       !
       if (Dipole_bands_ordered) then
         if (.not.PAR_IND_CON_BANDS_X(X%whoami)%element_1D(ic)) cycle 
       endif
       !
       do iv=X%ib(1),iv_max 
         !
         if (Dipole_bands_ordered) then
           if (.not.PAR_IND_VAL_BANDS_X(X%whoami)%element_1D(iv)) cycle 
         else
#if defined _SC && !defined _YPP_RT
           if (.not.PAR_IND_B_mat%element_1D( B_mat_index(ic,iv))) cycle
#endif
         endif
         !
         do i_spinor=1,n_spinor
           !
           i_spin=i_sp_pol+i_spinor-1
           ivfft(i_spin)=WF%index(iv,ik,i_spin)
           icfft(i_spin)=WF%index(ic,ik,i_spin)
           !
           if (ivfft(i_spin)==0) call error(" Error in parallel wf distribution (Dipole iv)")
           if (icfft(i_spin)==0) call error(" Error in parallel wf distribution (Dipole ic)")
           !
         enddo
         !
         if(l_rotate_dipoles) then
           Ev_m_Ec=Xen%E_dip(iv,ik,i_sp_pol)-Xen%E_dip(ic,ik,i_sp_pol)
         else
           Ev_m_Ec=Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)
           if (associated(Xen%Eo) ) Ev_m_Ec=Xen%Eo(iv,ik,i_sp_pol)-Xen%Eo(ic,ik,i_sp_pol)
         endif
         !
         if (any((/-Ev_m_Ec<X%ehe(1).and.X%ehe(1)>0.0_SP,-Ev_m_Ec>X%ehe(2).and.X%ehe(2)>0.0_SP/)).or.&
&            abs(Ev_m_Ec)<=Dipole_Energy_treshold.and..not.l_real_time) then
           !
           call live_timing(steps=1)
           cycle
           !
         endif
         !
         ! Evaluate <ivfft|p|icfft>
         call DIPOLE_p_matrix_elements(Xk,ivfft,icfft,ik,wf_dim,rho)
         !
         ! This is outside the nspinor loop because the kb operator can be non diagonal
         ! in spin space if n_spinor=2
         if (io_Vnl_err==0) rho(1:3) = rho(1:3) + Vnl(1:3,ic,iv,ik,i_sp_pol)*0.5*real(n_spinor,SP)
         if (io_KB_abinit_err==0) call DIPOLE_kb_sum(ivfft,icfft,rho(:3),kbv,1)
         if (io_KB_pwscf_err ==0) call DIPOLE_kb_sum(ivfft,icfft,rho(:3),kbv,2)
         !
#if defined _SC || defined _KERR
         !
         !=====
         ! <P>
         !=====
         ! P_vc=rho(:3)
         !
         if (l_eval_DIP_P) DIP_P(:,ic,iv,ik_mem,i_sp_pol)=rho(:3)
#endif
         !
         !======
         ! <iR>
         !======
         ! as [x,p_x]=i we get [x,H] = [x,p^2/2]= i p_x. So from rho (that is P_vc) we can extract
         !
         ! DIP_iR(c,v) = i <v|r|c> = i <v|[r,H]|c>/(Ec-Ev) = i <v|i p|c>/(Ec-Ev) =
         !             = - <v | -i grad |c> / (Ec-Ev) = <v|-i grad|c>/(Ev-Ec) = P_vc/(Ev-Ec)
         !
         rho(1:3)=rho(1:3)/Ev_m_Ec
         !
         if (abs(Ev_m_Ec)<=Dipole_Energy_treshold) rho(1:3)=cZERO
         !
         DIP_iR(:,ic,iv,ik_mem,i_sp_pol)=rho(:3)
         !
#if defined _SC
         !
         !======
         ! <P^2>
         !======
         !
         if (l_eval_DIP_P2) P_square(ic,iv,ik_mem,i_sp_pol)= rho(4)
#endif
         !
         call live_timing(steps=1)
         !
       enddo    ! conduction band loop
       !
     enddo      ! valence band loop
     ! 
   enddo        ! sp_pol loop
   ! 
 enddo          ! k-points
 !
 call live_timing()
 !
 ! CLEAN
 !
 if (io_KB_pwscf_err==0.or.io_KB_abinit_err==0) then
   if (allocated(kbv))          deallocate(kbv)
   if (allocated(pp_n_l_comp))  deallocate(pp_n_l_comp)
   if (allocated(pp_table))     deallocate(pp_table)
   if (allocated(pp_factor))    deallocate(pp_factor)
   call PP_free()
   call mem_est("KBV") 
 else if (io_Vnl_err==0) then
   if (allocated(Vnl)) deallocate(Vnl)
 endif 
 !
 do ik=1,Xk%nibz
   !
   if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
   !
   ik_mem=PAR_DIPk_ibz_index(ik)
   ! 
   do i_sp_pol=1,n_sp_pol
#if !defined _YPP_RT
     call PP_redux_wait(DIP_iR(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_k_for_P%COMM)
#endif
#if defined _SC || defined _KERR || !defined _YPP_RT
     if (l_eval_DIP_P ) call PP_redux_wait(DIP_P(:,:,:,ik_mem,i_sp_pol),COMM=PAR_COM_k_for_P%COMM)
     if (l_eval_DIP_P2) call PP_redux_wait(P_square(:,:,ik_mem,i_sp_pol),COMM=PAR_COM_k_for_P%COMM)
#endif
   enddo
   !
   ! Symmetrization 
   !================
   !
   ! To avoid numerical noise in the real-time evolution we need to impose
   ! P (iR) to be Hermitian (anti-Hermitian)
   !
   if (.not.Dipole_bands_ordered) then
     do iv=X%ib(1),X%ib(2)
       do ic=iv+1,X%ib(2)
         do i_sp_pol=1,n_sp_pol
           tmp=( DIP_iR(:,ic,iv,ik_mem,i_sp_pol)-conjg(DIP_iR(:,iv,ic,ik_mem,i_sp_pol)) )/2._SP
           DIP_iR(:,ic,iv,ik_mem,i_sp_pol) =       tmp
           DIP_iR(:,iv,ic,ik_mem,i_sp_pol) =-conjg(tmp)
#if defined _SC || defined _KERR
           if (l_eval_DIP_P ) then
             tmp=( DIP_P(:,ic,iv,ik_mem,i_sp_pol) + conjg(DIP_P(:,iv,ic,ik_mem,i_sp_pol)) )/2._SP
             DIP_P(:,ic,iv,ik_mem,i_sp_pol) =       tmp
             DIP_P(:,iv,ic,ik_mem,i_sp_pol) = conjg(tmp)
           endif
           if (l_eval_DIP_P2) then
             tmp(1)=( P_square(ic,iv,ik_mem,i_sp_pol) + conjg(P_square(iv,ic,ik_mem,i_sp_pol)) )/2._SP 
             P_square(ic,iv,ik_mem,i_sp_pol) =       tmp(1)
             P_square(iv,ic,ik_mem,i_sp_pol) = conjg(tmp(1))
           endif
#endif
         enddo
       enddo
     enddo
   endif
   !
 enddo
 !
#if defined _SC
 if(.not.l_sc_run.and.l_rotate_dipoles) call DIPOLE_rotate(SC_R,Xen,Xk,X)
#endif
 !
end subroutine Dipole_transverse
