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
 use pars,                ONLY:SP,pi,schlen
 use drivers,             ONLY:l_real_time
 use LIVE_t,              ONLY:live_timing
 use electrons,           ONLY:levels,n_spin, n_spinor, n_sp_pol
 use parallel_m,          ONLY:PP_redux_wait,PAR_IND_CON_BANDS_X,PAR_IND_DIPk_ibz,&
&                              PAR_DIPk_ibz_index,PAR_IND_VAL_BANDS_X,&
&                              PAR_IND_CON_BANDS_X_ID,PAR_IND_VAL_BANDS_X_ID,PAR_IND_CON_BANDS_X_ID,&
&                              PAR_IND_Xk_ibz_ID,PAR_COM_q_for_Xo,PAR_COM_k_for_P,&
&                              PAR_IND_B_mat,PAR_IND_B_mat_ID,PAR_IND_WF_k
 use D_lattice,           ONLY:alat
 use pseudo,              ONLY:PP_free,pp_table,Vnl,pp_n_l_comp,PP_free,  &
&                              pp_kbv_dim,spin_m_table,pp_factor
 use R_lattice,           ONLY:g_vec,bz_samp
 use X_m,                 ONLY:X_t,DIP_iR,Dipole_bands_ordered,P_square,DIP_P,Dipole_Energy_treshold
 use IO_m,                ONLY:io_control,RD_CL,RD
 use memory_m,            ONLY:mem_est
 use wave_func,           ONLY:wf_ng,WF
 use wrapper,             ONLY:M_by_V
 use com,                 ONLY:error,msg
#if defined _KERR
 use X_m,                 ONLY:global_gauge
#endif
 !
 implicit none
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
 !
 ! Work Space
 !
 character(schlen)            :: live_t_string
 logical                      :: l_eval_DIP_P,l_eval_DIP_P2
 integer                      :: ik,i1,icfft,ivfft,ic,iv,iv_max,ic_min,i_spin,E_i_spin,pp_spin,&
&                                alloc_err,wf_dim,n_LT_steps,ik_mem
#if defined _OPENMP
 integer                      :: ig
#endif
 complex(SP), allocatable     :: wf_xyz(:,:),kbv(:,:,:)
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
   allocate(kbv(wf_ng,pp_kbv_dim,4),stat=alloc_err)
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
 if (associated(Xen%Eo)) call msg('rns','[X] Using Kohn-Sham energies for the dipoles ')
 !
 !
 if (Dipole_bands_ordered) then
   n_LT_steps=PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)*&
&             PAR_IND_CON_BANDS_X(X%whoami)%n_of_elements(PAR_IND_CON_BANDS_X_ID(X%whoami)+1)*&
&             PAR_IND_VAL_BANDS_X(X%whoami)%n_of_elements(PAR_IND_VAL_BANDS_X_ID(X%whoami)+1)*n_spin
 else
   n_LT_steps=PAR_IND_DIPk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)*&
&             PAR_IND_B_mat%n_of_elements(PAR_IND_B_mat_ID+1)*n_spin
 endif
 !
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
 call PP_redux_wait(COMM=PAR_COM_q_for_Xo%COMM)
 !
 allocate(wf_xyz(wf_ng,wf_dim))
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
   if (Dipole_bands_ordered) then
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle 
   else
     if (.not.PAR_IND_WF_k%element_1D(ik)) cycle 
   endif
   !
   ik_mem=PAR_DIPk_ibz_index(ik)
   ! 
   do i_spin=1,n_spin
     ! 
     pp_spin=i_spin
     if (io_KB_abinit_err==0.and.n_spinor==2) pp_spin=1
     if (io_KB_pwscf_err ==0.and.n_sp_pol==2) pp_spin=1
     if (io_KB_abinit_err==0) call DIPOLE_kb_abinit_comp(ik,pp_spin,Xk,kbv)
     if (io_KB_pwscf_err ==0) call DIPOLE_kb_pwscf_comp(ik,pp_spin,Xk,kbv)
     !
     E_i_spin=i_spin
     if (n_spinor==2) E_i_spin=1
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
         endif
         !
         rho(:)=(0._SP,0._SP)
         !
         Ev_m_Ec=Xen%E(iv,ik,E_i_spin)-Xen%E(ic,ik,E_i_spin)
         if (associated(Xen%Eo)) Ev_m_Ec=Xen%Eo(iv,ik,E_i_spin)-Xen%Eo(ic,ik,E_i_spin)
         !
         if (any((/-Ev_m_Ec<X%ehe(1).and.X%ehe(1)>0.0_SP,-Ev_m_Ec>X%ehe(2).and.X%ehe(2)>0.0_SP/)).or.&
&            abs(Ev_m_Ec)<=Dipole_Energy_treshold.and..not.l_real_time) then
           !
           call live_timing(steps=1)
           cycle
           !
         endif
         !
         ivfft=WF%index(iv,ik,i_spin)
         if (ivfft==0) call error(" Error in parallel wf distribution (Dipole iv)")
         !
         ! wf_xyz = -i grad fdo(iv,ik)
         !
#if defined _OPENMP
!$omp parallel default(shared), private(i1,ig)
         do i1=1,3
!$omp do
           do ig=1,wf_ng
             wf_xyz(ig,i1)=WF%c(ig,ivfft)*(Xk%pt(ik,i1)+g_vec(ig,i1))*2.0_SP*pi/alat(i1)
           enddo
!$omp end do
         enddo
!$omp end parallel
#else
         do i1=1,3
           wf_xyz(:,i1)=WF%c(:,ivfft)*(Xk%pt(ik,i1)+g_vec(:wf_ng,i1))*2.0_SP*pi/alat(i1)
         enddo
#endif
         ! 
         !
         icfft=WF%index(ic,ik,i_spin)
         if (icfft==0) call error(" Error in parallel wf distribution (Dipole ic)")
         !
         ! WARNING: 
         ! ========
         ! The convention is to revert the order of the bands
         ! indexes in the definition of DIP_P and DIP_iR :
         ! DIP_P(iv,ic)  = P_cv ; DIP_iR(iv,ic) = (iR)_cv 
         !
         ! Note that P is Hermitian and iR anti-hermitian.
         !
         ! rho = <v|p|c> = P_vc =
         !     = wf_xyz^* fdo(ic,ik)= - i < fdo(iv,ik)| grad |  fdo(ic,ik) > =
         !       \sum_G -i u^*_{iv ik}(G) i ( k + G ) u_{ic ik}(G)
         !     = \sum_G u^*_{iv ik}(G) ( k + G ) u_{ic ik}(G)
         !
         ! rho(4) =  \sum_G u^*_{iv ik}(G) ( k + G )^2 u_{ic ik}(G)
         !
         call M_by_V('C',wf_ng,wf_dim,(1._SP,0._SP),wf_xyz,wf_ng,WF%c(:,icfft),1,(0._SP,0._SP),rho,1)
         !
         if (io_KB_abinit_err==0.or.io_KB_pwscf_err==0) call DIPOLE_kb_sum(ivfft,icfft,rho(:3),kbv)
         if (io_Vnl_err==0) then
           if(n_spinor==1) rho(1:3) = rho(1:3) + Vnl(1:3,ic,iv,ik,i_spin)*0.5
           if(n_spinor==2.and.i_spin==2) rho(1:3) = rho(1:3) + Vnl(1:3,ic,iv,ik,1)*0.5
         endif
         !
         !=====
         ! <P>
         !=====
         ! P_vc=rho(:3)
         !
#if defined _SC || defined _KERR
         if (l_eval_DIP_P) then
           if (n_spinor==2) then
             DIP_P(:,ic,iv,ik_mem,1)=DIP_P(:,ic,iv,ik_mem,1)+rho(:3)
           else
             DIP_P(:,ic,iv,ik_mem,i_spin)=rho(:3)
           endif
         endif
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
         if (abs(Ev_m_Ec)<=Dipole_Energy_treshold) rho(1:3)=(0._SP,0._SP)
         !
         if (n_spinor==2) then
           DIP_iR(:,ic,iv,ik_mem,1)=DIP_iR(:,ic,iv,ik_mem,1)+rho(:3)
         else 
           DIP_iR(:,ic,iv,ik_mem,i_spin)=rho(:3)
         endif
         !
         !
         call live_timing(steps=1)
         !
       enddo    ! conduction band loop
       !
     enddo      ! valence band loop
     ! 
   enddo        ! spin loop
   ! 
 enddo          ! k-points
 !
 call live_timing()
 !
 ! CLEAN
 !
 deallocate(wf_xyz)
 !
 if (io_KB_pwscf_err==0.or.io_KB_abinit_err==0) then
   if (allocated(kbv))          deallocate(kbv)
   if (allocated(pp_n_l_comp))  deallocate(pp_n_l_comp)
   if (allocated(pp_table))     deallocate(pp_table)
   if (allocated(pp_factor))    deallocate(pp_factor)
   if (allocated(spin_m_table)) deallocate(spin_m_table)
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
   do i_spin=1,n_sp_pol
     call PP_redux_wait(DIP_iR(:,:,:,ik_mem,i_spin),COMM=PAR_COM_k_for_P%COMM)
#if defined _SC || defined _KERR || !defined _YPP_RT
     if (l_eval_DIP_P ) call PP_redux_wait(DIP_P(:,:,:,ik_mem,i_spin),COMM=PAR_COM_k_for_P%COMM)
     if (l_eval_DIP_P2) call PP_redux_wait(P_square(:,:,ik_mem,i_spin),COMM=PAR_COM_k_for_P%COMM)
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
         do i_spin=1,n_sp_pol
           tmp=( DIP_iR(:,ic,iv,ik_mem,i_spin)-conjg(DIP_iR(:,iv,ic,ik_mem,i_spin)) )/2._SP
           DIP_iR(:,ic,iv,ik_mem,i_spin) =       tmp
           DIP_iR(:,iv,ic,ik_mem,i_spin) =-conjg(tmp)
#if defined _SC || defined _KERR
           if (l_eval_DIP_P ) then
             tmp=( DIP_P(:,ic,iv,ik_mem,i_spin) + conjg(DIP_P(:,iv,ic,ik_mem,i_spin)) )/2._SP
             DIP_P(:,ic,iv,ik_mem,i_spin) =       tmp
             DIP_P(:,iv,ic,ik_mem,i_spin) = conjg(tmp)
           endif
           if (l_eval_DIP_P2) then
             tmp(1)=( P_square(ic,iv,ik_mem,i_spin) + conjg(P_square(iv,ic,ik_mem,i_spin)) )/2._SP 
             P_square(ic,iv,ik_mem,i_spin) =       tmp(1)
             P_square(iv,ic,ik_mem,i_spin) = conjg(tmp(1))
           endif
#endif
         enddo
       enddo
     enddo
   endif
   !
 enddo
 !
end subroutine Dipole_transverse
