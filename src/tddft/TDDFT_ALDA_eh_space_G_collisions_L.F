!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
! headers
!
#include<dev_defs.h>
!
!
subroutine TDDFT_ALDA_eh_space_G_collisions_L(iq,Xk,i_T_grp,NG,l_bs_tddft_wf_in_loop,tddft_wf,mode)
 !
 ! Calculates the F_xc scattering 
 !
 !  (ic(1),ik(1),is(1)) --<--:...:--<-- (ic(2),ik(2),is(2))
 !                       :sqrt(Fxc):
 !
 use pars,           ONLY:SP,DP,pi,cZERO
 use FFT_m,          ONLY:fft_size
 use wave_func,      ONLY:WF
 use interfaces,     ONLY:WF_load,WF_free
 use R_lattice,      ONLY:qindx_X,bz_samp,minus_G
 use BS,             ONLY:l_BS_magnons,BS_T_grp,BS_bands
 use electrons,      ONLY:n_spinor,n_sp_pol,n_spin
 use timing_m,       ONLY:timing
 use openmp,         ONLY:OPENMP_update,master_thread
 use interfaces,     ONLY:WF_apply_symm
 use TDDFT,          ONLY:tddft_wf_t
 !
 implicit none
 !
 type(bz_samp),intent(in) :: Xk
 integer,      intent(in) :: iq,i_T_grp,NG(2)
 logical,      intent(in) :: l_bs_tddft_wf_in_loop
 character(1), intent(in) :: mode
 type(tddft_wf_t), target, intent(inout) :: tddft_wf 
 ! 
 ! Work Space
 !
 logical     :: l_load_WFs
 integer     :: i_spinor,j_spinor,ip_spinor,jp_spinor,ifft,irhotw,is_yambo,os_yambo,i_spinor_y,j_spinor_y
 integer     :: i_T_el,i_T_el_p,N_T_el_p,i_c,i_v,i_sp_c,i_sp_v,i_k_bz,i_k,i_s,i_g0,i_g1,i_g2,i_p_bz,i_g_p,i_p,&
&               i_sp,i_T_grp_p,is(4),os(4),qs(3),NK(2)
 !
 complex(DP), pointer DEV_ATTR :: rhotwr_p(:)
 complex(SP), pointer :: WF_symm1_p(:,:)
 complex(SP), pointer :: WF_symm2_p(:,:)
 !
 call timing('T_space ALDA Osc.',OPR='start')
 !
 WF_symm1_p => tddft_wf%WF_symm1
 WF_symm2_p => tddft_wf%WF_symm2
 rhotwr_p   => tddft_wf%rhotwr_DP
 !
 WF_symm1_p=cZERO
 WF_symm2_p=cZERO
 rhotwr_p=  cZERO
 !
#if defined(__NOTNOW) && ! defined(_CUDA)
 !$omp parallel default(shared), private( K_EXCH_collision, &
 !$omp &        i_T_el,i_k_bz,i_k,i_s, i_p_bz,i_p,i_sp, i_v,i_c,i_sp_c,i_sp_v, &
 !$omp &        i_T_grp_last_with_identity_sym, i_T_el_last_with_identity_sym, &
 !$omp &        i_T_grp_p, N_T_el_p, i_T_el_p, i_g1, i_g2, Nk, is, os)
#endif
 !
 call OPENMP_update(master_thread)
 !
#if defined(__NOTNOW) && ! defined(_CUDA)
 !$omp do schedule(dynamic)
#endif
 NK=-1
 do i_T_el=1,BS_T_grp(i_T_grp)%size
   !
   i_k_bz=BS_T_grp(i_T_grp)%table(i_T_el,1)
   i_k   =Xk%sstar(i_k_bz,1)
   i_s   =Xk%sstar(i_k_bz,2)
   !
   i_p_bz=qindx_X(iq,i_k_bz,1)
   i_g_p =qindx_X(iq,i_k_bz,2)
   !
   i_p   =Xk%sstar(i_p_bz,1)
   i_sp  =Xk%sstar(i_p_bz,2)
   !
   i_v   =BS_T_grp(i_T_grp)%table(i_T_el,2)
   i_c   =BS_T_grp(i_T_grp)%table(i_T_el,3)
   i_sp_c=BS_T_grp(i_T_grp)%table(i_T_el,4)
   i_sp_v=BS_T_grp(i_T_grp)%table(i_T_el,5)
   !
   is=(/0,i_k,i_s ,i_sp_c/)
   os=(/0,i_p,i_sp,i_sp_v/)
   qs=(/minus_G(i_g_p),iq,1/)
   !
   if (mode=="R".or.mode=="C".or.mode=="F") then
     is(1)=i_c
     os(1)=i_v
   else if (mode=="A".or.mode=="Q") then
     ! Same momenta and spin indexes, inverted bands
     is(1)=i_v
     os(1)=i_c
   endif
   !
   l_load_WFs= l_bs_tddft_wf_in_loop .and. (NK(1)/=min(i_k,i_p).or.NK(2)/=max(i_k,i_p))
   if (l_load_WFs) then
#if defined(__NOTNOW) && ! defined(_CUDA)
     !$omp critical
#endif
     if (NK(2)/=-1) call WF_free(WF,keep_fft=.true.,keep_states_to_load=.true.)
     NK=(/min(i_k,i_p),max(i_k,i_p)/)
     call WF_load(WF,NG(1),NG(2),BS_bands,NK,k_extrema_only=.true.,quiet=.true.,&
     &            space='R',title="Kernel exch",keep_states_to_load=.true.)
#if defined(__NOTNOW) && ! defined(_CUDA)
     !$omp end critical
#endif
   endif
   !
   if ( (is(4)/=os(4)) .and. ((.not.l_BS_magnons).or. n_sp_pol==1) ) call error(" TDDFT_ALDA_R_space: wrong spin index")
   if ( (is(4)==os(4)) .and. ((     l_BS_magnons).and.n_sp_pol==2) ) call error(" TDDFT_ALDA_R_space: wrong spin index")
   !
   call WF_apply_symm(is,WF_symm1_p)
   call WF_apply_symm(os,WF_symm2_p)
   !
   if( n_spinor==1 ) then
     rhotwr_p(:)=cmplx(WF_symm1_p(:,1)*conjg(WF_symm2_p(:,1)),kind=DP)
     call perform_fft_3d(qs,rhotwr_p,BS_T_grp(i_T_grp)%O_tddft_L(:,i_T_el,1,1))
   endif
   !
   if(n_spinor==2) then
     !
     do i_spinor=1,n_spinor
       do j_spinor=1,n_spinor
         !
	 do ifft=1,fft_size
	   rhotwr_p(ifft)=cmplx(WF_symm1_p(ifft,i_spinor)*conjg(WF_symm2_p(ifft,j_spinor)),kind=DP)
         enddo
         call perform_fft_3d(qs,rhotwr_p,BS_T_grp(i_T_grp)%O_tddft_L(:,i_T_el,i_spinor,j_spinor))
         !
       enddo
     enddo
   endif
   !
 enddo
 !
 if(l_bs_tddft_wf_in_loop) call WF_free(WF,keep_fft=.true.,keep_states_to_load=.true.)
 !
 nullify(WF_symm1_p)
 nullify(WF_symm2_p)
 nullify(rhotwr_p)
 !
 call timing('T_space ALDA Osc.',OPR='stop')
 !
end subroutine TDDFT_ALDA_eh_space_G_collisions_L
!
!
subroutine perform_fft_3d(qs,rhotw,O_x)
 !
 use BS,             ONLY:BS_n_g_fxc
 use pars,           ONLY:cZERO,cONE,DP,SP,DP
 use R_lattice,      ONLY:DEV_VAR(g_rot)
 use FFT_m,          ONLY:fftw_plan,cufft_plan,fft_size,fft_dim,DEV_VAR(fft_g_table)
 !
 implicit none
 !
 integer :: ig,iGo
 !
 integer,     intent(in)  :: qs(3)
 complex(DP), intent(in)  :: rhotw(fft_size)
 complex(SP), intent(out) :: O_x(BS_n_g_fxc)
 !
 ! perform the actual FFT
 !
#if defined _CUDA
 !
 call fft_3d_cuda(rhotw,fft_dim,+1,cufft_plan)
 !
#else 
 !
#  if defined _FFTW
 call fft_3d(rhotw,fft_dim,+1,fftw_plan)
#  elif defined _FFTSG || (defined _USE_3D_FFT && defined _FFTQE)
 call fft_3d(rhotw,fft_dim,+1)
#  elif defined _FFTQE
 !
 ! the QE fft must be performed in the right
 ! direction. conjg is taken care separately
 ! AF: please note the different definition of isc%rho_tw_rs above
 !     (one extra conjg has been added)
 !
 call fft_3d(rhotw,fft_dim,-1)
 !
#  else
#    error "[CPP] Inconsistent FFT environment"
#  endif
 !
#endif
 !
 iGo=qs(1)
 !
#if defined _FFTQE && !defined _USE_3D_FFT && !defined _CUDA_LOC
 !
 ! one extra conjg is performed on rho_tw_rs
 !
 !$omp parallel do default(shared), private(ig)
 do ig = 1, BS_n_g_fxc
   O_x(ig)= cmplx(conjg(rhotw(fft_g_table)(ig,iGo)),kind=SP)
 enddo
 !
#else
 !
#ifdef _CUDA_LOC
 !$cuf kernel do(1) <<<*,*>>>
#else
 !$omp parallel do default(shared), private(ig)
#endif
 do ig = 1, BS_n_g_fxc
   O_x(ig)= cmplx(rhotw(DEV_VAR(fft_g_table)(ig,iGo)),kind=SP)
 enddo
 !
#endif
 !
end subroutine perform_fft_3d
