!
! License-Identifier: GPL
!
! Copyright (C) 2012 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG AF DS
!
subroutine eval_Gradient(f,f_gradient,ng_)
 !
 ! The gradient of a periodic function f(r):
 ! 
 ! f(r) = \sum f(G) exp(iGr) => FFT: f(G)
 !
 ! f'(r) = \sum iGf(G) exp(iGr) =           
 !
 ! \sum f'(G) exp(iGr)       => FFT^-1
 !
 use pars,          ONLY:SP,DP,cI,cZERO,cZERO_DP
 use FFT_m,         ONLY:fft_size,fft_dim,fft_g_table
#if defined _FFTW
 use FFT_m,         ONLY:fftw_plan
#endif
 use R_lattice,     ONLY:g_vec,ng_vec
 use wave_func,     ONLY:wf_ng
 !
#include<y_memory.h>
 !
 integer,          intent(in) ::ng_
 complex(SP),      intent(in) ::f(fft_size)
 complex(SP),      intent(out)::f_gradient(fft_size,3)
 !
 ! Work Space 
 !
 integer :: ic,ig
 complex(DP) :: Vr(fft_size), V3g(ng_,3)
 !
 V3g(:,:) = cZERO_DP
 !
 f_gradient = cZERO
 !
 Vr=cmplx(f(:),kind=DP)
 !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan = 0
 call fft_3d(Vr,fft_dim,-1,fftw_plan)
#else
 call fft_3d(Vr,fft_dim,-1)
#endif
 !
 forall (ic=1:3,ig=1:ng_) V3g(ig,ic)=cI*real(g_vec(ig,ic),DP)*Vr(fft_g_table(ig,1))/real(fft_size,DP)
 !
 do ic = 1,3
   Vr = cZERO_DP
   Vr(fft_g_table(1:ng_,1)) = V3g(1:ng_,ic)
#if defined _FFTW
   call dfftw_destroy_plan(fftw_plan)
   fftw_plan = 0
   call fft_3d(Vr,fft_dim,1,fftw_plan)
#else
   call fft_3d(Vr,fft_dim,1)
#endif
   f_gradient(:,ic) = cmplx(Vr(:),kind=SP)
 enddo
 !
end subroutine eval_Gradient
!
!
subroutine eval_Gradient_wf(wf,wf_gradient)
 !
 use pars,          ONLY:SP
 use FFT_m,         ONLY:fft_size,fft_g_table
 use wave_func,     ONLY:wf_ng
 use electrons,     ONLY:n_spinor
 !
 implicit none
 !
 complex(SP),      intent(in) ::wf(fft_size,n_spinor)
 complex(SP),      intent(out)::wf_gradient(fft_size,n_spinor,3)
 !
 complex(SP) :: wf_tmp(fft_size,3)
 !
 integer  :: i_spinor,ng_
 !
 ng_=min(wf_ng,size(fft_g_table,1))
 !
 do i_spinor=1,n_spinor
   call eval_Gradient(wf(:,i_spinor),wf_tmp,ng_)
   wf_gradient(:,i_spinor,:)=wf_tmp
 enddo
 !
end subroutine eval_Gradient_wf
!
!
subroutine eval_Gradient_rho_pot(Vrho,Vrho_gradient)
 !
 use pars,          ONLY:SP
 use FFT_m,         ONLY:fft_size,fft_g_table
 use R_lattice,     ONLY:g_vec,ng_vec
 !
 implicit none
 !
 complex(SP),      intent(in) ::Vrho(fft_size)
 complex(SP),      intent(out)::Vrho_gradient(fft_size,3)
 !
 integer :: ng_
 !
 ng_=min(ng_vec,size(fft_g_table,1))
 call eval_Gradient(Vrho,Vrho_gradient,ng_)
 !
end subroutine eval_Gradient_rho_pot