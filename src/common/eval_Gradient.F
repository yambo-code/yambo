!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG AF
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
#include<dev_defs.h>
!
subroutine eval_Gradient(f,f_gradient,spinor_size,f_type)
 !
 ! The gradient of a periodic function f(r):
 ! 
 ! f(r) = \sum f(G) exp(iGr) => FFT: f(G)
 !
 ! f'(r) = \sum iGf(G) exp(iGr) =           
 !
 ! \sum f'(G) exp(iGr)       => FFT^-1
 !
 ! Exploitation of GPU is done when cards are available
 !
 use pars,          ONLY:SP,DP,cI,cZERO,cZERO_DP
 use FFT_m,         ONLY:fft_size,fft_dim,DEV_VAR(fft_g_table)
 use FFT_m,         ONLY:fftw_plan,cufft_plan
 use R_lattice,     ONLY:g_vec,g_vec_d,ng_vec
 use wave_func,     ONLY:wf_ng
 use deviceXlib_m,  ONLY:dev_memcpy
 use cuda_m,        ONLY:have_cuda,cuda_devsync
 use timing_m
 !
#include<memory.h>
 !
 integer,          intent(in) ::spinor_size
 complex(SP),      intent(in) ::f(fft_size,spinor_size)
 complex(SP),      intent(out)::f_gradient(fft_size,spinor_size,3)
 character(len=*), intent(in) ::f_type       ! "wave", "density" or "potential"
 !
 ! Work Space 
 !
 integer :: ng_,ic,ig,i_spinor
 complex(DP), allocatable DEV_ATTR :: Vr(:), V3g(:,:)
 complex(DP), allocatable :: Vr_h(:)
 !
 call timing("eval_Gradient","start")
 !
 select case( trim(f_type) )
 case( "wave" )
   ng_=min(wf_ng,size(DEV_VAR(fft_g_table),1))
 case( "density")
   ng_=min(ng_vec,size(DEV_VAR(fft_g_table),1))
 case( "potential")
   ng_=min(ng_vec,size(DEV_VAR(fft_g_table),1))
 case default
   call error(' [FFT] unknown f_type when computing gradient')
 end select
 !
 YAMBO_ALLOC(Vr,(fft_size))
 YAMBO_ALLOC(Vr_h,(fft_size))
 YAMBO_ALLOC(V3g,(ng_,3))
 !
 ! Synchronize g_vec_d if needed
 if (have_cuda.and..not.allocated(g_vec_d)) then
   YAMBO_ALLOC_SOURCE(g_vec_d,g_vec)
 endif
 !
 f_gradient = cZERO
 !
 ! f, f_gradient are on host memory
 ! Vr, V3g on GPU memory if CUDA support is enabled
 !
 do i_spinor=1,spinor_size
   !
   Vr_h=cmplx(f(:,i_spinor),kind=DP)
   call dev_memcpy(Vr,Vr_h)
   !
#if defined _CUDA
   call fft_3d_cuda(Vr,fft_dim,-1,cufft_plan) 
#else
#  if defined _FFTW
   call dfftw_destroy_plan(fftw_plan)
   fftw_plan = 0
   call fft_3d(Vr,fft_dim,-1,fftw_plan)
#  else
   call fft_3d(Vr,fft_dim,-1)
#  endif
#endif
   !
   V3g(:,:) = cZERO_DP
   !
   !$cuf kernel do(2) <<<*,*>>>
   do ic = 1,3
   do ig = 1,ng_
      V3g(ig,ic)=cI*DEV_VAR(g_vec)(ig,ic)*Vr(DEV_VAR(fft_g_table)(ig,1))/real(fft_size,DP)
   enddo
   enddo
   !
   do ic = 1,3
     !
     Vr = cZERO_DP
     !$cuf kernel do(1) <<<*,*>>>
     do ig = 1, ng_
       Vr(DEV_VAR(fft_g_table)(ig,1)) = V3g(ig,ic)
     enddo
     !
#if defined _CUDA    
     call fft_3d_cuda(Vr,fft_dim,1,cufft_plan) 
#else
#    if defined _FFTW
     call dfftw_destroy_plan(fftw_plan)
     fftw_plan = 0
     call fft_3d(Vr,fft_dim,1,fftw_plan)
#    else
     call fft_3d(Vr,fft_dim,1)
#    endif
#endif
     !
     call dev_memcpy(Vr_h,Vr)
     f_gradient(:,i_spinor,ic) = cmplx(Vr_h(:),kind=SP)
   enddo
   !
 enddo
 !
 YAMBO_FREE(Vr)
 YAMBO_FREE(Vr_h)
 YAMBO_FREE(V3g)
 !
 call timing("eval_Gradient","stop")
 !
end subroutine eval_Gradient
