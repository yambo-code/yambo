!
!        Copyright (C) 2000-2022 the YAMBO team
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
 use iso_c_binding, ONLY: c_null_ptr
 use pars,          ONLY: SP,DP,cI,cZERO,cZERO_DP
 use FFT_m
 use R_lattice,     ONLY: g_vec,DEV_VAR(g_vec),ng_vec
 use wave_func,     ONLY: wf_ng
 use devxlib,       ONLY: devxlib_memcpy_h2d,devxlib_memcpy_d2h,devxlib_mapped,devxlib_memset_d
 use gpu_m,         ONLY: have_gpu
#ifdef _HIP
 use hipfft_m
#endif
#ifdef _CUDA
 use cufft_m
#endif
#ifdef _MKLGPU
 use mkl_dfti_omp_offload
#endif
 use timing_m
 !
#include<y_memory.h>
 !
 integer,          intent(in) :: spinor_size
 complex(SP),      intent(in) :: f(fft_size,spinor_size)
 complex(SP),      intent(out):: f_gradient(fft_size,spinor_size,3)
 character(len=*), intent(in) :: f_type       ! "wave", "density" or "potential"
 !
 ! Work Space
 !
 integer :: ng_,ic,ig,i_spinor,ierr
 complex(DP), allocatable,          target :: Vr(:), V3g(:,:)
 complex(DP), allocatable DEV_ATTR, target :: Vr_d(:), V3g_d(:,:)
 complex(DP), pointer DEV_ATTR :: Vr_p(:), V3g_p(:,:)
 complex(DP), pointer          :: Vr_h(:)
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
 YAMBO_ALLOC_GPU(DEV_VAR(Vr),(fft_size))
 YAMBO_ALLOC_GPU(DEV_VAR(V3g),(ng_,3))
 !
 ! Synchronize g_vec host/dev if needed
#ifdef _GPU
 if (have_gpu.and..not.devxlib_mapped(DEV_VAR(g_vec))) then
   YAMBO_ALLOC_GPU_SOURCE(DEV_VAR(g_vec),g_vec)
 endif
#endif
 !
 f_gradient = cZERO
 !
 ! f, f_gradient are on host memory
 ! Vr, V3g on GPU memory if CUDA support is enabled
 !
 Vr_h  => Vr
 Vr_p  => DEV_VAR(Vr)
 V3g_p => DEV_VAR(V3g)
 !
 do i_spinor=1,spinor_size
   !
   Vr_h=cmplx(f(:,i_spinor),kind=DP)
   call devxlib_memcpy_h2d(Vr_p,Vr_h)
   !
#if defined _CUDA
   ierr = cufftDestroy(cufft_plan)
   cufft_plan=0
   call fft_3d_cuda(Vr_p,fft_dim,-1,cufft_plan)
#elif defined _HIP
   ierr = hipfftDestroy(hipfft_plan)
   hipfft_plan=c_null_ptr
   call fft_3d_hip(Vr_p,fft_dim,-1,hipfft_plan)
#elif defined _MKLGPU
   ierr = DftiFreeDescriptor(mklgpu_desc)
   call fft_3d_mklgpu(Vr_p,fft_dim,-1,mklgpu_desc)
#else
#  if defined _FFTW
   call dfftw_destroy_plan(fftw_plan)
   fftw_plan = 0
   call fft_3d(Vr_p,fft_dim,-1,fftw_plan)
#  else
   call fft_3d(Vr_p,fft_dim,-1)
#  endif
#endif
   !
   call devxlib_memset_d(V3g_p,cZERO_DP)
   !
   !DEV_ACC_DEBUG data present(Vr_p,V3g_p,fft_g_table,g_vec)
   !DEV_ACC parallel loop collapse(2)
   !DEV_CUF kernel do(2) <<<*,*>>>
   !DEV_OMPGPU target map(present,alloc:Vr_p,V3g_p,fft_g_table,g_vec)
   !DEV_OMPGPU teams loop collapse(2)
   do ic = 1,3
   do ig = 1,ng_
      V3g_p(ig,ic)=cI*real(DEV_VAR(g_vec)(ig,ic),DP)*Vr_p(DEV_VAR(fft_g_table)(ig,1))/real(fft_size,DP)
   enddo
   enddo
   !DEV_OMPGPU end target
   !DEV_ACC_DEBUG end data
   !
   do ic = 1,3
     !
     call devxlib_memset_d(Vr_p,cZERO_DP)
     !
     !DEV_ACC_DEBUG data present(Vr_p,V3g_p,fft_g_table)
     !DEV_ACC parallel loop
     !DEV_CUF kernel do(1) <<<*,*>>>
     !DEV_OMPGPU target map(present,alloc:Vr_p,V3g_p,fft_g_table)
     !DEV_OMPGPU teams loop
     do ig = 1, ng_
       Vr_p(DEV_VAR(fft_g_table)(ig,1)) = V3g_p(ig,ic)
     enddo
     !DEV_OMPGPU end target
     !DEV_ACC_DEBUG end data
     !
#if defined _CUDA
     call fft_3d_cuda(Vr_p,fft_dim,1,cufft_plan)
#elif defined _HIP
     call fft_3d_hip(Vr_p,fft_dim,1,hipfft_plan)
#elif defined _MKLGPU
     call fft_3d_mklgpu(Vr_p,fft_dim,1,mklgpu_desc)
#else
#    if defined _FFTW
     call dfftw_destroy_plan(fftw_plan)
     fftw_plan = 0
     call fft_3d(Vr_p,fft_dim,1,fftw_plan)
#    else
     call fft_3d(Vr_p,fft_dim,1)
#    endif
#endif
     !
     call devxlib_memcpy_d2h(Vr_h,Vr_p)
     f_gradient(:,i_spinor,ic) = cmplx(Vr_h(:),kind=SP)
   enddo
   !
 enddo
 !
 YAMBO_FREE_GPU(DEV_VAR(Vr))
 YAMBO_FREE_GPU(DEV_VAR(V3g))
 YAMBO_FREE(Vr)
 YAMBO_FREE(V3g)
 !
 call timing("eval_Gradient","stop")
 !
end subroutine eval_Gradient
