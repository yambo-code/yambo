!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
! headers
!
#include<dev_defs.h>
#include<y_memory.h>
!
!
subroutine fft_free()
 !
 use iso_c_binding
 use FFT_m,    ONLY:fft_size,fft_dim,fft_dim_loaded,fft_g_table,&
&                   fft_rot_r,fft_rot_r_inv,fft_multiplier
#if defined _FFTW
 use FFT_m,    ONLY:fftw_plan
#endif
#if defined _GPU
 use FFT_m,    ONLY:cufft_plan,hipfft_plan, &
&                   DEV_VAR(fft_g_table),DEV_VAR(fft_rot_r),DEV_VAR(fft_rot_r_inv)
#  if defined _CUDAF
 use cufft
#  elif defined _OPENACC && defined _CUDA
 use cufft_m
#  elif defined _HIP
 use hipfft_m
#  elif defined _MKLGPU
 use mkl_dfti_omp_offload
 use FFT_m,    ONLY:mklgpu_desc
#  endif
#endif
 !
 USE_MEMORY
 !
 implicit none
 !
 integer :: ierr
 !
 fft_size=0
 fft_dim=0
 fft_dim_loaded=0
 fft_multiplier=1
 !
#if defined _GPU
 YAMBO_FREE_GPU(DEV_VAR(fft_g_table))
 YAMBO_FREE_GPU(DEV_VAR(fft_rot_r))
 YAMBO_FREE_GPU(DEV_VAR(fft_rot_r_inv))
#  if defined _CUDA
 ierr=cufftDestroy(cufft_plan)
 cufft_plan=0
#  elif defined _HIP
 ierr=hipfftDestroy(hipfft_plan)
 hipfft_plan=c_null_ptr
#  elif defined _MKLGPU
 ierr = DftiFreeDescriptor(mklgpu_desc)
#  endif
#endif
 !
 YAMBO_FREE(fft_g_table)
 YAMBO_FREE(fft_rot_r)
 YAMBO_FREE(fft_rot_r_inv)
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan=0
#endif
 !
end subroutine fft_free
