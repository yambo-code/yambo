!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AF
!
! headers
!
#include<dev_defs.h>
#include<y_memory.h> 
!
!=====================================================================
! Driver to 3D FFT: FFTW, Goedecker
!
! fft_sign = +1  : G-space to R-space, output = \sum_G f(G)exp(+iG*R) (FW)
! fft_sign = -1  : R-space to G-space, output = \int_R f(R)exp(-iG*R) (BW)
!
! Note that as the YAMBO convention for the oscillators is
! 
!  <n k | e ^{iq.r} | n' k-q> 
!
! the +1 sign (FW) is used in scatter_Bamp as well.
!
! Note that that inverse operation of 
!
! call fft_3d(wf,fft_dim, 1,bw_plan)
!
! is
!
! call fft_3d(wf/real(fft_size,SP),fft_dim, 1,fw_plan)
!
!=====================================================================
!
!
subroutine fft_3d_cuda(c_d,n,fft_sign,cufft_plan)
 !
 use pars,          ONLY:DP
 use devxlib_environment
 use devxlib_async
#if defined _CUDA
 use cufft_m
#endif
#if defined _OPENACC
 use openacc
#endif
 use gpu_m,  only: gpu_devsync
 !
 USE_MEMORY
 !
 implicit none
 !
 integer   :: fft_sign,n(3)
 integer   :: cufft_plan
 complex(DP) DEV_ATTR :: c_d(n(1),n(2),n(3))
 !
 ! 
 ! Work Space
 !
 integer              :: i_sign,istat
 complex(DP), allocatable :: tmp_d(:,:,:)
 !
 if (fft_sign>0) i_sign=CUFFT_INVERSE
 if (fft_sign<0) i_sign=CUFFT_FORWARD
 !
 if (cufft_plan==0) then
   !
#ifdef _OPENACC
   ! 2022-02-02 DS and AF TO BE FIXED
# if defined _PGI || defined _NV
   !istat = cufftSetStream( cufft_plan, acc_get_cuda_stream(acc_async_sync) )
   istat = cufftSetStream( cufft_plan, acc_get_cuda_stream(acc_async_noval) )
   ! The following lines, in place of the one above,
   ! is needed to compile with gfortran
   ! However they give an error with nvfortran
# else
   istat = cufftSetStream( cufft_plan, devxlib_async2stream(acc_async_noval) )
   !istat = cufftSetStream( cufft_plan, stream_default )
# endif
   !istat = gpu_devsync()
#endif
   istat = cufftPlan3d( cufft_plan, n(3), n(2), n(1), CUFFT_Z2Z )
   !
 endif
 !
#ifdef _OPENACC
 ! 2022-02-02 DS and AF TO BE FIXED
 !DEV_ACC wait(acc_async_noval)
 !istat = cudaStreamSynchronize(acc_get_cuda_stream(acc_async_noval))
 istat = gpu_devsync()
#endif
 !
 !DEV_ACC_DEBUG data present(c_d)
 !DEV_ACC host_data use_device(c_d)
 istat = cufftExecZ2Z( cufft_plan, c_d, c_d, i_sign )
 !DEV_ACC end host_data
 !DEV_ACC_DEBUG end data
 !
#ifdef _OPENACC
 !DEV_ACC wait(acc_async_noval)
 !istat = cudaStreamSynchronize(acc_get_cuda_stream(acc_async_noval))
 istat = gpu_devsync()
#endif
 !
end subroutine fft_3d_cuda

