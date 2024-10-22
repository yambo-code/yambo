!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF
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
#include<dev_defs.h>
!
subroutine fft_3d_hip(c_d,n,fft_sign,hipfft_plan)
 !
 use iso_c_binding,        only: c_ptr,c_associated
 use pars,                 only: DP
 use gpu_m,                only: gpu_devsync
 use devxlib_environment
 use hipfft_m
 !
#include<y_memory.h> 
 !
 integer      :: fft_sign,n(3)
 type(c_ptr)  :: hipfft_plan
 complex(DP)  :: c_d(n(1),n(2),n(3))
 !
 ! 
 ! Work Space
 !
 integer(c_int) :: i_sign
 integer :: istat
 !
 if (fft_sign>0) i_sign=HIPFFT_BACKWARD
 if (fft_sign<0) i_sign=HIPFFT_FORWARD
 !
 if (.not. c_associated(hipfft_plan)) then
   !
!#ifdef _OPENACC
!   istat = cufftSetStream( cufft_plan, acc_get_cuda_stream(acc_async_noval) )
!   !istat = gpu_devsync()
!#endif
   !
   istat = hipfftPlan3d( hipfft_plan, n(3), n(2), n(1), HIPFFT_Z2Z)
   !
 endif
 !
! AF_FIX:  to be checked
 !istat = cudaStreamSynchronize(acc_get_cuda_stream(acc_async_noval))
 istat = gpu_devsync()
 !
 !DEV_ACC wait(acc_async_noval)
 !!!!DEV_OMPGPU ???
 !
 !DEV_ACC_DEBUG data present(c_d)
 !DEV_ACC host_data use_device(c_d)
 !DEV_OMPGPU target data use_device_ptr(c_d)
 !
 istat = hipfftExecZ2Z( hipfft_plan, c_loc(c_d), c_loc(c_d), i_sign )
 !
 !DEV_OMPGPU end target data
 !DEV_ACC end host_data
 !DEV_ACC_DEBUG end data
 !
 !DEV_ACC wait(acc_async_noval)
 !!!!DEV_OMPGPU wait ???

! AF_FIX: to be checked
 !istat = cudaStreamSynchronize(acc_get_cuda_stream(acc_async_noval))
 istat = gpu_devsync()
 !
end subroutine fft_3d_hip

