!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine fft_3d_cuda(c_d,n,fft_sign,cufft_plan)
 !
 use pars,          ONLY:DP
 use dev_openacc_m
#if defined _CUDAF
 use cufft
#endif
#if defined _OPENACC
 use cudafor_m_y
 use cufft_m_y
#endif
 use cuda_m_y,  only: cuda_devsync
 !
#include<memory.h> 
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
   !istat = cufftSetStream( cufft_plan, acc_get_cuda_stream(acc_async_sync) )
   istat = cufftSetStream( cufft_plan, acc_get_cuda_stream(acc_async_noval) )
   !istat = cufftSetStream( cufft_plan, stream_default )
   !istat = cuda_devsync()
#endif
   istat = cufftPlan3d( cufft_plan, n(3), n(2), n(1), CUFFT_Z2Z )
   !
 endif
 !
#ifdef _OPENACC
 ! 2022-02-02 DS and AF TO BE FIXED
 !DEV_ACC wait(acc_async_noval)
 !istat = cudaStreamSynchronize(acc_get_cuda_stream(acc_async_noval))
 istat = cuda_devsync()
#endif
 !
 !DEV_ACC data present(c_d)
 !DEV_ACC host_data use_device(c_d)
 istat = cufftExecZ2Z( cufft_plan, c_d, c_d, i_sign )
 !DEV_ACC end host_data
 !DEV_ACC end data
 !
#ifdef _OPENACC
 !DEV_ACC wait(acc_async_noval)
 !istat = cudaStreamSynchronize(acc_get_cuda_stream(acc_async_noval))
 istat = cuda_devsync()
#endif
 !
end subroutine fft_3d_cuda

