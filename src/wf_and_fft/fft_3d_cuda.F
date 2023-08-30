!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AF
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
subroutine fft_3d_cuda(c_d,n,fft_sign,cufft_plan)
 !
 use pars,          ONLY:DP
 use cudafor
 use cufft
 !
 implicit none
 integer   :: fft_sign,n(3)
 integer   :: cufft_plan
 complex(DP), device :: c_d(n(1),n(2),n(3))
 ! 
 ! Work Space
 !
 integer             :: i_sign,istat
 !
 if (fft_sign>0) i_sign=CUFFT_INVERSE
 if (fft_sign<0) i_sign=CUFFT_FORWARD
 !
 if (cufft_plan==0) then
   !
   istat = cufftPlan3d( cufft_plan, n(3), n(2), n(1), CUFFT_Z2Z )
   !
 endif
 !
 istat = cufftExecZ2Z( cufft_plan, c_d, c_d, i_sign )
 !
end subroutine fft_3d_cuda
