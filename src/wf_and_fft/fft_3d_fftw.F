!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
subroutine fft_3d(c,n,fft_sign,fftw_plan)
 !
 use pars,          ONLY:DP
 use openmp,        ONLY:n_threads_now,n_threads_FFT
 implicit none
 integer     :: fft_sign,n(3)
 integer(8)  :: fftw_plan
#if defined _OPENMP
 integer     :: iret
#endif
 complex(DP) :: c(n(1),n(2),n(3))
 ! 
 ! Work Space
 !
 integer             :: i_sign
 integer , parameter :: FFTW_ESTIMATE=64
 !
 if (fftw_plan==0) then
   if (fft_sign>0) i_sign=+1
   if (fft_sign<0) i_sign=-1
#if defined _OPENMP && defined _FFTW_OMP
   call dfftw_init_threads(iret)
   if (n_threads_FFT > 0) then
     call dfftw_plan_with_nthreads(n_threads_FFT)
   else
     call dfftw_plan_with_nthreads(n_threads_now)
   endif
#endif
#if defined _OPENMP
!$omp single
#endif
   call dfftw_plan_dft_3d(fftw_plan,n(1),n(2),n(3),c,c,i_sign,FFTW_ESTIMATE)
#if defined _OPENMP
!$omp end single
#endif
 endif
 !
 call dfftw_execute_dft(fftw_plan,c,c)
 !
end subroutine
