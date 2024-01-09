!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine XC_eval_gga_kernel(v1rho,v1sigma,v2rho2,v2rhosigma,v2sigma2)
 !
 ! Expression to be derived ...
 ! 
 use pars,           ONLY:DP
 use electrons,      ONLY:n_spin
 use FFT_m,          ONLY:fft_size
 use xc_functionals, ONLY:xc_spin
 !
 implicit none
 !
 real(DP),intent(in) :: v1rho(n_spin,fft_size),v1sigma(xc_spin(1),fft_size)
 real(DP),intent(in) :: v2rho2(xc_spin(1),fft_size),v2rhosigma(xc_spin(1)*n_spin,fft_size),v2sigma2(xc_spin(1)*n_spin,fft_size)
 !
 call warning(' Fxc not coded for GGA. Using LDA part only of the functional ')
 !
 call XC_eval_lda_kernel(v1rho,v2rho2)
 !
end subroutine XC_eval_gga_kernel


