!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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


