!
! Copyright (C) 2000-2010 C. Hogan  and the YAMBO team 
!              http://www.yambo-code.org
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
module convolute
  !
  ! Routines for convoluting functions 
  !
  use pars,                ONLY : PI, SP
  use units,               ONLY : HARTREE
  use numerical_integration, ONLY : simpson
  implicit none
  real(SP)                     :: gbroad ! The FWHM
  save
  private
  
  public :: init_convolute
  public :: convolute_gaussian, gaussian, gbroad

contains

subroutine print_convolute
  use com, ONLY : msg
  implicit none

  if(abs(gbroad).gt.0.0001) call msg('rs',&
&   'Gaussian broadening applied to spectra (FWHM, eV):',gbroad*HARTREE)
  return
end subroutine print_convolute

subroutine init_convolute(defs) 
  use it_m,                  ONLY : it, initdefs, E_unit,G_unit,T_unit, V_general
  implicit none
  type(initdefs), intent(inout)  :: defs
#if defined _REELS
  call it(defs,'GausConv', '[REELS] Gaussian convolution (FWHM)',gbroad, E_unit, verb_level=V_general )
#else
  call it(defs,'GausConv', '[REELS] Gaussian convolution (FWHM)',gbroad, E_unit )
#endif
  
  return
end subroutine init_convolute

  subroutine convolute_gaussian(f,hw,nw,gbroad_)
    implicit none
    integer, intent(in)  :: nw
    real(SP), intent(inout) :: f(nw)
    real(SP), intent(in) :: hw(nw)
    real(SP), intent(in), optional :: gbroad_ 
    real(SP), parameter  :: gfac = 2.3548_SP
    real(SP)             :: sigma
    real(SP)             :: f_in(nw), fg(nw)
    integer              :: i,j
    real(SP), parameter  :: zero = 1.0e-4

    if (present(gbroad_)) gbroad = gbroad_

    if(abs(gbroad).lt.zero) return

    sigma = gbroad/gfac
    f_in(:) = f(:)

    do i = 1, nw
      !
      ! Construct the convolution f * g
      !
      forall(j=1:nw) fg(j) = f_in(j) * gaussian(sigma, hw(i) - hw(j))
      !
      ! Integrate
      !
      f(i) = simpson(fg, hw, nw) 
      !
    enddo

    return
  end subroutine convolute_gaussian

  real(SP) pure function gaussian(sigma,x)
    implicit none
    real(SP),intent(in)     :: sigma,x

    gaussian = exp(-0.5*(x**2)/(sigma**2))/sigma/sqrt(2.0_SP*PI)
    return
  end function gaussian

end module convolute
