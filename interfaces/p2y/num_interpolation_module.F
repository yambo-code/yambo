! Self-energies and eXcitations (SaX)
! Copyright (C) 2006 SaX developers team
!
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License
! as published by the Free Software Foundation; either version 2
! of the License, or (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

!#include "tools_error.h"
!@ MANUAL
module num_interpolation_module
use pars,  ONLY:SP
implicit none
! This module provides the definition of "num_interpolation" and its methods
private
public :: num_interpolation_init
public :: num_interpolation_destroy
public :: num_interpolation_calc
public :: num_interpolation_calc_der
public :: num_interpolation

!@ END MANUAL

!@ MANUAL
! This is a type for numerical interpolation
! It contains a table of n+1 pairs of reals,
! ordered in decreasing or increasing x.
! if parity is different from 0 it can be used to improve
! the interpolation
type num_interpolation
  integer      :: n
  real(SP)         :: xmin,xmax
  real(SP),pointer :: x(:) => null()
  real(SP),pointer :: y(:) => null()
  integer      :: parity
end type num_interpolation
!@ END MANUAL

contains


subroutine num_interpolation_init(interpolation,xmin,xmax,delta,parity)
! Initialise the table.
! delta is an excess approximation to the interpoint distance
! (the point are equally spaced and span the interval [xmin,xmax]).
! If parity/=0, then xmin should be 0.0 and xmax should be positive.
  type (num_interpolation), intent(out) :: interpolation
  real(SP),                 intent(in)  :: xmin,xmax,delta
  integer, optional,        intent(in)  :: parity
!@ END MANUAL
  integer :: i
  interpolation%n = ceiling(abs(xmax - xmin)/delta) * sign(1._SP,(xmax - xmin))
  allocate(interpolation%x(0:interpolation%n))
  allocate(interpolation%y(0:interpolation%n))
  interpolation%xmin = xmin
  interpolation%xmax = xmax
  do i=0,interpolation%n
    interpolation%x(i) = xmin + (xmax - xmin) * real(i) / interpolation%n
  end do
  interpolation%y=0._SP
  if(present(parity)) then
    interpolation%parity = parity
  else
    interpolation%parity = 0
  end if
  if(interpolation%parity /= 0 .and. (xmin /= 0._SP .or. xmax<0._SP)) then
!   WARNING("")
    write(*,*) "WARNING in NUM INTERPOLATION"
    interpolation%parity = 0
  end if
end subroutine num_interpolation_init

!@ MANUAL
subroutine num_interpolation_destroy(interpolation)
! Destroys the table

  type (num_interpolation), intent(inout) :: interpolation
!@ END MANUAL
  if (associated(interpolation%x)) deallocate(interpolation%x)
  if (associated(interpolation%y)) deallocate(interpolation%y)
end subroutine num_interpolation_destroy

subroutine num_interpolation_little_table(interpolation,x,order,tab_x,tab_y)
! Private routine to build a small table to be used by the low-level
! interpolation routines.
  use numrec_module
  type (num_interpolation), intent(in)  :: interpolation
  real(SP),                     intent(in)  :: x
  integer,                  intent(in)  :: order
  real(SP),                     intent(out) :: tab_x(0:order-1),tab_y(0:order-1)
  integer :: j,k
  if(interpolation%parity==0) then
    call numrec_locate(interpolation%x,interpolation%n+1,x,j)
    k = min(max(j-(order-1)/2,1),interpolation%n+1+1-order) - 1
    if(k<0 .or. k+order-1>interpolation%n) stop "little table error 1" 
    tab_x(:) = interpolation%x(k:k+order-1)
    tab_y(:) = interpolation%y(k:k+order-1)
  else
    call numrec_locate(interpolation%x,interpolation%n+1,abs(x),j)
    k = min(j-(order-1)/2,interpolation%n+1+1-order) - 1
    if(k+order-1>interpolation%n) stop "little table error 2" 
    if(k>=0) then
      tab_x(:) = interpolation%x(k:k+order-1)
      if(interpolation%parity>0) then
        tab_y(:) = interpolation%y(k:k+order-1)
      else
        tab_y(:) = interpolation%y(k:k+order-1) * sign(1._SP,x)
      end if
    else
      tab_x(-k:order-1) = interpolation%x(0:k+order-1)
      tab_y(-k:order-1) = interpolation%y(0:k+order-1)
      tab_x(0:-k-1)     = - interpolation%x(-k:1:-1)
      tab_y(0:-k-1)     = interpolation%y(-k:1:-1) * sign(1,interpolation%parity)
    end if
  end if
end subroutine num_interpolation_little_table

!@ MANUAL
function num_interpolation_calc(interpolation,x,order)
  use numrec_module
! Returns the interpolated value in the point x, using a order-1 polynomial
  real(SP)                                 :: num_interpolation_calc
  type (num_interpolation), intent(in) :: interpolation
  real(SP),                     intent(in) :: x
  integer,                  intent(in) :: order
!@ END MANUAL
  real(SP) :: tab_x(order),tab_y(order)
  real(SP) :: y,dy
! write(*,*) "START: num_interpolation_calc"
  call num_interpolation_little_table(interpolation,x,order,tab_x,tab_y)
! write(*,*) "call num_interpolation_little_table done"
  call numrec_polint(tab_x,tab_y,order,abs(x),y,dy)
! write(*,*) "call numrec_polint done"
  num_interpolation_calc = y
end function num_interpolation_calc

!@ MANUAL
function num_interpolation_calc_der(interpolation,x,order,ider)
  use numrec_module
! Returns the interpolated value of the ider-th derivative in the point x
! Note: ider=0 => the function not derived (in this case prefer num_interpolation_calc)
  real(SP)                                 :: num_interpolation_calc_der
  type (num_interpolation), intent(in) :: interpolation
  real(SP),                     intent(in) :: x
  integer,                  intent(in) :: order
  integer,                  intent(in) :: ider
!@ END MANUAL
  real(SP)    :: tab_x(order),tab_y(order)
  real(SP)    :: cof(order),pd(0:ider)
  call num_interpolation_little_table(interpolation,x,order,tab_x,tab_y)
  call numrec_polcof(tab_x,tab_y,order,cof)
  call numrec_ddpoly(cof,order,x,pd,ider+1)
  num_interpolation_calc_der = pd(ider)
end function num_interpolation_calc_der

end module num_interpolation_module
