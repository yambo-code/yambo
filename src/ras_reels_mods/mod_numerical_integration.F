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
module numerical_integration
  use pars,                ONLY : PI, SP
  implicit none
  save
  private
  
  public :: simpson

contains

  real(SP) function simpson(f,x,n_)
    implicit none
    integer, intent(in)  :: n_
    real(SP), intent(in) :: f(n_)
    real(SP), intent(in) :: x(n_)
    ! ws
    integer              :: n, n_int, i 
    real(SP)             :: a, b, hs, hs3, sumodd, sumeven
    ! 
    !  Quick return
    ! 
    n = n_
    if(n.eq.1) then
      simpson = f(1)
      return
    else if(n.eq.2) then
      simpson = (f(1)+f(2))/2.0_SP
      return
    endif
    ! 
    ! Truncate for now if even (better add trapezoidal!)
    ! 
    n_int = n - 1 
    if(mod(n,2).eq.1) then
      n_int = n_int - 1
      n     = n     - 1
    endif
    !
    ! Calculate integral
    !
    a = x(1)
    b = x(n)
    hs = abs(b-a)/real(n)
    hs3 = hs/3.0_SP
    sumodd = f(2)
    sumeven = 0.0_SP

    do i = 3,n-1,2
      sumeven = sumeven + f(i)
      sumodd = sumodd + f(i+1)
    enddo
    simpson = (f(1) + f(n) + 4.0_SP*sumodd + 2.0_SP*sumeven)*hs3
    return

  end function simpson

end module numerical_integration
