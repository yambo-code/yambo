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

!@ MANUAL
module pw_struct_module
implicit none
! This module contains the definition of pw_struct and its methods
private
public :: pw_struct, &
          operator(==)
!@ END MANUAL

!@ MANUAL
type pw_struct
  real a(3,3), b(3,3), a_omega, b_omega
end type pw_struct
! This object contains the informations about the periodic cell
! a(:,1), a(:,2) and a(:,3) :: basis vectors (real space)
! b(:,1), b(:,2) and b(:,3) :: basis vectors (reciprocal space)
! a_omega                   :: cell volume (real space)
! b_omega                   :: cell volume (reciprocal space)
! All quantities in atomic units
!   a       [a.u.]
!   b       [a.u.^-1]
!   a_omega [a.u.^3]
!   b_omega [a.u.^-3]
!@ END MANUAL

interface operator(==)
  module procedure pw_struct_is_equal
end interface

contains

!subroutine pw_struct_set(struct,a)
 ! Subroutine for struct creation.
!  It takes as an argument the a matrix and sets up
 ! all quantities in struct.
!  use num_module
!  type(pw_struct), intent(out) :: struct
!  real,            intent(in)  :: a(3,3)
!  real :: tmp(3,3)
 ! a(:,1), a(:,2) e a(:,3) :: basis vectors (real space, a.u.)
 !@ END MANUAL
!  struct%a = a
!  struct%a_omega = num_determinant(struct%a)
!  if(abs(struct%a_omega)<1e-8) ERROR("")
!  tmp = num_inverse(struct%a)
!  struct%b = num_2pi * transpose(tmp)
!  struct%b_omega = num_determinant(struct%b)
!end subroutine pw_struct_set

!@ MANUAL
function pw_struct_is_equal(struct1,struct2)
! Compares two objects
! interfaced with operator(==)
! NOTE it compares only struct1%a and struct2%a
  logical                     :: pw_struct_is_equal
  type(pw_struct), intent(in) :: struct1,struct2
!@ END MANUAL
  pw_struct_is_equal = all(abs(struct1%a-struct2%a)<1e-8)
end function pw_struct_is_equal

end module pw_struct_module
