!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DALV
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
!-------------------------------------------------------
! Multipole solution for the polarizability 
! in the GW approximation
!                           exact solution for 1-3 poles
! by Dario A. Leon
!
! Notes:
!
!   1) X(w) is approximated as a sum of poles
!   2) Form of one pole: -R/(w**2 - Q)
!   3) The input are two w and X(w) for each pole
!   4) The output are R and Q coefficients
!   5) Use real(R), imaginary(I) or complex(C) w
!
! * The module works for scalar polarizabilities, so if 
!   you want the solution for matrix element X(G,G',q)
!   you should call RQ_solver for each G, G' and q.  
!-------------------------------------------------------
!
module mpa_m
 !
 use pars,          ONLY:SP,DP
 implicit none
 private

 interface mpa_RQ_1p_solver
    module procedure mpa_RQ_1p_solver_R, mpa_RQ_1p_solver_C
 end interface
 interface mpa_RQ_2p_solver
    module procedure mpa_RQ_2p_solver_R, mpa_RQ_2p_solver_C
 end interface
 interface mpa_RQ_3p_solver
    module procedure mpa_RQ_3p_solver_R, mpa_RQ_3p_solver_C
 end interface

 public :: mpa_RQ_1p_solver
 public :: mpa_RQ_2p_solver
 public :: mpa_RQ_3p_solver
 public :: mpa_RQ_1p_solver_I
 public :: mpa_RQ_2p_solver_I
 public :: mpa_RQ_3p_solver_I

contains

 subroutine mpa_RQ_1p_solver_R(w, x, R, Q) 
   ! analytical form of the 1 pole solution
   implicit none
   !
   real(SP),     intent(in)     :: w(2)
   complex(SP),  intent(in)     :: x(2)
   complex(SP),  intent(out)    :: R, Q
   ! 
   R =  x(1)*x(2)* (w(1)**2- w(2)**2 )/  (x(1) - x(2))
   Q = (x(1)*w(1)**2  -  x(2)*w(2)**2 )/ (x(1) - x(2))
   !
 end subroutine mpa_RQ_1p_solver_R
 !
 subroutine mpa_RQ_1p_solver_C(w, x, R, Q) 
   ! analytical form of the 1 pole solution
   implicit none
   !
   complex(SP),  intent(in)     :: w(2)
   complex(SP),  intent(in)     :: x(2)
   complex(SP),  intent(out)    :: R, Q
   ! 
   R =  x(1)*x(2)* (w(1)**2- w(2)**2 )/  (x(1) - x(2))
   Q = (x(1)*w(1)**2  -  x(2)*w(2)**2 )/ (x(1) - x(2))
   !
 end subroutine mpa_RQ_1p_solver_C

 subroutine mpa_RQ_1p_solver_I(w, x, R, Q)
   ! To be used in the imaginary axis
   implicit none
   !
   real(SP),     intent(in)     :: w(2)
   complex(SP),  intent(in)     :: x(2)
   complex(SP),  intent(out)    :: R, Q
   ! 
   R =  -x(1)*x(2)* (w(1)**2- w(2)**2 )/  (x(1) - x(2))   ! RQ_1p_solverI(w, x, R, Q)  =   
   Q = -(x(1)*w(1)**2  -  x(2)*w(2)**2 )/ (x(1) - x(2))   ! RQ_1p_solver(w, x, -R, -Q)
   !
 end subroutine mpa_RQ_1p_solver_I

 subroutine mpa_RQ_2p_solver_R(w, x, r, q) 
   implicit none
   !
   real(SP),     intent(in)     :: w(4)
   complex(SP),  intent(in)     :: x(4)
   complex(SP),  intent(out)    :: r(2), q(2)
   !
   integer     :: i, j, ii=0
   complex(SP) :: a, b, c !coefficients of the q polynom
   complex(SP) :: den, det
   !
   a =  -(x(1)-x(2))*(x(3)-x(4))*( w(1)**2*w(2)**2 + w(3)**2*w(4)**2 ) &
        +(x(1)-x(3))*(x(2)-x(4))*( w(1)**2*w(3)**2 + w(2)**2*w(4)**2 ) &
        -(x(1)-x(4))*(x(2)-x(3))*( w(1)**2*w(4)**2 + w(2)**2*w(3)**2 ) 
   ! 
   b =   (x(1)-x(2))*w(3)**2*w(4)**2*( x(3)*w(3)**2 - x(4)*w(4)**2 ) - &
         (x(1)-x(3))*w(2)**2*w(4)**2*( x(2)*w(2)**2 - x(4)*w(4)**2 ) + &
         (x(1)-x(4))*w(2)**2*w(3)**2*( x(2)*w(2)**2 - x(3)*w(3)**2 ) + &
         (x(2)-x(3))*w(1)**2*w(4)**2*( x(1)*w(1)**2 - x(4)*w(4)**2 ) - &
         (x(2)-x(4))*w(1)**2*w(3)**2*( x(1)*w(1)**2 - x(3)*w(3)**2 ) + &
         (x(3)-x(4))*w(1)**2*w(2)**2*( x(1)*w(1)**2 - x(2)*w(2)**2 )   
   !
   c =   (x(1)-x(2))*w(1)**2*w(2)**2*( x(3)*w(3)**4 - x(4)*w(4)**4 ) - &
         (x(1)-x(3))*w(1)**2*w(3)**2*( x(2)*w(2)**4 - x(4)*w(4)**4 ) + &
         (x(1)-x(4))*w(1)**2*w(4)**2*( x(2)*w(2)**4 - x(3)*w(3)**4 ) + &
         (x(2)-x(3))*w(2)**2*w(3)**2*( x(1)*w(1)**4 - x(4)*w(4)**4 ) - &
         (x(2)-x(4))*w(2)**2*w(4)**2*( x(1)*w(1)**4 - x(3)*w(3)**4 ) + &
         (x(3)-x(4))*w(3)**2*w(4)**2*( x(1)*w(1)**4 - x(2)*w(2)**4 ) 
   !
   det  = b**2. - 4.*a*c
   q(1) = -(b + sqrt(det))/(2.*a)
   q(2) = -(b - sqrt(det))/(2.*a)
   ! 
   den  =  (q(1)-q(2))*(w(2)-w(3))*(w(2)+w(3))
   r(1) = -(q(1) - w(2)**2)*(q(1) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(2)*(x(2) - x(3)) ) /den
   r(2) =  (q(2) - w(2)**2)*(q(2) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(1)*(x(2) - x(3)) ) /den
   !
 end subroutine mpa_RQ_2p_solver_R

 subroutine mpa_RQ_2p_solver_C(w, x, r, q) 
   implicit none
   !
   complex(SP),  intent(in)     :: w(4)
   complex(SP),  intent(in)     :: x(4)
   complex(SP),  intent(out)    :: r(2), q(2)
   !
   integer     :: i, j, ii=0
   complex(SP) :: a, b, c !coefficients of the q polynom
   complex(SP) :: den, det
   !
   a =  -(x(1)-x(2))*(x(3)-x(4))*( w(1)**2*w(2)**2 + w(3)**2*w(4)**2 ) &
        +(x(1)-x(3))*(x(2)-x(4))*( w(1)**2*w(3)**2 + w(2)**2*w(4)**2 ) &
        -(x(1)-x(4))*(x(2)-x(3))*( w(1)**2*w(4)**2 + w(2)**2*w(3)**2 ) 
   !
   b =   (x(1)-x(2))*w(3)**2*w(4)**2*( x(3)*w(3)**2 - x(4)*w(4)**2 ) - &
         (x(1)-x(3))*w(2)**2*w(4)**2*( x(2)*w(2)**2 - x(4)*w(4)**2 ) + &
         (x(1)-x(4))*w(2)**2*w(3)**2*( x(2)*w(2)**2 - x(3)*w(3)**2 ) + &
         (x(2)-x(3))*w(1)**2*w(4)**2*( x(1)*w(1)**2 - x(4)*w(4)**2 ) - &
         (x(2)-x(4))*w(1)**2*w(3)**2*( x(1)*w(1)**2 - x(3)*w(3)**2 ) + &
         (x(3)-x(4))*w(1)**2*w(2)**2*( x(1)*w(1)**2 - x(2)*w(2)**2 )   
   !
   c =   (x(1)-x(2))*w(1)**2*w(2)**2*( x(3)*w(3)**4 - x(4)*w(4)**4 ) - &
         (x(1)-x(3))*w(1)**2*w(3)**2*( x(2)*w(2)**4 - x(4)*w(4)**4 ) + &
         (x(1)-x(4))*w(1)**2*w(4)**2*( x(2)*w(2)**4 - x(3)*w(3)**4 ) + &
         (x(2)-x(3))*w(2)**2*w(3)**2*( x(1)*w(1)**4 - x(4)*w(4)**4 ) - &
         (x(2)-x(4))*w(2)**2*w(4)**2*( x(1)*w(1)**4 - x(3)*w(3)**4 ) + &
         (x(3)-x(4))*w(3)**2*w(4)**2*( x(1)*w(1)**4 - x(2)*w(2)**4 ) 
   !
   det  = b**2 -4._SP*a*c
   q(1) = -(b + sqrt(det))/(2._SP*a)
   q(2) = -(b - sqrt(det))/(2._SP*a)
   ! 
   den  =  (q(1)-q(2))*(w(2)-w(3))*(w(2)+w(3))
   r(1) = -(q(1) - w(2)**2)*(q(1) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(2)*(x(2) - x(3)) ) /den
   r(2) =  (q(2) - w(2)**2)*(q(2) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(1)*(x(2) - x(3)) ) /den
   !
 end subroutine mpa_RQ_2p_solver_C

 subroutine mpa_RQ_2p_solver_I(w, x, r, q) 
   implicit none
   !
   real(SP),     intent(in)     :: w(4)
   complex(SP),  intent(in)     :: x(4)
   complex(SP),  intent(out)    :: r(2), q(2)
   !
   integer     :: i, j, ii=0
   complex(SP) :: a, b, c !coefficients of the q polynom
   complex(SP) :: den, det
   !
   a =  -(x(1)-x(2))*(x(3)-x(4))*( w(1)**2*w(2)**2 + w(3)**2*w(4)**2 ) &
        +(x(1)-x(3))*(x(2)-x(4))*( w(1)**2*w(3)**2 + w(2)**2*w(4)**2 ) &
        -(x(1)-x(4))*(x(2)-x(3))*( w(1)**2*w(4)**2 + w(2)**2*w(3)**2 ) 
   ! 
   b =   (x(1)-x(2))*w(3)**2*w(4)**2*( x(3)*w(3)**2 - x(4)*w(4)**2 ) - &
         (x(1)-x(3))*w(2)**2*w(4)**2*( x(2)*w(2)**2 - x(4)*w(4)**2 ) + &
         (x(1)-x(4))*w(2)**2*w(3)**2*( x(2)*w(2)**2 - x(3)*w(3)**2 ) + &
         (x(2)-x(3))*w(1)**2*w(4)**2*( x(1)*w(1)**2 - x(4)*w(4)**2 ) - &
         (x(2)-x(4))*w(1)**2*w(3)**2*( x(1)*w(1)**2 - x(3)*w(3)**2 ) + &
         (x(3)-x(4))*w(1)**2*w(2)**2*( x(1)*w(1)**2 - x(2)*w(2)**2 )   
   !
   c =   (x(1)-x(2))*w(1)**2*w(2)**2*( x(3)*w(3)**4 - x(4)*w(4)**4 ) - &
         (x(1)-x(3))*w(1)**2*w(3)**2*( x(2)*w(2)**4 - x(4)*w(4)**4 ) + &
         (x(1)-x(4))*w(1)**2*w(4)**2*( x(2)*w(2)**4 - x(3)*w(3)**4 ) + &
         (x(2)-x(3))*w(2)**2*w(3)**2*( x(1)*w(1)**4 - x(4)*w(4)**4 ) - &
         (x(2)-x(4))*w(2)**2*w(4)**2*( x(1)*w(1)**4 - x(3)*w(3)**4 ) + &
         (x(3)-x(4))*w(3)**2*w(4)**2*( x(1)*w(1)**4 - x(2)*w(2)**4 ) 
   !
   det  = b**2 -4._SP*a*c
   q(1) = (b + sqrt(det))/(2._SP*a)  ! q(1) = - q(1)_Real, r(1) = - r(1)_Real
   q(2) = (b - sqrt(det))/(2._SP*a)  ! q(2) = - q(2)_Real, r(2) = - r(2)_Real
   ! 
   den  =  (q(1)-q(2))*(w(2)-w(3))*(w(2)+w(3))
   r(1) =  (q(1) - w(2)**2)*(q(1) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(2)*(x(2) - x(3)) ) /den
   r(2) = -(q(2) - w(2)**2)*(q(2) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(1)*(x(2) - x(3)) ) /den
   !
 end subroutine mpa_RQ_2p_solver_I

 function f1(t, w, x) 
   implicit none
   real(SP),     intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP) :: f1
   !
   f1 = (x(1)-x(2)) *w(3)**2 *w(t)**2 *(w(3)**2 - w(t)**2) -  &
        (x(1)-x(3)) *w(2)**2 *w(t)**2 *(w(2)**2 - w(t)**2) +  &
        (x(1)-x(t)) *w(2)**2 *w(3)**2 *(w(2)**2 - w(3)**2) +  &
        (x(2)-x(3)) *w(1)**2 *w(t)**2 *(w(1)**2 - w(t)**2) -  &
        (x(2)-x(t)) *w(1)**2 *w(3)**2 *(w(1)**2 - w(3)**2) +  &
        (x(3)-x(t)) *w(1)**2 *w(2)**2 *(w(1)**2 - w(2)**2)    
 end function

 function f1_C(t, w, x) 
   implicit none
   complex(SP),  intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP) :: f1_C
   !
   f1_C = (x(1)-x(2)) *w(3)**2 *w(t)**2 *(w(3)**2 - w(t)**2) -  &
          (x(1)-x(3)) *w(2)**2 *w(t)**2 *(w(2)**2 - w(t)**2) +  &
          (x(1)-x(t)) *w(2)**2 *w(3)**2 *(w(2)**2 - w(3)**2) +  &
          (x(2)-x(3)) *w(1)**2 *w(t)**2 *(w(1)**2 - w(t)**2) -  &
          (x(2)-x(t)) *w(1)**2 *w(3)**2 *(w(1)**2 - w(3)**2) +  &
          (x(3)-x(t)) *w(1)**2 *w(2)**2 *(w(1)**2 - w(2)**2)    
 end function

 function f2(t, w, x) 
   implicit none
   real(SP),     intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP) :: f2
   !
   f2 = (x(1)-x(2)) *w(1)**2 *w(2)**2 *(w(3)**4 - w(t)**4) -  &
        (x(1)-x(3)) *w(1)**2 *w(3)**2 *(w(2)**4 - w(t)**4) +  &
        (x(1)-x(t)) *w(1)**2 *w(t)**2 *(w(2)**4 - w(3)**4) +  &
        (x(2)-x(3)) *w(2)**2 *w(3)**2 *(w(1)**4 - w(t)**4) -  &
        (x(2)-x(t)) *w(2)**2 *w(t)**2 *(w(1)**4 - w(3)**4) +  &
        (x(3)-x(t)) *w(3)**2 *w(t)**2 *(w(1)**4 - w(2)**4)    
 end function

 function f2_C(t, w, x) 
   implicit none
   complex(SP),  intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP) :: f2_C
   !
   f2_C = (x(1)-x(2)) *w(1)**2 *w(2)**2 *(w(3)**4 - w(t)**4) -  &
          (x(1)-x(3)) *w(1)**2 *w(3)**2 *(w(2)**4 - w(t)**4) +  &
          (x(1)-x(t)) *w(1)**2 *w(t)**2 *(w(2)**4 - w(3)**4) +  &
          (x(2)-x(3)) *w(2)**2 *w(3)**2 *(w(1)**4 - w(t)**4) -  &
          (x(2)-x(t)) *w(2)**2 *w(t)**2 *(w(1)**4 - w(3)**4) +  &
          (x(3)-x(t)) *w(3)**2 *w(t)**2 *(w(1)**4 - w(2)**4)    
 end function

 function f3(t, w, x) 
   implicit none
   real(SP),     intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP) :: f3
   !
   f3 = (x(1)-x(2)) *w(1)**4 *w(2)**4 *(w(3)**2 - w(t)**2) -  &
        (x(1)-x(3)) *w(1)**4 *w(3)**4 *(w(2)**2 - w(t)**2) +  &
        (x(1)-x(t)) *w(1)**4 *w(t)**4 *(w(2)**2 - w(3)**2) +  &
        (x(2)-x(3)) *w(2)**4 *w(3)**4 *(w(1)**2 - w(t)**2) -  &
        (x(2)-x(t)) *w(2)**4 *w(t)**4 *(w(1)**2 - w(3)**2) +  &
        (x(3)-x(t)) *w(3)**4 *w(t)**4 *(w(1)**2 - w(2)**2)    
 end function

 function f3_C(t, w, x) 
   implicit none
   complex(SP),  intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP) :: f3_C
   !
   f3_C = (x(1)-x(2)) *w(1)**4 *w(2)**4 *(w(3)**2 - w(t)**2) -  &
          (x(1)-x(3)) *w(1)**4 *w(3)**4 *(w(2)**2 - w(t)**2) +  &
          (x(1)-x(t)) *w(1)**4 *w(t)**4 *(w(2)**2 - w(3)**2) +  &
          (x(2)-x(3)) *w(2)**4 *w(3)**4 *(w(1)**2 - w(t)**2) -  &
          (x(2)-x(t)) *w(2)**4 *w(t)**4 *(w(1)**2 - w(3)**2) +  &
          (x(3)-x(t)) *w(3)**4 *w(t)**4 *(w(1)**2 - w(2)**2)    
 end function

 function f4(t, w, x) 
   implicit none
   real(SP),     intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP)                  :: f4
   !
   f4 = x(1) *w(1)**6 * (w(2)**2 - w(3)**2) * (w(2)**2 - w(t)**2) * (w(3)**2 - w(t)**2) - &
        x(2) *w(2)**6 * (w(1)**2 - w(3)**2) * (w(1)**2 - w(t)**2) * (w(3)**2 - w(t)**2) + &
        x(3) *w(3)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(t)**2) * (w(2)**2 - w(t)**2) - &
        x(t) *w(t)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(3)**2) * (w(2)**2 - w(3)**2)   
 end function

 function f4_C(t, w, x) 
   implicit none
   complex(SP),  intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP) :: f4_C
   !
   f4_C = x(1) *w(1)**6 * (w(2)**2 - w(3)**2) * (w(2)**2 - w(t)**2) * (w(3)**2 - w(t)**2) - &
          x(2) *w(2)**6 * (w(1)**2 - w(3)**2) * (w(1)**2 - w(t)**2) * (w(3)**2 - w(t)**2) + &
          x(3) *w(3)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(t)**2) * (w(2)**2 - w(t)**2) - &
          x(t) *w(t)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(3)**2) * (w(2)**2 - w(3)**2)   
 end function

 subroutine mpa_RQ_3p_solver_R(w, x, r, q) 
   implicit none
   real(SP),     intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   complex(SP),  intent(out)    :: r(3), q(3)
   !
   integer     :: i, j, ii=0
   complex(SP) :: a, b, c, d !coefficients of the q polynom
   complex(SP) :: D1, D2, den, s=complex(1._SP, sqrt(3._SP)), deR
   !
   a =  f1(4, w, x)* ( f2(5, w, x)*f3(6, w, x) - f2(6, w, x)*f3(5, w, x) ) - &
        f1(5, w, x)* ( f2(4, w, x)*f3(6, w, x) - f2(6, w, x)*f3(4, w, x) ) + &
        f1(6, w, x)* ( f2(4, w, x)*f3(5, w, x) - f2(5, w, x)*f3(4, w, x) ) 
   !
   b = -f1(4, w, x)* ( f2(5, w, x)*f4(6, w, x) - f2(6, w, x)*f4(5, w, x) ) + &
        f1(5, w, x)* ( f2(4, w, x)*f4(6, w, x) - f2(6, w, x)*f4(4, w, x) ) - &
        f1(6, w, x)* ( f2(4, w, x)*f4(5, w, x) - f2(5, w, x)*f4(4, w, x) ) 
   !
   c = -f4(4, w, x)* ( f1(5, w, x)*f3(6, w, x) - f1(6, w, x)*f3(5, w, x) ) + &
        f4(5, w, x)* ( f1(4, w, x)*f3(6, w, x) - f1(6, w, x)*f3(4, w, x) ) - &
        f4(6, w, x)* ( f1(4, w, x)*f3(5, w, x) - f1(5, w, x)*f3(4, w, x) ) 
   !
   d = -f4(4, w, x)* ( f2(5, w, x)*f3(6, w, x) - f2(6, w, x)*f3(5, w, x) ) + &
        f4(5, w, x)* ( f2(4, w, x)*f3(6, w, x) - f2(6, w, x)*f3(4, w, x) ) - &
        f4(6, w, x)* ( f2(4, w, x)*f3(5, w, x) - f2(5, w, x)*f3(4, w, x) ) 
   !
   D1   = 3._SP*a*c - b**2
   D2   = 2._SP*b*D1 + 3._SP*a*(b*c - 9._SP*a*d)
   den  = ( D2 + sqrt(4.0_SP*D1**3 + D2**2) )**(1.0_SP/3.0_SP)
   !
   q(1) = -( b + 2.0_SP**(1._SP/3._SP)*D1/den - den/2._SP**(1._SP/3._SP) )/(3.0_SP*a)
   q(2) = -( b - s* D1/2.**(2._SP/3._SP)/den + conjg(s)* den/2._SP**(4._SP/3._SP) )/(3._SP*a)
   q(3) = -( b - conjg(s)* D1/2._SP**(2._SP/3._SP)/den + s* den/2._SP**(4._SP/3._SP) )/(3._SP*a)
   !
   deR  =  (w(1)**2-q(1)) * (w(1)**2-q(2)) * (w(2)**2-q(3)) * (w(3)**2-q(3))*   &
          ((w(2)**2-q(1)) * (w(3)**2-q(2)) - (w(2)**2-q(2)) * (w(3)**2-q(1)))   &
          +(w(1)**2-q(1)) * (w(1)**2-q(3)) * (w(2)**2-q(2)) * (w(3)**2-q(2))*   &
          ((w(2)**2-q(3)) * (w(3)**2-q(1)) - (w(2)**2-q(1)) * (w(3)**2-q(3)))   &
          +(w(1)**2-q(2)) * (w(1)**2-q(3)) * (w(2)**2-q(1)) * (w(3)**2-q(1))*   &
          ((w(2)**2-q(2)) * (w(3)**2-q(3)) - (w(2)**2-q(3)) * (w(3)**2-q(2))) 
   !
   r(1) = (w(1)**2-q(1))*(w(2)**2-q(1))*(w(3)**2-q(1))*(                                                         &
         -(w(1)**2-q(2))*(w(1)**2-q(3))*( (w(2)**2-q(2))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(2)) )*x(1)   &
         +(w(2)**2-q(2))*(w(2)**2-q(3))*( (w(1)**2-q(2))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(2)) )*x(2)   &
         -(w(3)**2-q(2))*(w(3)**2-q(3))*( (w(1)**2-q(2))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(2)) )*x(3) ) /deR 
   !
   r(2) = (w(1)**2-q(2))*(w(2)**2-q(2))*(w(3)**2-q(2))*(                                                         &
         +(w(1)**2-q(1))*(w(1)**2-q(3))*( (w(2)**2-q(1))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(1)) )*x(1)   &
         -(w(2)**2-q(1))*(w(2)**2-q(3))*( (w(1)**2-q(1))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(1)) )*x(2)   &
         +(w(3)**2-q(1))*(w(3)**2-q(3))*( (w(1)**2-q(1))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(1)) )*x(3) ) /deR 
   !
   r(3) = (w(1)**2-q(3))*(w(2)**2-q(3))*(w(3)**2-q(3))*(                                                        &
         -(w(1)**2-q(1))*(w(1)**2-q(2))*( (w(2)**2-q(1))*(w(3)**2-q(2)) - (w(2)**2-q(2))*(w(3)**2-q(1)) )*x(1)  & 
         +(w(2)**2-q(1))*(w(2)**2-q(2))*( (w(1)**2-q(1))*(w(3)**2-q(2)) - (w(1)**2-q(2))*(w(3)**2-q(1)) )*x(2)  & 
         -(w(3)**2-q(1))*(w(3)**2-q(2))*( (w(1)**2-q(1))*(w(2)**2-q(2)) - (w(1)**2-q(2))*(w(2)**2-q(1)) )*x(3) ) /deR
   !                                                                
 end subroutine mpa_RQ_3p_solver_R

 subroutine mpa_RQ_3p_solver_C(w, x, r, q) 
   implicit none
   complex(SP),  intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   complex(SP),  intent(out)    :: r(3), q(3)
   !
   integer     :: i, j, ii=0
   complex(SP) :: a, b, c, d !coefficients of the q polynom
   complex(SP) :: D1, D2, den, s = complex(1._SP, sqrt(3._SP)), deR
   !
   a =  f1_C(4, w, x)* ( f2_C(5, w, x)*f3_C(6, w, x) - f2_C(6, w, x)*f3_C(5, w, x) ) - &
        f1_C(5, w, x)* ( f2_C(4, w, x)*f3_C(6, w, x) - f2_C(6, w, x)*f3_C(4, w, x) ) + &
        f1_C(6, w, x)* ( f2_C(4, w, x)*f3_C(5, w, x) - f2_C(5, w, x)*f3_C(4, w, x) ) 
   !
   b = -f1_C(4, w, x)* ( f2_C(5, w, x)*f4_C(6, w, x) - f2_C(6, w, x)*f4_C(5, w, x) ) + &
        f1_C(5, w, x)* ( f2_C(4, w, x)*f4_C(6, w, x) - f2_C(6, w, x)*f4_C(4, w, x) ) - &
        f1_C(6, w, x)* ( f2_C(4, w, x)*f4_C(5, w, x) - f2_C(5, w, x)*f4_C(4, w, x) ) 
   ! 
   c = -f4_C(4, w, x)* ( f1_C(5, w, x)*f3_C(6, w, x) - f1_C(6, w, x)*f3_C(5, w, x) ) + &
        f4_C(5, w, x)* ( f1_C(4, w, x)*f3_C(6, w, x) - f1_C(6, w, x)*f3_C(4, w, x) ) - &
        f4_C(6, w, x)* ( f1_C(4, w, x)*f3_C(5, w, x) - f1_C(5, w, x)*f3_C(4, w, x) ) 
   !
   d = -f4_C(4, w, x)* ( f2_C(5, w, x)*f3_C(6, w, x) - f2_C(6, w, x)*f3_C(5, w, x) ) + &
        f4_C(5, w, x)* ( f2_C(4, w, x)*f3_C(6, w, x) - f2_C(6, w, x)*f3_C(4, w, x) ) - &
        f4_C(6, w, x)* ( f2_C(4, w, x)*f3_C(5, w, x) - f2_C(5, w, x)*f3_C(4, w, x) ) 
   !
   D1   = 3._SP*a*c - b**2
   D2   = 2._SP*b*D1 + 3._SP*a*(b*c - 9._SP*a*d)
   den  = ( D2 + sqrt(4._SP*D1**3 + D2**2) )**(1._SP/3.0_SP)
   !
   q(1) = -( b + 2._SP**(1._SP/3._SP)*D1/den -den/2._SP**(1._SP/3._SP) )/(3._SP*a)              
   q(2) = -( b - s* D1/2._SP**(2._SP/3._SP)/den + conjg(s)* den/2._SP**(4._SP/3._SP) )/(3._SP*a)  
   q(3) = -( b - conjg(s)* D1/2._SP**(2._SP/3._SP)/den + s* den/2._SP**(4._SP/3._SP) )/(3._SP*a) 
   !
   deR  =  (w(1)**2-q(1)) * (w(1)**2-q(2)) * (w(2)**2-q(3)) * (w(3)**2-q(3))*   &
          ((w(2)**2-q(1)) * (w(3)**2-q(2)) - (w(2)**2-q(2)) * (w(3)**2-q(1)))   &
          +(w(1)**2-q(1)) * (w(1)**2-q(3)) * (w(2)**2-q(2)) * (w(3)**2-q(2))*   &
          ((w(2)**2-q(3)) * (w(3)**2-q(1)) - (w(2)**2-q(1)) * (w(3)**2-q(3)))   &
          +(w(1)**2-q(2)) * (w(1)**2-q(3)) * (w(2)**2-q(1)) * (w(3)**2-q(1))*   &
          ((w(2)**2-q(2)) * (w(3)**2-q(3)) - (w(2)**2-q(3)) * (w(3)**2-q(2))) 
   !
   r(1) = (w(1)**2-q(1))*(w(2)**2-q(1))*(w(3)**2-q(1))*(                                                         &
         -(w(1)**2-q(2))*(w(1)**2-q(3))*( (w(2)**2-q(2))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(2)) )*x(1)   &
         +(w(2)**2-q(2))*(w(2)**2-q(3))*( (w(1)**2-q(2))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(2)) )*x(2)   &
         -(w(3)**2-q(2))*(w(3)**2-q(3))*( (w(1)**2-q(2))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(2)) )*x(3) ) /deR 
   !
   r(2) = (w(1)**2-q(2))*(w(2)**2-q(2))*(w(3)**2-q(2))*(                                                         &
         +(w(1)**2-q(1))*(w(1)**2-q(3))*( (w(2)**2-q(1))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(1)) )*x(1)   &
         -(w(2)**2-q(1))*(w(2)**2-q(3))*( (w(1)**2-q(1))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(1)) )*x(2)   &
         +(w(3)**2-q(1))*(w(3)**2-q(3))*( (w(1)**2-q(1))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(1)) )*x(3) ) /deR 
   !
   r(3) = (w(1)**2-q(3))*(w(2)**2-q(3))*(w(3)**2-q(3))*(                                                        &
         -(w(1)**2-q(1))*(w(1)**2-q(2))*( (w(2)**2-q(1))*(w(3)**2-q(2)) - (w(2)**2-q(2))*(w(3)**2-q(1)) )*x(1)  & 
         +(w(2)**2-q(1))*(w(2)**2-q(2))*( (w(1)**2-q(1))*(w(3)**2-q(2)) - (w(1)**2-q(2))*(w(3)**2-q(1)) )*x(2)  & 
         -(w(3)**2-q(1))*(w(3)**2-q(2))*( (w(1)**2-q(1))*(w(2)**2-q(2)) - (w(1)**2-q(2))*(w(2)**2-q(1)) )*x(3) ) /deR
   !
 end subroutine mpa_RQ_3p_solver_C

 subroutine mpa_RQ_3p_solver_I(w, x, r, q) 
   implicit none
   real(SP),     intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   complex(SP),  intent(out)    :: r(3), q(3)
   !
   integer     :: i, j, ii=0
   complex(SP) :: a, b, c, d !coefficients of the q polynom
   complex(SP) :: D1, D2, den, s = complex(1._SP, sqrt(3._SP)), deR
   !
   a =  f1(4, w, x)* ( f2(5, w, x)*f3(6, w, x) - f2(6, w, x)*f3(5, w, x) ) - &
        f1(5, w, x)* ( f2(4, w, x)*f3(6, w, x) - f2(6, w, x)*f3(4, w, x) ) + &
        f1(6, w, x)* ( f2(4, w, x)*f3(5, w, x) - f2(5, w, x)*f3(4, w, x) ) 
   !
   b = -f1(4, w, x)* ( f2(5, w, x)*f4(6, w, x) - f2(6, w, x)*f4(5, w, x) ) + &
        f1(5, w, x)* ( f2(4, w, x)*f4(6, w, x) - f2(6, w, x)*f4(4, w, x) ) - &
        f1(6, w, x)* ( f2(4, w, x)*f4(5, w, x) - f2(5, w, x)*f4(4, w, x) ) 
   ! 
   c = -f4(4, w, x)* ( f1(5, w, x)*f3(6, w, x) - f1(6, w, x)*f3(5, w, x) ) + &
        f4(5, w, x)* ( f1(4, w, x)*f3(6, w, x) - f1(6, w, x)*f3(4, w, x) ) - &
        f4(6, w, x)* ( f1(4, w, x)*f3(5, w, x) - f1(5, w, x)*f3(4, w, x) ) 
   !
   d = -f4(4, w, x)* ( f2(5, w, x)*f3(6, w, x) - f2(6, w, x)*f3(5, w, x) ) + &
        f4(5, w, x)* ( f2(4, w, x)*f3(6, w, x) - f2(6, w, x)*f3(4, w, x) ) - &
        f4(6, w, x)* ( f2(4, w, x)*f3(5, w, x) - f2(5, w, x)*f3(4, w, x) ) 
   !
   D1   = 3._SP*a*c - b**2
   D2   = 2._SP*b*D1 + 3._SP*a*(b*c - 9._SP*a*d)
   den  = ( D2 + sqrt(4._SP*D1**3 + D2**2) )**(1.0_SP/3.0_SP)
   !
   !q(1) = -q(1)_Real, r(1) = -r(1)_Real
   !q(2) = -q(2)_Real, r(2) = -r(2)_Real
   !q(3) = -q(3)_Real, r(3) = -r(3)_Real
   q(1) = ( b + 2._SP**(1._SP/3._SP)*D1/den - den/2.0_SP**(1._SP/3._SP) )/(3._SP*a)  
   q(2) = ( b - s* D1/2._SP**(2._SP/3._SP)/den + conjg(s)* den/2._SP**(4._SP/3._SP) )/(3._SP*a)  
   q(3) = ( b - conjg(s)* D1/2._SP**(2._SP/3._SP)/den + s* den/2._SP**(4._SP/3._SP) )/(3._SP*a)  
   !
   deR  =  (w(1)**2-q(1)) * (w(1)**2-q(2)) * (w(2)**2-q(3)) * (w(3)**2-q(3))*   &
          ((w(2)**2-q(1)) * (w(3)**2-q(2)) - (w(2)**2-q(2)) * (w(3)**2-q(1)))   &
          +(w(1)**2-q(1)) * (w(1)**2-q(3)) * (w(2)**2-q(2)) * (w(3)**2-q(2))*   &
          ((w(2)**2-q(3)) * (w(3)**2-q(1)) - (w(2)**2-q(1)) * (w(3)**2-q(3)))   &
          +(w(1)**2-q(2)) * (w(1)**2-q(3)) * (w(2)**2-q(1)) * (w(3)**2-q(1))*   &
          ((w(2)**2-q(2)) * (w(3)**2-q(3)) - (w(2)**2-q(3)) * (w(3)**2-q(2))) 
   !
   r(1) =-(w(1)**2-q(1))*(w(2)**2-q(1))*(w(3)**2-q(1))*(                                                         &
         -(w(1)**2-q(2))*(w(1)**2-q(3))*( (w(2)**2-q(2))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(2)) )*x(1)   &
         +(w(2)**2-q(2))*(w(2)**2-q(3))*( (w(1)**2-q(2))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(2)) )*x(2)   &
         -(w(3)**2-q(2))*(w(3)**2-q(3))*( (w(1)**2-q(2))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(2)) )*x(3) ) /deR 
   !
   r(2) =-(w(1)**2-q(2))*(w(2)**2-q(2))*(w(3)**2-q(2))*(                                                         &
         +(w(1)**2-q(1))*(w(1)**2-q(3))*( (w(2)**2-q(1))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(1)) )*x(1)   &
         -(w(2)**2-q(1))*(w(2)**2-q(3))*( (w(1)**2-q(1))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(1)) )*x(2)   &
         +(w(3)**2-q(1))*(w(3)**2-q(3))*( (w(1)**2-q(1))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(1)) )*x(3) ) /deR 
   !
   r(3) =-(w(1)**2-q(3))*(w(2)**2-q(3))*(w(3)**2-q(3))*(                                                        &
         -(w(1)**2-q(1))*(w(1)**2-q(2))*( (w(2)**2-q(1))*(w(3)**2-q(2)) - (w(2)**2-q(2))*(w(3)**2-q(1)) )*x(1)  & 
         +(w(2)**2-q(1))*(w(2)**2-q(2))*( (w(1)**2-q(1))*(w(3)**2-q(2)) - (w(1)**2-q(2))*(w(3)**2-q(1)) )*x(2)  & 
         -(w(3)**2-q(1))*(w(3)**2-q(2))*( (w(1)**2-q(1))*(w(2)**2-q(2)) - (w(1)**2-q(2))*(w(2)**2-q(1)) )*x(3) ) /deR
   !                                                                
 end subroutine mpa_RQ_3p_solver_I

end module mpa_m

