!--------------------------------
!..   Fortran .f95 MODULE      ..
!--------------------------------

!-------------------------------------------------------
! Multipole solution for the polarizability 
! in the GW approximation
!                           exact solution for 1-3 poles
! by Dario A. Leon
!-------------------------------------------------------

!-------------------------------------------------------
! Notes:

!   1) X(w) is approximated as a sum of poles
!   2) Form of one pole: -R/(w**2 - Q)
!   3) The input are two w and X(w) for each pole
!   4) The output are R and Q coefficients
!   5) Use real(R), imaginary(I) or complex(C) w

! *The module works for scalar polarizabilities, so if 
!   you want the solution for matrix element X(G,G',q)
!      you should call RQ_solver for each G, G' and q.  
!-------------------------------------------------------


MODULE mod_exact_3p

 use pars,          ONLY:SP,DP

IMPLICIT NONE

private RQ_1p_solver_R, RQ_1p_solver_C, RQ_2p_solver_R, RQ_2p_solver_C, RQ_3p_solver_R, RQ_3p_solver_C

interface RQ_1p_solver
  module procedure RQ_1p_solver_R, RQ_1p_solver_C
end interface

interface RQ_2p_solver
  module procedure RQ_2p_solver_R, RQ_2p_solver_C
end interface

interface RQ_3p_solver
  module procedure RQ_3p_solver_R, RQ_3p_solver_C
end interface

CONTAINS

!!----------------------------------------------------------

 subroutine RQ_1p_solver_R(w, x, R, Q) 

   real(SP),     intent(in)     :: w(2)
   complex(SP),  intent(in)     :: x(2)
   complex(SP),  intent(out)    :: R, Q
 
   R =  x(1)*x(2)* (w(1)**2- w(2)**2 )/  (x(1) - x(2))   ! analytical form of the 1 pole solution  
   Q = (x(1)*w(1)**2  -  x(2)*w(2)**2 )/ (x(1) - x(2))   ! analytical form of the 1 pole solution  

 end subroutine RQ_1p_solver_R


 subroutine RQ_1p_solver_C(w, x, R, Q) 

   complex(SP),  intent(in)     :: w(2)
   complex(SP),  intent(in)     :: x(2)
   complex(SP),  intent(out)    :: R, Q
 
   R =  x(1)*x(2)* (w(1)**2- w(2)**2 )/  (x(1) - x(2))   ! analytical form of the 1 pole solution  
   Q = (x(1)*w(1)**2  -  x(2)*w(2)**2 )/ (x(1) - x(2))   ! analytical form of the 1 pole solution  

 end subroutine RQ_1p_solver_C


 subroutine RQ_1p_solver_I(w, x, R, Q)  ! To be used in the imaginary axis

   real(SP),     intent(in)     :: w(2)
   complex(SP),  intent(in)     :: x(2)
   complex(SP),  intent(out)    :: R, Q
 
   R =  -x(1)*x(2)* (w(1)**2- w(2)**2 )/  (x(1) - x(2))   ! RQ_1p_solverI(w, x, R, Q)  =   
   Q = -(x(1)*w(1)**2  -  x(2)*w(2)**2 )/ (x(1) - x(2))   ! RQ_1p_solver(w, x, -R, -Q)

 end subroutine RQ_1p_solver_I

!!----------------------------------------------------------

 subroutine RQ_2p_solver_R(w, x, r, q) 

   real(SP),     intent(in)     :: w(4)
   complex(SP),  intent(in)     :: x(4)
   complex(SP),  intent(out)    :: r(2), q(2)

  ! Local variables:
   integer                 :: i, j, ii=0
   complex(SP)             :: a, b, c !coefficients of the q polynom
   complex(SP)             :: den, det
  !--------------------

  a =  -(x(1)-x(2))*(x(3)-x(4))*( w(1)**2*w(2)**2 + w(3)**2*w(4)**2 ) &
       +(x(1)-x(3))*(x(2)-x(4))*( w(1)**2*w(3)**2 + w(2)**2*w(4)**2 ) &
       -(x(1)-x(4))*(x(2)-x(3))*( w(1)**2*w(4)**2 + w(2)**2*w(3)**2 ) 
 
  b =   (x(1)-x(2))*w(3)**2*w(4)**2*( x(3)*w(3)**2 - x(4)*w(4)**2 ) - &
        (x(1)-x(3))*w(2)**2*w(4)**2*( x(2)*w(2)**2 - x(4)*w(4)**2 ) + &
        (x(1)-x(4))*w(2)**2*w(3)**2*( x(2)*w(2)**2 - x(3)*w(3)**2 ) + &
        (x(2)-x(3))*w(1)**2*w(4)**2*( x(1)*w(1)**2 - x(4)*w(4)**2 ) - &
        (x(2)-x(4))*w(1)**2*w(3)**2*( x(1)*w(1)**2 - x(3)*w(3)**2 ) + &
        (x(3)-x(4))*w(1)**2*w(2)**2*( x(1)*w(1)**2 - x(2)*w(2)**2 )   

  c =   (x(1)-x(2))*w(1)**2*w(2)**2*( x(3)*w(3)**4 - x(4)*w(4)**4 ) - &
        (x(1)-x(3))*w(1)**2*w(3)**2*( x(2)*w(2)**4 - x(4)*w(4)**4 ) + &
        (x(1)-x(4))*w(1)**2*w(4)**2*( x(2)*w(2)**4 - x(3)*w(3)**4 ) + &
        (x(2)-x(3))*w(2)**2*w(3)**2*( x(1)*w(1)**4 - x(4)*w(4)**4 ) - &
        (x(2)-x(4))*w(2)**2*w(4)**2*( x(1)*w(1)**4 - x(3)*w(3)**4 ) + &
        (x(3)-x(4))*w(3)**2*w(4)**2*( x(1)*w(1)**4 - x(2)*w(2)**4 ) 

  det  = b**2. - 4.*a*c
  q(1) = -(b + sqrt(det))/(2.*a)
  q(2) = -(b - sqrt(det))/(2.*a)
 
  den  =  (q(1)-q(2))*(w(2)-w(3))*(w(2)+w(3))
  r(1) = -(q(1) - w(2)**2)*(q(1) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(2)*(x(2) - x(3)) ) /den
  r(2) =  (q(2) - w(2)**2)*(q(2) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(1)*(x(2) - x(3)) ) /den                                                                     

 end subroutine RQ_2p_solver_R


 subroutine RQ_2p_solver_C(w, x, r, q) 

   complex(SP),  intent(in)     :: w(4)
   complex(SP),  intent(in)     :: x(4)
   complex(SP),  intent(out)    :: r(2), q(2)

  ! Local variables:
   integer                 :: i, j, ii=0
   complex(SP)             :: a, b, c !coefficients of the q polynom
   complex(SP)             :: den, det
  !--------------------

  a =  -(x(1)-x(2))*(x(3)-x(4))*( w(1)**2*w(2)**2 + w(3)**2*w(4)**2 ) &
       +(x(1)-x(3))*(x(2)-x(4))*( w(1)**2*w(3)**2 + w(2)**2*w(4)**2 ) &
       -(x(1)-x(4))*(x(2)-x(3))*( w(1)**2*w(4)**2 + w(2)**2*w(3)**2 ) 
 
  b =   (x(1)-x(2))*w(3)**2*w(4)**2*( x(3)*w(3)**2 - x(4)*w(4)**2 ) - &
        (x(1)-x(3))*w(2)**2*w(4)**2*( x(2)*w(2)**2 - x(4)*w(4)**2 ) + &
        (x(1)-x(4))*w(2)**2*w(3)**2*( x(2)*w(2)**2 - x(3)*w(3)**2 ) + &
        (x(2)-x(3))*w(1)**2*w(4)**2*( x(1)*w(1)**2 - x(4)*w(4)**2 ) - &
        (x(2)-x(4))*w(1)**2*w(3)**2*( x(1)*w(1)**2 - x(3)*w(3)**2 ) + &
        (x(3)-x(4))*w(1)**2*w(2)**2*( x(1)*w(1)**2 - x(2)*w(2)**2 )   

  c =   (x(1)-x(2))*w(1)**2*w(2)**2*( x(3)*w(3)**4 - x(4)*w(4)**4 ) - &
        (x(1)-x(3))*w(1)**2*w(3)**2*( x(2)*w(2)**4 - x(4)*w(4)**4 ) + &
        (x(1)-x(4))*w(1)**2*w(4)**2*( x(2)*w(2)**4 - x(3)*w(3)**4 ) + &
        (x(2)-x(3))*w(2)**2*w(3)**2*( x(1)*w(1)**4 - x(4)*w(4)**4 ) - &
        (x(2)-x(4))*w(2)**2*w(4)**2*( x(1)*w(1)**4 - x(3)*w(3)**4 ) + &
        (x(3)-x(4))*w(3)**2*w(4)**2*( x(1)*w(1)**4 - x(2)*w(2)**4 ) 

  det  = b**2. - 4.*a*c
  q(1) = -(b + sqrt(det))/(2.*a)
  q(2) = -(b - sqrt(det))/(2.*a)
 
  den  =  (q(1)-q(2))*(w(2)-w(3))*(w(2)+w(3))
  r(1) = -(q(1) - w(2)**2)*(q(1) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(2)*(x(2) - x(3)) ) /den
  r(2) =  (q(2) - w(2)**2)*(q(2) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(1)*(x(2) - x(3)) ) /den                                                                     

 end subroutine RQ_2p_solver_C


 subroutine RQ_2p_solver_I(w, x, r, q) 

   real(SP),     intent(in)     :: w(4)
   complex(SP),  intent(in)     :: x(4)
   complex(SP),  intent(out)    :: r(2), q(2)

  ! Local variables:
   integer                 :: i, j, ii=0
   complex(SP)             :: a, b, c !coefficients of the q polynom
   complex(SP)             :: den, det
  !--------------------

  a =  -(x(1)-x(2))*(x(3)-x(4))*( w(1)**2*w(2)**2 + w(3)**2*w(4)**2 ) &
       +(x(1)-x(3))*(x(2)-x(4))*( w(1)**2*w(3)**2 + w(2)**2*w(4)**2 ) &
       -(x(1)-x(4))*(x(2)-x(3))*( w(1)**2*w(4)**2 + w(2)**2*w(3)**2 ) 
 
  b =   (x(1)-x(2))*w(3)**2*w(4)**2*( x(3)*w(3)**2 - x(4)*w(4)**2 ) - &
        (x(1)-x(3))*w(2)**2*w(4)**2*( x(2)*w(2)**2 - x(4)*w(4)**2 ) + &
        (x(1)-x(4))*w(2)**2*w(3)**2*( x(2)*w(2)**2 - x(3)*w(3)**2 ) + &
        (x(2)-x(3))*w(1)**2*w(4)**2*( x(1)*w(1)**2 - x(4)*w(4)**2 ) - &
        (x(2)-x(4))*w(1)**2*w(3)**2*( x(1)*w(1)**2 - x(3)*w(3)**2 ) + &
        (x(3)-x(4))*w(1)**2*w(2)**2*( x(1)*w(1)**2 - x(2)*w(2)**2 )   

  c =   (x(1)-x(2))*w(1)**2*w(2)**2*( x(3)*w(3)**4 - x(4)*w(4)**4 ) - &
        (x(1)-x(3))*w(1)**2*w(3)**2*( x(2)*w(2)**4 - x(4)*w(4)**4 ) + &
        (x(1)-x(4))*w(1)**2*w(4)**2*( x(2)*w(2)**4 - x(3)*w(3)**4 ) + &
        (x(2)-x(3))*w(2)**2*w(3)**2*( x(1)*w(1)**4 - x(4)*w(4)**4 ) - &
        (x(2)-x(4))*w(2)**2*w(4)**2*( x(1)*w(1)**4 - x(3)*w(3)**4 ) + &
        (x(3)-x(4))*w(3)**2*w(4)**2*( x(1)*w(1)**4 - x(2)*w(2)**4 ) 

  det  = b**2. - 4.*a*c
  q(1) = (b + sqrt(det))/(2.*a)  ! q(1) = - q(1)_Real, r(1) = - r(1)_Real
  q(2) = (b - sqrt(det))/(2.*a)  ! q(2) = - q(2)_Real, r(2) = - r(2)_Real
 
  den  =  (q(1)-q(2))*(w(2)-w(3))*(w(2)+w(3))
  r(1) =  (q(1) - w(2)**2)*(q(1) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(2)*(x(2) - x(3)) ) /den
  r(2) = -(q(2) - w(2)**2)*(q(2) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(1)*(x(2) - x(3)) ) /den                                                                     

 end subroutine RQ_2p_solver_I
!!----------------------------------------------------------

 function f1(t, w, x) 

   real(SP),     intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP)                  :: f1

   f1 = (x(1)-x(2)) *w(3)**2 *w(t)**2 *(w(3)**2 - w(t)**2) -  &
        (x(1)-x(3)) *w(2)**2 *w(t)**2 *(w(2)**2 - w(t)**2) +  &
        (x(1)-x(t)) *w(2)**2 *w(3)**2 *(w(2)**2 - w(3)**2) +  &
        (x(2)-x(3)) *w(1)**2 *w(t)**2 *(w(1)**2 - w(t)**2) -  &
        (x(2)-x(t)) *w(1)**2 *w(3)**2 *(w(1)**2 - w(3)**2) +  &
        (x(3)-x(t)) *w(1)**2 *w(2)**2 *(w(1)**2 - w(2)**2)    

 endfunction f1

 function f1_C(t, w, x) 

   complex(SP),  intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP)                  :: f1_C

   f1_C = (x(1)-x(2)) *w(3)**2 *w(t)**2 *(w(3)**2 - w(t)**2) -  &
          (x(1)-x(3)) *w(2)**2 *w(t)**2 *(w(2)**2 - w(t)**2) +  &
          (x(1)-x(t)) *w(2)**2 *w(3)**2 *(w(2)**2 - w(3)**2) +  &
          (x(2)-x(3)) *w(1)**2 *w(t)**2 *(w(1)**2 - w(t)**2) -  &
          (x(2)-x(t)) *w(1)**2 *w(3)**2 *(w(1)**2 - w(3)**2) +  &
          (x(3)-x(t)) *w(1)**2 *w(2)**2 *(w(1)**2 - w(2)**2)    

 endfunction f1_C

 function f2(t, w, x) 

   real(SP),     intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP)                  :: f2

   f2 = (x(1)-x(2)) *w(1)**2 *w(2)**2 *(w(3)**4 - w(t)**4) -  &
        (x(1)-x(3)) *w(1)**2 *w(3)**2 *(w(2)**4 - w(t)**4) +  &
        (x(1)-x(t)) *w(1)**2 *w(t)**2 *(w(2)**4 - w(3)**4) +  &
        (x(2)-x(3)) *w(2)**2 *w(3)**2 *(w(1)**4 - w(t)**4) -  &
        (x(2)-x(t)) *w(2)**2 *w(t)**2 *(w(1)**4 - w(3)**4) +  &
        (x(3)-x(t)) *w(3)**2 *w(t)**2 *(w(1)**4 - w(2)**4)    

 endfunction f2

 function f2_C(t, w, x) 

   complex(SP),  intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP)                  :: f2_C

   f2_C = (x(1)-x(2)) *w(1)**2 *w(2)**2 *(w(3)**4 - w(t)**4) -  &
          (x(1)-x(3)) *w(1)**2 *w(3)**2 *(w(2)**4 - w(t)**4) +  &
          (x(1)-x(t)) *w(1)**2 *w(t)**2 *(w(2)**4 - w(3)**4) +  &
          (x(2)-x(3)) *w(2)**2 *w(3)**2 *(w(1)**4 - w(t)**4) -  &
          (x(2)-x(t)) *w(2)**2 *w(t)**2 *(w(1)**4 - w(3)**4) +  &
          (x(3)-x(t)) *w(3)**2 *w(t)**2 *(w(1)**4 - w(2)**4)    

 endfunction f2_C

 function f3(t, w, x) 

   real(SP),     intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP)                  :: f3

   f3 = (x(1)-x(2)) *w(1)**4 *w(2)**4 *(w(3)**2 - w(t)**2) -  &
        (x(1)-x(3)) *w(1)**4 *w(3)**4 *(w(2)**2 - w(t)**2) +  &
        (x(1)-x(t)) *w(1)**4 *w(t)**4 *(w(2)**2 - w(3)**2) +  &
        (x(2)-x(3)) *w(2)**4 *w(3)**4 *(w(1)**2 - w(t)**2) -  &
        (x(2)-x(t)) *w(2)**4 *w(t)**4 *(w(1)**2 - w(3)**2) +  &
        (x(3)-x(t)) *w(3)**4 *w(t)**4 *(w(1)**2 - w(2)**2)    

 endfunction f3

 function f3_C(t, w, x) 

   complex(SP),  intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP)                  :: f3_C

   f3_C = (x(1)-x(2)) *w(1)**4 *w(2)**4 *(w(3)**2 - w(t)**2) -  &
          (x(1)-x(3)) *w(1)**4 *w(3)**4 *(w(2)**2 - w(t)**2) +  &
          (x(1)-x(t)) *w(1)**4 *w(t)**4 *(w(2)**2 - w(3)**2) +  &
          (x(2)-x(3)) *w(2)**4 *w(3)**4 *(w(1)**2 - w(t)**2) -  &
          (x(2)-x(t)) *w(2)**4 *w(t)**4 *(w(1)**2 - w(3)**2) +  &
          (x(3)-x(t)) *w(3)**4 *w(t)**4 *(w(1)**2 - w(2)**2)    

 endfunction f3_C

 function f4(t, w, x) 

   real(SP),     intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP)                  :: f4

   f4 = x(1) *w(1)**6 * (w(2)**2 - w(3)**2) * (w(2)**2 - w(t)**2) * (w(3)**2 - w(t)**2) - &
        x(2) *w(2)**6 * (w(1)**2 - w(3)**2) * (w(1)**2 - w(t)**2) * (w(3)**2 - w(t)**2) + &
        x(3) *w(3)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(t)**2) * (w(2)**2 - w(t)**2) - &
        x(t) *w(t)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(3)**2) * (w(2)**2 - w(3)**2)   

 endfunction f4

 function f4_C(t, w, x) 

   complex(SP),  intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP)                  :: f4_C

   f4_C = x(1) *w(1)**6 * (w(2)**2 - w(3)**2) * (w(2)**2 - w(t)**2) * (w(3)**2 - w(t)**2) - &
          x(2) *w(2)**6 * (w(1)**2 - w(3)**2) * (w(1)**2 - w(t)**2) * (w(3)**2 - w(t)**2) + &
          x(3) *w(3)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(t)**2) * (w(2)**2 - w(t)**2) - &
          x(t) *w(t)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(3)**2) * (w(2)**2 - w(3)**2)   

 endfunction f4_C
!!----------------------------------------------------------

 subroutine RQ_3p_solver_R(w, x, r, q) 

   real(SP),     intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   complex(SP),  intent(out)    :: r(3), q(3)

  ! Local variables:
   integer                 :: i, j, ii=0
   complex(SP)             :: a, b, c, d !coefficients of the q polynom
   complex(SP)             :: D1, D2, den, s = complex(1., sqrt(3.)), deR
  !--------------------

  a =  f1(4, w, x)* ( f2(5, w, x)*f3(6, w, x) - f2(6, w, x)*f3(5, w, x) ) - &
       f1(5, w, x)* ( f2(4, w, x)*f3(6, w, x) - f2(6, w, x)*f3(4, w, x) ) + &
       f1(6, w, x)* ( f2(4, w, x)*f3(5, w, x) - f2(5, w, x)*f3(4, w, x) ) 

  b = -f1(4, w, x)* ( f2(5, w, x)*f4(6, w, x) - f2(6, w, x)*f4(5, w, x) ) + &
       f1(5, w, x)* ( f2(4, w, x)*f4(6, w, x) - f2(6, w, x)*f4(4, w, x) ) - &
       f1(6, w, x)* ( f2(4, w, x)*f4(5, w, x) - f2(5, w, x)*f4(4, w, x) ) 
  
  c = -f4(4, w, x)* ( f1(5, w, x)*f3(6, w, x) - f1(6, w, x)*f3(5, w, x) ) + &
       f4(5, w, x)* ( f1(4, w, x)*f3(6, w, x) - f1(6, w, x)*f3(4, w, x) ) - &
       f4(6, w, x)* ( f1(4, w, x)*f3(5, w, x) - f1(5, w, x)*f3(4, w, x) ) 

  d = -f4(4, w, x)* ( f2(5, w, x)*f3(6, w, x) - f2(6, w, x)*f3(5, w, x) ) + &
       f4(5, w, x)* ( f2(4, w, x)*f3(6, w, x) - f2(6, w, x)*f3(4, w, x) ) - &
       f4(6, w, x)* ( f2(4, w, x)*f3(5, w, x) - f2(5, w, x)*f3(4, w, x) ) 

  D1   = 3.*a*c - b**2. 
  D2   = 2.*b*D1 + 3.*a*(b*c - 9.*a*d)
  den  = ( D2 + sqrt(4.*D1**3. + D2**2.) )**(1./3.00)

  q(1) = -( b + 2.**(1./3.)*D1/den - den/2.**(1./3.) )/(3.*a)
  q(2) = -( b - s* D1/2.**(2./3.)/den + conjg(s)* den/2.**(4./3.) )/(3.*a)
  q(3) = -( b - conjg(s)* D1/2.**(2./3.)/den + s* den/2.**(4./3.) )/(3.*a)


  deR  =  (w(1)**2-q(1)) * (w(1)**2-q(2)) * (w(2)**2-q(3)) * (w(3)**2-q(3))*   &
         ((w(2)**2-q(1)) * (w(3)**2-q(2)) - (w(2)**2-q(2)) * (w(3)**2-q(1)))   &
         +(w(1)**2-q(1)) * (w(1)**2-q(3)) * (w(2)**2-q(2)) * (w(3)**2-q(2))*   &
         ((w(2)**2-q(3)) * (w(3)**2-q(1)) - (w(2)**2-q(1)) * (w(3)**2-q(3)))   &
         +(w(1)**2-q(2)) * (w(1)**2-q(3)) * (w(2)**2-q(1)) * (w(3)**2-q(1))*   &
         ((w(2)**2-q(2)) * (w(3)**2-q(3)) - (w(2)**2-q(3)) * (w(3)**2-q(2))) 

  r(1) = (w(1)**2-q(1))*(w(2)**2-q(1))*(w(3)**2-q(1))*(                                                         &
        -(w(1)**2-q(2))*(w(1)**2-q(3))*( (w(2)**2-q(2))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(2)) )*x(1)   &
        +(w(2)**2-q(2))*(w(2)**2-q(3))*( (w(1)**2-q(2))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(2)) )*x(2)   &
        -(w(3)**2-q(2))*(w(3)**2-q(3))*( (w(1)**2-q(2))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(2)) )*x(3) ) /deR 

  r(2) = (w(1)**2-q(2))*(w(2)**2-q(2))*(w(3)**2-q(2))*(                                                         &
        +(w(1)**2-q(1))*(w(1)**2-q(3))*( (w(2)**2-q(1))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(1)) )*x(1)   &
        -(w(2)**2-q(1))*(w(2)**2-q(3))*( (w(1)**2-q(1))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(1)) )*x(2)   &
        +(w(3)**2-q(1))*(w(3)**2-q(3))*( (w(1)**2-q(1))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(1)) )*x(3) ) /deR 

  r(3) = (w(1)**2-q(3))*(w(2)**2-q(3))*(w(3)**2-q(3))*(                                                        &
        -(w(1)**2-q(1))*(w(1)**2-q(2))*( (w(2)**2-q(1))*(w(3)**2-q(2)) - (w(2)**2-q(2))*(w(3)**2-q(1)) )*x(1)  & 
        +(w(2)**2-q(1))*(w(2)**2-q(2))*( (w(1)**2-q(1))*(w(3)**2-q(2)) - (w(1)**2-q(2))*(w(3)**2-q(1)) )*x(2)  & 
        -(w(3)**2-q(1))*(w(3)**2-q(2))*( (w(1)**2-q(1))*(w(2)**2-q(2)) - (w(1)**2-q(2))*(w(2)**2-q(1)) )*x(3) ) /deR
                                                                  
 end subroutine RQ_3p_solver_R


 subroutine RQ_3p_solver_C(w, x, r, q) 

   complex(SP),  intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   complex(SP),  intent(out)    :: r(3), q(3)

  ! Local variables:
   integer                 :: i, j, ii=0
   complex(SP)             :: a, b, c, d !coefficients of the q polynom
   complex(SP)             :: D1, D2, den, s = complex(1., sqrt(3.)), deR
  !--------------------

  a =  f1_C(4, w, x)* ( f2_C(5, w, x)*f3_C(6, w, x) - f2_C(6, w, x)*f3_C(5, w, x) ) - &
       f1_C(5, w, x)* ( f2_C(4, w, x)*f3_C(6, w, x) - f2_C(6, w, x)*f3_C(4, w, x) ) + &
       f1_C(6, w, x)* ( f2_C(4, w, x)*f3_C(5, w, x) - f2_C(5, w, x)*f3_C(4, w, x) ) 

  b = -f1_C(4, w, x)* ( f2_C(5, w, x)*f4_C(6, w, x) - f2_C(6, w, x)*f4_C(5, w, x) ) + &
       f1_C(5, w, x)* ( f2_C(4, w, x)*f4_C(6, w, x) - f2_C(6, w, x)*f4_C(4, w, x) ) - &
       f1_C(6, w, x)* ( f2_C(4, w, x)*f4_C(5, w, x) - f2_C(5, w, x)*f4_C(4, w, x) ) 
  
  c = -f4_C(4, w, x)* ( f1_C(5, w, x)*f3_C(6, w, x) - f1_C(6, w, x)*f3_C(5, w, x) ) + &
       f4_C(5, w, x)* ( f1_C(4, w, x)*f3_C(6, w, x) - f1_C(6, w, x)*f3_C(4, w, x) ) - &
       f4_C(6, w, x)* ( f1_C(4, w, x)*f3_C(5, w, x) - f1_C(5, w, x)*f3_C(4, w, x) ) 

  d = -f4_C(4, w, x)* ( f2_C(5, w, x)*f3_C(6, w, x) - f2_C(6, w, x)*f3_C(5, w, x) ) + &
       f4_C(5, w, x)* ( f2_C(4, w, x)*f3_C(6, w, x) - f2_C(6, w, x)*f3_C(4, w, x) ) - &
       f4_C(6, w, x)* ( f2_C(4, w, x)*f3_C(5, w, x) - f2_C(5, w, x)*f3_C(4, w, x) ) 

  D1   = 3.*a*c - b**2. 
  D2   = 2.*b*D1 + 3.*a*(b*c - 9.*a*d)
  den  = ( D2 + sqrt(4.*D1**3. + D2**2.) )**(1./3.00)

  q(1) = -( b + 2.**(1./3.)*D1/den - den/2.**(1./3.) )/(3.*a)              
  q(2) = -( b - s* D1/2.**(2./3.)/den + conjg(s)* den/2.**(4./3.) )/(3.*a)  
  q(3) = -( b - conjg(s)* D1/2.**(2./3.)/den + s* den/2.**(4./3.) )/(3.*a) 


  deR  =  (w(1)**2-q(1)) * (w(1)**2-q(2)) * (w(2)**2-q(3)) * (w(3)**2-q(3))*   &
         ((w(2)**2-q(1)) * (w(3)**2-q(2)) - (w(2)**2-q(2)) * (w(3)**2-q(1)))   &
         +(w(1)**2-q(1)) * (w(1)**2-q(3)) * (w(2)**2-q(2)) * (w(3)**2-q(2))*   &
         ((w(2)**2-q(3)) * (w(3)**2-q(1)) - (w(2)**2-q(1)) * (w(3)**2-q(3)))   &
         +(w(1)**2-q(2)) * (w(1)**2-q(3)) * (w(2)**2-q(1)) * (w(3)**2-q(1))*   &
         ((w(2)**2-q(2)) * (w(3)**2-q(3)) - (w(2)**2-q(3)) * (w(3)**2-q(2))) 


  r(1) = (w(1)**2-q(1))*(w(2)**2-q(1))*(w(3)**2-q(1))*(                                                         &
        -(w(1)**2-q(2))*(w(1)**2-q(3))*( (w(2)**2-q(2))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(2)) )*x(1)   &
        +(w(2)**2-q(2))*(w(2)**2-q(3))*( (w(1)**2-q(2))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(2)) )*x(2)   &
        -(w(3)**2-q(2))*(w(3)**2-q(3))*( (w(1)**2-q(2))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(2)) )*x(3) ) /deR 

  r(2) = (w(1)**2-q(2))*(w(2)**2-q(2))*(w(3)**2-q(2))*(                                                         &
        +(w(1)**2-q(1))*(w(1)**2-q(3))*( (w(2)**2-q(1))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(1)) )*x(1)   &
        -(w(2)**2-q(1))*(w(2)**2-q(3))*( (w(1)**2-q(1))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(1)) )*x(2)   &
        +(w(3)**2-q(1))*(w(3)**2-q(3))*( (w(1)**2-q(1))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(1)) )*x(3) ) /deR 

  r(3) = (w(1)**2-q(3))*(w(2)**2-q(3))*(w(3)**2-q(3))*(                                                        &
        -(w(1)**2-q(1))*(w(1)**2-q(2))*( (w(2)**2-q(1))*(w(3)**2-q(2)) - (w(2)**2-q(2))*(w(3)**2-q(1)) )*x(1)  & 
        +(w(2)**2-q(1))*(w(2)**2-q(2))*( (w(1)**2-q(1))*(w(3)**2-q(2)) - (w(1)**2-q(2))*(w(3)**2-q(1)) )*x(2)  & 
        -(w(3)**2-q(1))*(w(3)**2-q(2))*( (w(1)**2-q(1))*(w(2)**2-q(2)) - (w(1)**2-q(2))*(w(2)**2-q(1)) )*x(3) ) /deR
                                                                  
 end subroutine RQ_3p_solver_C


 subroutine RQ_3p_solver_I(w, x, r, q) 

   real(SP),     intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   complex(SP),  intent(out)    :: r(3), q(3)

  ! Local variables:
   integer                 :: i, j, ii=0
   complex(SP)             :: a, b, c, d !coefficients of the q polynom
   complex(SP)             :: D1, D2, den, s = complex(1., sqrt(3.)), deR
  !--------------------

  a =  f1(4, w, x)* ( f2(5, w, x)*f3(6, w, x) - f2(6, w, x)*f3(5, w, x) ) - &
       f1(5, w, x)* ( f2(4, w, x)*f3(6, w, x) - f2(6, w, x)*f3(4, w, x) ) + &
       f1(6, w, x)* ( f2(4, w, x)*f3(5, w, x) - f2(5, w, x)*f3(4, w, x) ) 

  b = -f1(4, w, x)* ( f2(5, w, x)*f4(6, w, x) - f2(6, w, x)*f4(5, w, x) ) + &
       f1(5, w, x)* ( f2(4, w, x)*f4(6, w, x) - f2(6, w, x)*f4(4, w, x) ) - &
       f1(6, w, x)* ( f2(4, w, x)*f4(5, w, x) - f2(5, w, x)*f4(4, w, x) ) 
  
  c = -f4(4, w, x)* ( f1(5, w, x)*f3(6, w, x) - f1(6, w, x)*f3(5, w, x) ) + &
       f4(5, w, x)* ( f1(4, w, x)*f3(6, w, x) - f1(6, w, x)*f3(4, w, x) ) - &
       f4(6, w, x)* ( f1(4, w, x)*f3(5, w, x) - f1(5, w, x)*f3(4, w, x) ) 

  d = -f4(4, w, x)* ( f2(5, w, x)*f3(6, w, x) - f2(6, w, x)*f3(5, w, x) ) + &
       f4(5, w, x)* ( f2(4, w, x)*f3(6, w, x) - f2(6, w, x)*f3(4, w, x) ) - &
       f4(6, w, x)* ( f2(4, w, x)*f3(5, w, x) - f2(5, w, x)*f3(4, w, x) ) 

  D1   = 3.*a*c - b**2. 
  D2   = 2.*b*D1 + 3.*a*(b*c - 9.*a*d)
  den  = ( D2 + sqrt(4.*D1**3. + D2**2.) )**(1./3.00)

  q(1) = ( b + 2.**(1./3.)*D1/den - den/2.**(1./3.) )/(3.*a)               !q(1) = -q(1)_Real, r(1) = -r(1)_Real
  q(2) = ( b - s* D1/2.**(2./3.)/den + conjg(s)* den/2.**(4./3.) )/(3.*a)  !q(2) = -q(2)_Real, r(2) = -r(2)_Real
  q(3) = ( b - conjg(s)* D1/2.**(2./3.)/den + s* den/2.**(4./3.) )/(3.*a)  !q(3) = -q(3)_Real, r(3) = -r(3)_Real


  deR  =  (w(1)**2-q(1)) * (w(1)**2-q(2)) * (w(2)**2-q(3)) * (w(3)**2-q(3))*   &
         ((w(2)**2-q(1)) * (w(3)**2-q(2)) - (w(2)**2-q(2)) * (w(3)**2-q(1)))   &
         +(w(1)**2-q(1)) * (w(1)**2-q(3)) * (w(2)**2-q(2)) * (w(3)**2-q(2))*   &
         ((w(2)**2-q(3)) * (w(3)**2-q(1)) - (w(2)**2-q(1)) * (w(3)**2-q(3)))   &
         +(w(1)**2-q(2)) * (w(1)**2-q(3)) * (w(2)**2-q(1)) * (w(3)**2-q(1))*   &
         ((w(2)**2-q(2)) * (w(3)**2-q(3)) - (w(2)**2-q(3)) * (w(3)**2-q(2))) 


  r(1) =-(w(1)**2-q(1))*(w(2)**2-q(1))*(w(3)**2-q(1))*(                                                         &
        -(w(1)**2-q(2))*(w(1)**2-q(3))*( (w(2)**2-q(2))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(2)) )*x(1)   &
        +(w(2)**2-q(2))*(w(2)**2-q(3))*( (w(1)**2-q(2))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(2)) )*x(2)   &
        -(w(3)**2-q(2))*(w(3)**2-q(3))*( (w(1)**2-q(2))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(2)) )*x(3) ) /deR 

  r(2) =-(w(1)**2-q(2))*(w(2)**2-q(2))*(w(3)**2-q(2))*(                                                         &
        +(w(1)**2-q(1))*(w(1)**2-q(3))*( (w(2)**2-q(1))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(1)) )*x(1)   &
        -(w(2)**2-q(1))*(w(2)**2-q(3))*( (w(1)**2-q(1))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(1)) )*x(2)   &
        +(w(3)**2-q(1))*(w(3)**2-q(3))*( (w(1)**2-q(1))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(1)) )*x(3) ) /deR 

  r(3) =-(w(1)**2-q(3))*(w(2)**2-q(3))*(w(3)**2-q(3))*(                                                        &
        -(w(1)**2-q(1))*(w(1)**2-q(2))*( (w(2)**2-q(1))*(w(3)**2-q(2)) - (w(2)**2-q(2))*(w(3)**2-q(1)) )*x(1)  & 
        +(w(2)**2-q(1))*(w(2)**2-q(2))*( (w(1)**2-q(1))*(w(3)**2-q(2)) - (w(1)**2-q(2))*(w(3)**2-q(1)) )*x(2)  & 
        -(w(3)**2-q(1))*(w(3)**2-q(2))*( (w(1)**2-q(1))*(w(2)**2-q(2)) - (w(1)**2-q(2))*(w(2)**2-q(1)) )*x(3) ) /deR
                                                                  
 end subroutine RQ_3p_solver_I

!!----------------------------------------------------------

END MODULE mod_exact_3p