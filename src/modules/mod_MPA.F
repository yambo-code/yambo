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

 public :: mpa_RE_solver, err_func_np

contains

 subroutine mpa_RQ_1p_solver(w, x, R, Q) 
   ! analytical form of the 1 pole solution
   implicit none
   !
   complex(DP),  intent(in)     :: w(2)
   complex(DP),  intent(in)     :: x(2)
   complex(DP),  intent(out)    :: R, Q
   ! 
   R =  x(1)*x(2)* (w(1)**2- w(2)**2 )/  (x(1) - x(2))
   Q = (x(1)*w(1)**2  -  x(2)*w(2)**2 )/ (x(1) - x(2))
   !
 end subroutine mpa_RQ_1p_solver
!-------------------------------------------------------------------------------

 subroutine mpa_RQ_2p_solver(w, x, r, q) 
   implicit none
   !
   complex(DP),  intent(in)     :: w(4)
   complex(DP),  intent(in)     :: x(4)
   complex(DP),  intent(out)    :: r(2), q(2)
   !
   integer     :: i, j
   complex(DP) :: a, b, c !coefficients of the q polynom
   complex(DP) :: den, det
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
   det  = b**2 -4._DP*a*c
   q(1) = -(b + sqrt(det))/(2._DP*a)
   q(2) = -(b - sqrt(det))/(2._DP*a)
   ! 
   den  =  (q(1)-q(2))*(w(2)-w(3))*(w(2)+w(3))
   r(1) = -(q(1) - w(2)**2)*(q(1) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(2)*(x(2) - x(3)) ) /den
   r(2) =  (q(2) - w(2)**2)*(q(2) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(1)*(x(2) - x(3)) ) /den
   !
 end subroutine mpa_RQ_2p_solver
!-------------------------------------------------------------------------------

 function f1(t, w, x) 
   implicit none
   complex(DP),  intent(in)     :: w(6)
   complex(DP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(DP) :: f1
   !
   f1 = (x(1)-x(2)) *w(3)**2 *w(t)**2 *(w(3)**2 - w(t)**2) -  &
        (x(1)-x(3)) *w(2)**2 *w(t)**2 *(w(2)**2 - w(t)**2) +  &
        (x(1)-x(t)) *w(2)**2 *w(3)**2 *(w(2)**2 - w(3)**2) +  &
        (x(2)-x(3)) *w(1)**2 *w(t)**2 *(w(1)**2 - w(t)**2) -  &
        (x(2)-x(t)) *w(1)**2 *w(3)**2 *(w(1)**2 - w(3)**2) +  &
        (x(3)-x(t)) *w(1)**2 *w(2)**2 *(w(1)**2 - w(2)**2)    
 end function

 function f2(t, w, x) 
   implicit none
   complex(DP),  intent(in)     :: w(6)
   complex(DP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(DP) :: f2
   !
   f2 = (x(1)-x(2)) *w(1)**2 *w(2)**2 *(w(3)**4 - w(t)**4) -  &
        (x(1)-x(3)) *w(1)**2 *w(3)**2 *(w(2)**4 - w(t)**4) +  &
        (x(1)-x(t)) *w(1)**2 *w(t)**2 *(w(2)**4 - w(3)**4) +  &
        (x(2)-x(3)) *w(2)**2 *w(3)**2 *(w(1)**4 - w(t)**4) -  &
        (x(2)-x(t)) *w(2)**2 *w(t)**2 *(w(1)**4 - w(3)**4) +  &
        (x(3)-x(t)) *w(3)**2 *w(t)**2 *(w(1)**4 - w(2)**4)    
 end function

 function f3(t, w, x) 
   implicit none
   complex(DP),  intent(in)     :: w(6)
   complex(DP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(DP) :: f3
   !
   f3 = (x(1)-x(2)) *w(1)**4 *w(2)**4 *(w(3)**2 - w(t)**2) -  &
        (x(1)-x(3)) *w(1)**4 *w(3)**4 *(w(2)**2 - w(t)**2) +  &
        (x(1)-x(t)) *w(1)**4 *w(t)**4 *(w(2)**2 - w(3)**2) +  &
        (x(2)-x(3)) *w(2)**4 *w(3)**4 *(w(1)**2 - w(t)**2) -  &
        (x(2)-x(t)) *w(2)**4 *w(t)**4 *(w(1)**2 - w(3)**2) +  &
        (x(3)-x(t)) *w(3)**4 *w(t)**4 *(w(1)**2 - w(2)**2)    
 end function

 function f4(t, w, x) 
   implicit none
   complex(DP),  intent(in)     :: w(6)
   complex(DP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(DP)                  :: f4
   !
   f4 = x(1) *w(1)**6 * (w(2)**2 - w(3)**2) * (w(2)**2 - w(t)**2) * (w(3)**2 - w(t)**2) - &
        x(2) *w(2)**6 * (w(1)**2 - w(3)**2) * (w(1)**2 - w(t)**2) * (w(3)**2 - w(t)**2) + &
        x(3) *w(3)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(t)**2) * (w(2)**2 - w(t)**2) - &
        x(t) *w(t)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(3)**2) * (w(2)**2 - w(3)**2)   
 end function

 subroutine mpa_RQ_3p_solver(w, x, r, q) 
   implicit none
   complex(DP),  intent(in)     :: w(6)
   complex(DP),  intent(in)     :: x(6)
   complex(DP),  intent(out)    :: r(3), q(3)
   !
   integer     :: i, j
   complex(DP) :: a, b, c, d !coefficients of the q polynom
   complex(DP) :: D1, D2, den, deR 
   real(DP)    :: t=0.333333333333334_DP 
   complex(DP) :: s=(1._DP, 1.73205080757_DP) ! = (1._DP, sqrt(3._DP))
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
   D1   = 3._DP*a*c - b**2
   D2   = 2._DP*b*D1 + 3._DP*a*(b*c - 9._DP*a*d)
   den  = ( D2 + sqrt(4.0_DP*D1**3 + D2**2) )**(t)
   !
   q(1) = -( b + 2.0_DP**(t)*D1/den - den/2._DP**(t) )/(3.0_DP*a)
   q(2) = -( b - s* D1/2._DP**(2._DP*t)/den + conjg(s)* den/2._DP**(4._DP*t) )/(3._DP*a)
   q(3) = -( b - conjg(s)* D1/2._DP**(2._DP*t)/den + s* den/2._DP**(4._DP*t) )/(3._DP*a)
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
 end subroutine mpa_RQ_3p_solver
!-------------------------------------------------------------------------------

 subroutine mpa_R_solver(np, w, xq, R, Q)
   implicit none
   integer,      intent(in)        :: np
   complex(DP),  intent(in)        :: w(2*np)
   complex(DP),  intent(in)        :: xq(2*np), Q(np)
   complex(DP),  intent(out)       :: R(np)
   ! 
   complex(DP)                     :: A(np,np)
   integer                         :: i, k, info, ind(np)    
   ! 
   do k = 1, np
     R(k) = xq(k)
     do i = 1, np
       A(k,i) = -1._DP/( w(k)**2 - Q(i) )
     enddo
   enddo 
   ! 
#ifdef _DOUBLE
   call zgesv(np, 1, A, np, ind, R, np,info) 
#else
   call cgesv(np, 1, A, np, ind, R, np,info) 
#endif
   ! 
 end subroutine mpa_R_solver

 subroutine mpa_Q_solver(np, z, xq, Q)
   implicit none
   integer,      intent(in)        :: np
   complex(DP),  intent(in)        :: z(2*np)
   complex(DP),  intent(in)        :: xq(2*np)
   complex(DP),  intent(out)       :: Q(np)
   ! 
   complex(DP)                     :: Z1(np,np), Z2(np,np), M1(np,np), M2(np,np), v1(np)
   complex(DP)                     :: M(np,np),Mp(np,np), b(np), Companion(np,np)
   complex(DP)                     :: rwork(2*np), work(2*np), VR(np), VL(np)
   integer                         :: i, j, info, ind(np)    
   ! 
   do j = 1, np
     ind(j) = j
!     v1(j)  = xq(2*j-1)*z(2*j-1)**np
!     b (j)  = xq(2*j  )*z(2*j  )**np
     v1(j)  = xq(j   )*z(j   )**np
     b (j)  = xq(j+np)*z(j+np)**np
     Z1(j,1) = 1._DP
     Z2(j,1) = 1._DP  
!     M1(j,1) = -xq(2*j-1)
!     M2(j,1) = -xq(2*j) 
     M1(j,1) = -xq(j)
     M2(j,1) = -xq(j+np) 
     M (j,j) = 1._DP
     M (j,1) = 0._DP
     do i = 2, np
!       Z1(j,i) =  z(2*j-1)**(i-1)
!       Z2(j,i) =  z(2*j  )**(i-1)
!       M1(j,i) = -xq(2*j-1)*Z1(j,i)
!       M2(j,i) = -xq(2*j  )*Z2(j,i)
       Z1(j,i) =  z(j   )**(i-1)
       Z2(j,i) =  z(j+np)**(i-1)
       M1(j,i) = -xq(j   )*Z1(j,i)
       M2(j,i) = -xq(j+np)*Z2(j,i)
       if(i/=j) M(j,i) = 0._DP 
     enddo
   enddo
   M (1,1) = 1._DP
#ifdef _DOUBLE
   call zgesv(np, np, Z1, np, ind, M, np,info) 
#else
   call cgesv(np, np, Z1, np, ind, M, np,info) 
#endif
   M  = matmul(Z2,M)
   M2 = matmul(M,M1) - M2
   b  = matmul(M,v1) - b
   ! 
   M = 0.0_DP
   do i =1,np
     M(i,i) = 1.0_DP
   enddo
#ifdef _DOUBLE
   call zgesv(np, np, M2, np, ind, M, np,info) 
#else
   call cgesv(np, np, M2, np, ind, M, np,info) 
#endif
   b = matmul(M,b) 
   Companion=0._DP
   Companion(1,np) = -b(1)
   do i = 2, np
     Companion(i,np)  = -b(i)
     Companion(i,i-1) = 1._DP
   enddo
#ifdef _DOUBLE
   call  zgeev( 'N', 'N', np, Companion, np, Q, VL, 1, VR, 1, work, 2*np, rwork, info )
#else
   call  cgeev( 'N', 'N', np, Companion, np, Q, VL, 1, VR, 1, work, 2*np, rwork, info )
#endif
 end subroutine mpa_Q_solver
!-------------------------------------------------------------------------------

 subroutine mpa_RE_solver(np, w, x, R, E) 
   implicit none
   !
   integer,      intent(in)     :: np
   complex(DP),  intent(in)     :: w(2*np), x(2*np)
   complex(DP),  intent(out)    :: R(np), E(np)
   ! 
   integer      :: i
   !  
   select case(np)
     case(1)
       call mpa_RQ_1p_solver(w, x, R(1), E(1))
     case(2)
       call mpa_RQ_2p_solver(w, x, R, E)
     case(3)
       call mpa_RQ_3p_solver(w, x, R, E)
     case default
       call mpa_Q_solver(np, w, x, E)
       call mpa_R_solver(np, w, x, R, E)
   end select
   !
   do i=1,np
     E(i) = sqrt(E(i))
     if(real(E(i),DP)<0._DP) then 
       E(i) = -E(i)
     endif
     !R(i) = -R(i)/(2._DP*E(i))
     if(abs(E(i))>epsilon(1._DP)) R(i) = -R(i)/(2._DP*E(i))
     !if(isnan(realpart(R(i))) .or. isnan(imagpart(R(i))) ) then
     !  E= (1._SP,0._SP)-(0._SP,1._SP)*epsilon(1._SP)
     !  R= x(1)/(2._SP*np)
     !endif
   enddo
   !
 end subroutine mpa_RE_solver
!------------------------------------------------------------------------------- 

 function err_func_np(np, R, E, w, x)
   implicit none
   !
   integer ,    intent(in)  :: np
   complex(DP), intent(in)  :: x(2*np), w(2*np)
   complex(DP), intent(in)  :: R(np), E(np)
   complex(DP)              :: aux
   real(DP)                 :: err_func_np
   integer   ::i,j
   !      
   err_func_np = 0._DP                                                    
   do j=1, 2*np
     aux = (0._DP,0._DP)
     do i=1, np
     aux = aux + 2*E(i)*R(i)/(w(j)**2-E(i)**2)     
!     aux = aux + R(i)/(w(j)-E(i)) - R(i)/(w(j)+E(i))         
     enddo 
     err_func_np = err_func_np + abs( aux - x(j) )**2
   enddo  
   err_func_np = sqrt(err_func_np)
 endfunction err_func_np
!------------------------------------------------------------------------------- 

 function err_funcQ_np(np, R, Q, w, x)
   implicit none
   !
   integer ,    intent(in)  :: np
   complex(DP), intent(in)  :: x(2*np), w(2*np)
   complex(DP), intent(in)  :: R(np), Q(np)
   complex(DP)              :: aux
   real(DP)                 :: err_funcQ_np
   integer   ::i,j
   !
   aux = (0._DP,0._DP)      
   err_funcQ_np = 0._DP                                                    
   do j=1, 2*np
     do i=1, np
     aux = aux + R(i)/(w(j)**2-Q(i))             
     enddo 
     err_funcQ_np = err_funcQ_np + abs( aux - x(j) )**2
   enddo  
   err_funcQ_np = sqrt(err_funcQ_np)
 endfunction err_funcQ_np
!------------------------------------------------------------------------------- 

end module mpa_m

