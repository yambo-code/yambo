!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DALV, AF
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
!   2) Form of one pole: -P/(w**2-Q) = 2*E*R/(w**2-E**2)
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
 use pars,            ONLY:SP,DP,cZERO,cI
 use functions,       ONLY:NAN
 !
#include <memory.h>

 complex(SP), allocatable :: MPA_E_mat(:,:,:)
 complex(SP), allocatable :: MPA_R_mat(:,:,:)
 complex(SP), allocatable :: MPA_freqs(:)

 public :: mpa_RE_solver,err_func_np
 public :: MPA_alloc,MPA_free

contains

 subroutine MPA_alloc(d)
   implicit none
   integer :: d(4)
   call MPA_free()
   YAMBO_ALLOC(MPA_E_mat,(d(1),d(2),d(3)))
   YAMBO_ALLOC(MPA_R_mat,(d(1),d(2),d(3)))
   YAMBO_ALLOC(MPA_freqs,(d(4)))
 end subroutine

 subroutine MPA_free()
   YAMBO_FREE(MPA_E_mat)
   YAMBO_FREE(MPA_R_mat)
   YAMBO_FREE(MPA_freqs)
 end subroutine
 !
 subroutine mpa_cond1(z, E)
   !
   complex(SP),  intent(in)     :: z(2)
   complex(SP),  intent(inout)  :: E
   ! 
   if(abs(E)<10*epsilon(1._SP)) then
     E=abs(z(2))-cI*epsilon(1._SP)   
   elseif(real(E)>0._SP) then     
     E = sqrt(E)
   else
     E = sqrt(-E)
   endif
   if(real(E)<0._SP) E=-E
   if(aimag(E)>-epsilon(1._SP)) E=real(E,SP)-cI*epsilon(1._SP)
   !
 end subroutine mpa_cond1
 !
 subroutine mpa_cond(np, z, E)
   !
   integer,  intent(in)         :: np
   complex(SP),  intent(in)     :: z(2*np)
   complex(SP),  intent(inout)  :: E(np)
   !
   integer     :: i, j
   ! 
   if(abs(E(1))<10*epsilon(1._SP)) then 
     E(1) = 0.5*abs(z(2))-cI*epsilon(1._SP)
   elseif(real(E(1))>0._SP) then
     E(1) = sqrt(E(1))
   else
     E(1) = sqrt(-E(1))
   endif
   if(real(E(1))<0._SP) E(1)=-E(1)
   if(aimag(E(1))>-epsilon(1._SP)) E(1)=real(E(1),SP)-cI*epsilon(1._SP)
   !
   do i=2,np
     if(abs(E(i))<10*epsilon(1._SP)) then 
       E(i) = abs(z(i))-cI*epsilon(1._SP)
     elseif(real(E(i))>0._SP) then
       E(i) = sqrt(E(i))
     else
       E(i) = sqrt(-E(i))
     endif
     if(real(E(i))<0._SP) E(i)=-E(i)
     if(aimag(E(i))>-epsilon(1._SP)) E(i)=real(E(i),SP)-cI*epsilon(1._SP)
     !
     do j=2,i-1
       if(abs(real(E(i),SP)-real(E(j),SP))<0.0001_SP) then
         E(i)=0.5*(real(E(j))+real(E(j-1)))-cI*epsilon(1._SP)
         !exit
       endif
     enddo
   enddo
   !
 end subroutine mpa_cond
 !
 subroutine mpa_RE_1p_solver(z, x, R, E) 
   !
   ! analytical form of the 1 pole solution
   !
   implicit none
   !
   complex(SP),  intent(in)     :: z(2)
   complex(SP),  intent(in)     :: x(2)
   complex(SP),  intent(out)    :: R, E
   ! 
!----General cond------------------------------------------------
   E = (x(1)*z(1)**2 -x(2)*z(2)**2 )/(x(1)-x(2))
!   if(abs(E)<10*epsilon(1._SP)) then
!     E=1._SP-cI*epsilon(1._SP)   
!   elseif(real(E)>0._SP) then     
!     E = sqrt(E)
!   else
!     E = sqrt(-E)
!   endif
!   if(real(E)<0._SP) E=-E
!   if(aimag(E)>-epsilon(1._SP)) E=real(E,SP)-cI*epsilon(1._SP)
   call mpa_cond1(z, E)
   !R = x(1)*(z(1)**2-E**2)/(2._SP*E) 
   R = x(2)*(z(2)**2-E**2)/(2._SP*E) 
   ! 
!----PPA_cond------------------------------------------------
!   if(real(x(1)/x(2))>1._SP) then        
!     E = sqrt((x(1)*z(1)**2 -x(2)*z(2)**2 )/(x(1)-x(2)))
!     if(real(E)<0._SP) E=-E
!     if(aimag(E)>0._SP) E=real(E,SP)-cI*epsilon(1._SP)
!     R = x(1)*(z(1)**2-E**2)/(2._SP*E) 
!   else
!     E = abs(z(1)+z(2))*0.5_SP-cI*epsilon(1._SP)
!     R = x(2)*(z(2)**2-E**2)/(2._SP*E) 
!   endif
   ! 
!--------------------------------------------------------
   !
 end subroutine mpa_RE_1p_solver
 !
 !
 subroutine mpa_RQ_2p_solver(w, x, r, q) 
   implicit none
   !
   complex(SP),  intent(in)     :: w(4)
   complex(SP),  intent(in)     :: x(4)
   complex(SP),  intent(out)    :: r(2), q(2)
   !
   integer     :: i, j
   complex(SP) :: a, b, c !coefficients of the q polynomial
   complex(SP) :: den, det!, w(4)
   !
   !w=real(waux,SP)
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
   q(1) = -(b + sqrt(det))/2._SP!*a)
   q(2) = -(b - sqrt(det))/2._SP!*a)
   ! 
   if(abs(a)>epsilon(1._SP)) then
     q(1)=q(1)/a
     q(2)=q(2)/a
   endif
   ! 
   den  =  (q(1)-q(2))*(w(2)-w(3))*(w(2)+w(3))
   r(1) = -(q(1) - w(2)**2)*(q(1) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(2)*(x(2) - x(3)) ) !/den
   r(2) =  (q(2) - w(2)**2)*(q(2) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(1)*(x(2) - x(3)) ) !/den
   !
   if(abs(den)>epsilon(1._SP)) then
     r(1)=r(1)/den
     r(2)=r(2)/den
   endif
   !   
 end subroutine mpa_RQ_2p_solver
 !
 !
 function f1(t, w, x) 
   implicit none
   complex(SP),  intent(in)     :: w(6)
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
 !
 !
 function f2(t, w, x) 
   implicit none
   complex(SP),  intent(in)     :: w(6)
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
 !
 !
 function f3(t, w, x) 
   implicit none
   complex(SP),  intent(in)     :: w(6)
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
 !
 !
 function f4(t, w, x) 
   implicit none
   complex(SP),  intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   integer,      intent(in)     :: t != 4,5,6
   complex(SP)                  :: f4
   !
   f4 = x(1) *w(1)**6 * (w(2)**2 - w(3)**2) * (w(2)**2 - w(t)**2) * (w(3)**2 - w(t)**2) - &
        x(2) *w(2)**6 * (w(1)**2 - w(3)**2) * (w(1)**2 - w(t)**2) * (w(3)**2 - w(t)**2) + &
        x(3) *w(3)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(t)**2) * (w(2)**2 - w(t)**2) - &
        x(t) *w(t)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(3)**2) * (w(2)**2 - w(3)**2)   
 end function
 !
 !
 subroutine mpa_RQ_3p_solver(w, x, r, q) 
   implicit none
   complex(SP),  intent(in)     :: w(6)
   complex(SP),  intent(in)     :: x(6)
   complex(SP),  intent(out)    :: r(3), q(3)
   !
   integer     :: i, j
   complex(SP) :: a, b, c, d !coefficients of the q polynomial
   complex(SP) :: D1, D2, den, deR 
   real(SP)    :: t=0.33333333333333334_SP 
   complex(SP) :: s=(1._SP, 1.7320508075688772_SP) ! = (1._SP, sqrt(3._SP))
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
   den  = ( D2 + sqrt(4.0_SP*D1**3 + D2**2) )**(t)
   !
   q(1) = -( den*(b- den/2._SP**(t)) + 2.0_SP**(t)*D1)/3.0_SP!*den*a)
   q(2) = -( den*(b+ conjg(s)* den/2._SP**(4._SP*t)) - s* D1/2._SP**(2._SP*t)  )/3._SP!*den*a)
   q(3) = -( den*(b+ s*den/2._SP**(4._SP*t)) - conjg(s)* D1/2._SP**(2._SP*t)  )/3._SP!*den*a)
   !
   if(abs(den*a)>epsilon(1._SP)) then
     q(1)=q(1)/(den*a)
     q(2)=q(2)/(den*a)
     q(3)=q(3)/(den*a)
   endif
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
         -(w(3)**2-q(2))*(w(3)**2-q(3))*( (w(1)**2-q(2))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(2)) )*x(3) ) !/deR 
   !
   r(2) = (w(1)**2-q(2))*(w(2)**2-q(2))*(w(3)**2-q(2))*(                                                         &
         +(w(1)**2-q(1))*(w(1)**2-q(3))*( (w(2)**2-q(1))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(1)) )*x(1)   &
         -(w(2)**2-q(1))*(w(2)**2-q(3))*( (w(1)**2-q(1))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(1)) )*x(2)   &
         +(w(3)**2-q(1))*(w(3)**2-q(3))*( (w(1)**2-q(1))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(1)) )*x(3) ) !/deR 
   !
   r(3) = (w(1)**2-q(3))*(w(2)**2-q(3))*(w(3)**2-q(3))*(                                                        &
         -(w(1)**2-q(1))*(w(1)**2-q(2))*( (w(2)**2-q(1))*(w(3)**2-q(2)) - (w(2)**2-q(2))*(w(3)**2-q(1)) )*x(1)  & 
         +(w(2)**2-q(1))*(w(2)**2-q(2))*( (w(1)**2-q(1))*(w(3)**2-q(2)) - (w(1)**2-q(2))*(w(3)**2-q(1)) )*x(2)  & 
         -(w(3)**2-q(1))*(w(3)**2-q(2))*( (w(1)**2-q(1))*(w(2)**2-q(2)) - (w(1)**2-q(2))*(w(2)**2-q(1)) )*x(3) ) !/deR
   ! 
   if(abs(deR)>epsilon(1._SP)) then
     r(1)=r(1)/deR
     r(2)=r(2)/deR
     r(3)=r(3)/deR
   endif
   !                                                                
 end subroutine mpa_RQ_3p_solver
 !
 !
 subroutine mpa_R_solver(np, w, xq, R, E, rcond)
   implicit none
   integer,      intent(in)        :: np
   complex(SP),  intent(in)        :: w(2*np)
   complex(SP),  intent(in)        :: xq(2*np), E(np)
   complex(SP),  intent(out)       :: R(np)
   real(SP),     intent(out)       :: rcond
   ! 
   complex(SP)                     :: A(np,np)
   real(SP)                        :: anorm
   integer                         :: i, k, info, ind(np)    
   complex(SP)                     :: rwork(2*np), work(2*np)
   ! 
   anorm=abs(2._SP*E(1)/(w(1)**2 -E(1)**2))
   do k = 1, np
     R(k) = xq(k)
     do i = 1, np
       A(k,i) = 2._SP*E(i)/(w(k)**2 -E(i)**2)
       if(abs(A(k,i))>anorm) anorm=abs(A(k,i))
     enddo
   enddo 
   ! 
#ifdef _DOUBLE
   call zgecon('I', np, A, np, anorm, rcond, work, rwork, info)
   call zgesv(np, 1, A, np, ind, R, np, info) 
#else
   call cgecon('I', np, A, np, anorm, rcond, work, rwork, info)
   call cgesv(np, 1, A, np, ind, R, np, info) 
#endif
   ! 
 end subroutine mpa_R_solver
 ! 
 subroutine mpa_E_solver(np, z, xq, E, rcond)
   implicit none
   integer,      intent(in)        :: np
   complex(SP),  intent(in)        :: z(2*np)
   complex(SP),  intent(in)        :: xq(2*np)
   complex(SP),  intent(out)       :: E(np)
   real(SP),     intent(out)       :: rcond(2)
   ! 
   complex(SP)                     :: Z1(np,np), Z2(np,np), M1(np,np), M2(np,np), v1(np)
   complex(SP)                     :: M(np,np), b(np), Companion(np,np)
   complex(SP)                     :: rwork(2*np),work(2*np),VR(np),VL(np),S(np),RCE(np,np),RCV(np,np),ABNRM
   integer                         :: i, j, info, ind(np), ILO,IHI    
   real(SP)                        :: anorm, Wm
   ! 
   Wm = maxval(abs(z),np)
   !    
   do j = 1, np
     ind(j) = j
     v1(j)  = xq(j   )*z(j   )**np 
     b (j)  = xq(j+np)*z(j+np)**np 
     Z1(j,1) = 1._SP
     Z2(j,1) = 1._SP  
     M1(j,1) = -xq(j) 
     M2(j,1) = -xq(j+np) 
     M (j,1) = 0._SP
     M (j,j) = 1._SP
     do i = 2, np
       Z1(j,i) =  (z(j)/Wm)**(i-1)
       Z2(j,i) =  (z(j+np)/Wm)**(i-1)
       M1(j,i) = -xq(j   )*Z1(j,i)
       M2(j,i) = -xq(j+np)*Z2(j,i)
       if(i/=j) M(j,i) = 0._SP 
     enddo
   enddo
   !
   anorm=abs(Z1(1,1))
   do j = 1, np
     do i = 1, np
       if(abs(Z1(j,i))>anorm) anorm=abs(Z1(j,i))
     enddo
   enddo
   !
#ifdef _DOUBLE
   call zgecon('I', np, M2, np, anorm, rcond(1), work, rwork, info)
   call zgesv(np, np, Z1, np, ind, M, np,info) 
#else
   call cgecon('I', np, M2, np, anorm, rcond(1), work, rwork, info)
   call cgesv(np, np, Z1, np, ind, M, np,info) 
#endif
   M  = matmul(Z2,M)
   M2 = matmul(M,M1) - M2
   b  = matmul(M,v1) - b
   ! 
   M = 0.0_SP
   do i =1,np
     M(i,i) = 1.0_SP
   enddo
   !
   anorm=abs(M2(1,1))
   do j = 1, np
     do i = 1, np
       if(abs(M2(j,i))>anorm) anorm=abs(M2(j,i))
     enddo
   enddo
   !
#ifdef _DOUBLE
   call zgecon('I', np, M2, np, anorm, rcond(2), work, rwork, info)
   call zgesv(np, 1, M2, np, ind, b, np, info)
#else
   call cgecon('I', np, Companion, np, anorm, rcond, work, rwork, info)
   call cgesv(np, 1, M2, np, ind, b, np, info)
#endif
   ! 
   Companion=0._SP
   Companion(1,np) = -b(1)
   do i = 2, np
     Companion(i,np)  = -b(i)/Wm**(i-1)
     Companion(i,i-1) = 1._SP
   enddo
   !
#ifdef _DOUBLE
   call zgeev( 'N', 'N', np, Companion, np, E, VL, 1, VR, 1, work, 2*np, rwork, info )
   !call zgeevx('N','N','N','N',np,Companion,np,Q,VL,1,VR,1,ILO,IHI,S,ABNRM,RCE,RCV,work,2*np,rwork,info)
#else
   call cgeev( 'N', 'N', np, Companion, np, E, VL, 1, VR, 1, work, 2*np, rwork, info )
   !call cgeevx('P','N','N','N',np,Companion,np,Q,VL,1,VR,1,ILO,IHI,S,10._SP,RCE,RCV,work,2*np,rwork,info)
#endif
   ! 
   ! here we need to force real(E) to be positive.
   ! This is because of the way the residue integral is performed, later.
   ! 
   call mpa_cond(np, z, E)
   ! 
 end subroutine mpa_E_solver
 !
 !
 subroutine mpa_RE_solver(np, w, x, R, E,edigits,rdigits) ! version 2.1
   implicit none
   !
   integer,      intent(in)     :: np
   complex(SP),  intent(in)     :: w(2*np), x(2*np)
   complex(SP),  intent(out)    :: R(np), E(np)
   real(SP),     intent(out)    :: edigits(2),rdigits
   ! 
   select case(np)
   case(1)
     call mpa_RE_1p_solver(w, x, R(1), E(1))
   !case(2)
   !  call mpa_RQ_2p_solver(w, x, R, E)
   !case(3)
   !  call mpa_RQ_3p_solver(w, x, R, E)
   case default
     !
     call mpa_E_solver(np, w**2, x, E, edigits)
     call mpa_R_solver(np, w, x, R, E, rdigits)
     edigits=log10(1._SP/edigits)
     rdigits=log10(1._SP/rdigits)
     !
   end select
   ! 
 end subroutine mpa_RE_solver
 !
 !
 function err_func_np(np, R, E, w, x)
   implicit none
   !
   integer ,    intent(in)  :: np
   complex(SP), intent(in)  :: x(2*np), w(2*np)
   complex(SP), intent(in)  :: R(np), E(np)
   complex(SP)              :: aux
   real(SP)                 :: err_func_np
   integer   ::i,j
   !      
   err_func_np = 0._SP                                                    
   do j=1, 2*np
     aux = (0._SP,0._SP)
     do i=1, np
     aux = aux + 2._SP*E(i)*R(i)/(w(j)**2-E(i)**2)     
     enddo 
     err_func_np = err_func_np + abs( aux - x(j) )**2
   enddo  
   err_func_np = sqrt(err_func_np)
 endfunction err_func_np
 !
end module mpa_m

