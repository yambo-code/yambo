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
 use pars,          ONLY:SP,DP,cZERO
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

 subroutine mpa_RE_1p_solver(z, x, R, E) 
   ! analytical form of the 1 pole solution
   implicit none
   !
   complex(SP),  intent(in)     :: z(2)
   complex(SP),  intent(in)     :: x(2)
   complex(SP),  intent(out)    :: R, E
   ! 
  ! R =  (1._SP-z(1)**2/z(2)**2)/(1._SP-x(2)/x(1))
  ! if( abs(x(1)-x(2))>1._SP*epsilon(1._SP) ) then 
 !  if( real(x(1)/x(2), SP)>1._SP ) then 
 !    E = sqrt( (x(1)*z(1)**2 - x(2)*z(2)**2 )/(x(1)-x(2)) )
 !    R = -R*x(2)*z(2)/2._SP
 !    if (abs(E)>0.1_SP) R =R*z(2)/E
 !    else
       !E = abs(z(2)-z(1))-(0._SP,1._SP)*epsilon(1._SP)
       !R = E*(x(1)+x(2))*0.625_SP
 !      E=(1._SP,-0.01_SP)
  !     R=x(1)
  ! endif
   ! 
   R =  x(1)*x(2)* (z(1)**2- z(2)**2 )  !/(x(1) - x(2))
   E = (x(1)*z(1)**2  -  x(2)*z(2)**2 ) !/(x(1) - x(2))
   !
   if(abs(x(1)-x(2))>epsilon(1._SP)) then
     E=E/(x(1) - x(2))
     R=R/(x(1) - x(2))
   endif
   E = sqrt(E)
   if(abs(E)>epsilon(1._SP)) R = -R/(2._SP*E)
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
   !Wm=maxval(abs(w),np)
   ! 
   anorm=abs(2._SP*E(i)/(w(1)**2 - E(1)**2))
   do k = 1, np
     R(k) = xq(k)
     do i = 1, np
       A(k,i) = 2._SP*E(i)/(w(k)**2 - E(i)**2)
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
 subroutine mpa_Q_solver(np, z, xq, E, rcond)
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
   Wm = maxval(abs(z),2*np)
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
#ifdef _DOUBLE
   call zgeev( 'N', 'N', np, Companion, np, E, VL, 1, VR, 1, work, 2*np, rwork, info )
   !call zgeevx('N','N','N','N',np,Companion,np,Q,VL,1,VR,1,ILO,IHI,S,ABNRM,RCE,RCV,work,2*np,rwork,info)
#else
   call cgeev( 'N', 'N', np, Companion, np, E, VL, 1, VR, 1, work, 2*np, rwork, info )
   !call cgeevx('P','N','N','N',np,Companion,np,Q,VL,1,VR,1,ILO,IHI,S,10._SP,RCE,RCV,work,2*np,rwork,info)
#endif
   ! 
   do i=1,np
     E(i) = sqrt(E(i))
     !if(real(E(i),SP)<0._SP) then 
     !  E(i) = -E(i)
     !endif
!    Dario: to be tested later
    if(real(E(i),SP)*aimag(E(i))>0._SP) then 
      E(i) = cmplx( abs(real(E(i),SP)), -abs(aimag(E(i))), SP  )
    endif
    ! 
   enddo
   ! 
 end subroutine mpa_Q_solver
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
   integer      :: i
   !  
   select case(np)
    ! case(1)
    !   call mpa_RE_1p_solver(w, x, R(1), E(1))
     !case(2)
     !  call mpa_RQ_2p_solver(w, x, R, E)
     !case(3)
     !  call mpa_RQ_3p_solver(w, x, R, E)
     case default
       call mpa_Q_solver(np, w**2, x, E, edigits)
       call mpa_R_solver(np, w, x, R, E, rdigits)
       edigits=log10(1._SP/edigits)
       rdigits=log10(1._SP/rdigits)
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
 !
!======================
  function pole_interp_pade_cmplx_noLS(nx,xdata,ydata,npoles_fit,params,allow_offset) result(ierr)
  !======================

  type poly_t
    integer :: order
    complex(dp), allocatable :: c(:)
    logical :: alloc=.false.
  end type

  implicit none
  !
  ! performs interpolation on input data using pade' approx
  ! (equivalent to a sum over poles)
  ! ierr /=0 indicates a numerical problem
  !
  integer     :: nx,npoles_fit
  real(dp)    :: xdata(nx)
  complex(dp) :: ydata(nx)
  complex(dp) :: params(*)
  integer     :: ierr
  logical, optional :: allow_offset
  !
  character(25) :: subname="pole_interp_pade_cmplx_LS"
  integer   :: ndim
  integer   :: i,j,ind
  logical  :: allow_offset_
  complex(dp) :: res,ctmp(1)
  complex(dp), allocatable :: ydata_fit(:)
  complex(dp), allocatable :: aa(:), gg(:,:)
  complex(dp), allocatable :: roots(:)
  !
  integer :: p_ndeg, q_ndeg
  type(poly_t) :: p_pol, q_pol
  type(poly_t) :: p1,p2
  type(poly_t), allocatable :: A_pol(:),B_pol(:)

  ierr=0
  !
  allow_offset_=.false.
  if (present(allow_offset)) allow_offset_=allow_offset
  if (allow_offset_) call errore(subname,"offset not implemented 1",10)
  !
  p_ndeg=npoles_fit-1
  if (allow_offset_) p_ndeg=npoles_fit
  q_ndeg=npoles_fit
  !
  ! the highest order coefficient of q is equal to 1
  ! to get rid of a fictitious degree of freedom
  ! due to the cancellation of a constant coeff in the
  ! p/q ratio
  !
  ndim=p_ndeg+q_ndeg+1
  !
  if (nx/=ndim) then
    ierr=-1
    return
  endif
  !
  allocate(A_pol(0:ndim))
  allocate(B_pol(0:ndim))
  allocate(aa(ndim))
  allocate(gg(ndim,ndim))
  allocate(ydata_fit(nx))
  !
  allocate(roots(q_ndeg))
  !
  ! build gg and aa
  !
  gg=0.0
  gg(:,1) = ydata(:)
  aa(1)=gg(1,1)
  !
  do j = 2, ndim
    !
    do i = j, nx
      gg(i,j) = ( gg(j-1,j-1)-gg(i,j-1) ) / ( (xdata(i)-xdata(j-1)) * gg(i,j-1) )
    enddo
    aa(j) = gg(j,j)
    !
  enddo

  !
  ! evaluate the A_pol and B_pol coeff
  !
  call poly_reset(p1,1)
  !
  call poly_reset(A_pol(0),0)
  call poly_reset(A_pol(1),0)
  call poly_reset(B_pol(0),0)
  call poly_reset(B_pol(1),0)
  !
  A_pol(0)%c=0.0
  A_pol(1)%c=aa(1)
  B_pol(0)%c=1.0
  B_pol(1)%c=1.0
  !
  call poly_reset(p2)
  !
  do j = 2, ndim
    p1%c(0:1) = aa(j)*(/-xdata(j-1),1.0_dp/)
    call poly_mult(p2,  p1, A_pol(j-2))
    call poly_sum(A_pol(j),   A_pol(j-1), p2 )
    call poly_reset(p2)
  enddo
  !
  do j = 2, ndim
    p1%c(0:1) = aa(j)*(/-xdata(j-1),1.0_dp/)
    call poly_mult(p2,  p1, B_pol(j-2))
    call poly_sum(B_pol(j),   B_pol(j-1), p2 )
    call poly_reset(p2)
  enddo

  !
  ! eval residuals
  !
  call poly_reset(p_pol,p_ndeg)
  call poly_reset(q_pol,q_ndeg)
  !
  p_pol=A_pol(ndim)
  q_pol=B_pol(ndim)
  p_pol%c=p_pol%c/q_pol%c(npoles_fit)
  q_pol%c=q_pol%c/q_pol%c(npoles_fit)
  !
  ydata_fit(:) = poly_eval(xdata,nx,p_pol) / poly_eval(xdata,nx,q_pol)
  res = dot_product(ydata(:)-ydata_fit(:),ydata(:)-ydata_fit(:))/real(nx,dp)

  !
  ! reshape data
  !
  call poly_roots(roots,q_pol)
  !
  params(1)=0.0d0
  if (allow_offset_) params(1)=0.0  ! to be fixed
  !
  do j = 1, npoles_fit
    !
    ind = 2+(j-1)*3
    params(ind+0)=real(roots(j),dp)
    params(ind+1)=aimag(roots(j))
    !
    ctmp(1)=poly_eval(roots(j),p_pol)
    do i = 1, npoles_fit
      if (i==j) cycle
      ctmp(1)=ctmp(1)/(roots(j)-roots(i))
    enddo
    params(ind+2)=ctmp(1)
    !
  enddo
  !
  params(2+3*npoles_fit) = res

  !
  ! cleanup
  !
  call cleanup_loc()
  return

contains

  subroutine cleanup_loc()
     do i = 1, size(A_pol)
        call poly_reset(A_pol(i-1))
     enddo
     if (allocated(A_pol)) deallocate(A_pol)
     do i = 1, size(B_pol)
        call poly_reset(B_pol(i-1))
     enddo
     if (allocated(B_pol)) deallocate(B_pol)
     call poly_reset(p1)
     call poly_reset(p2)
     !
     if (allocated(ydata_fit)) deallocate(ydata_fit)
     if (allocated(roots)) deallocate(roots)
     call poly_reset(p_pol)
     call poly_reset(q_pol)
  end subroutine
  !
end function pole_interp_pade_cmplx_noLS
 !
 !**********************************************************
   SUBROUTINE zmat_sv(n, nrhs, a, b, ierr)
   !**********************************************************
   !
   !  Computes the solution of the complex system of linear equations
   !     A * X = B,
   !  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
   !
   !  The LU decomposition with partial pivoting and row interchanges is
   !  used to factor A as
   !     A = P * L * U,
   !  where P is a permutation matrix, L is unit lower triangular, and U is
   !  upper triangular.  The factored form of A is then used to solve the
   !  system of equations A * X = B.
   !
   IMPLICIT NONE
   INTEGER, INTENT(IN)         :: n, nrhs
   COMPLEX(dbl), INTENT(IN)    :: a(:,:)
   COMPLEX(dbl), INTENT(INOUT) :: b(:,:)
   INTEGER, OPTIONAL, INTENT(out) :: ierr

   INTEGER :: ierr_, info
   INTEGER,      ALLOCATABLE :: ipiv(:)
   COMPLEX(dbl), ALLOCATABLE :: atmp(:,:)

   IF ( PRESENT(ierr) ) ierr=0
   !
   IF ( n > SIZE(a,1) .OR. n > SIZE(a,2) ) CALL errore('zmat_sv','matrix A too small',1)
   IF ( n > SIZE(b,1) ) CALL errore('zmat_sv','matrix B too small (I)',1)
   IF ( nrhs > SIZE(b,2) ) CALL errore('zmat_sv','matrix B too small (II)',1)

   ALLOCATE( atmp(n,n), ipiv(n), STAT=ierr_ )
     IF (ierr_/=0) CALL errore('zmat_sv','allocating atmp, ipiv',ABS(ierr_))
   !
   ! make a local copy of a
   atmp(:,:) = a(1:n,1:n) 

   ! use magma if possible
   CALL ZGESV( n, nrhs, atmp, n, ipiv, b, SIZE(b,1), info)

   IF ( PRESENT(ierr) ) THEN
        IF (info/=0) ierr= info
   ELSE
        IF ( info < 0 ) CALL errore('zmat_sv', 'ZGESV: info illegal value', -info )
        IF ( info > 0 ) CALL errore('zmat_sv', 'ZGESV: singular matrix', info )
   ENDIF

   DEALLOCATE( atmp, ipiv, STAT=ierr_)
      IF(ierr_/=0) CALL errore('zmat_sv','deallocating atmp, ipiv',ABS(ierr_))

   RETURN
END SUBROUTINE zmat_sv
 !
  function poly_eval(x,obj) result(res)
     implicit none
     complex(dp)  :: x
     type(poly_t) :: obj
     complex(dp)  :: res
     !
     integer :: i

     res=0.0
     if (.not.obj%alloc) return
     do i = obj%order, 0, -1
       res = res + obj%c(i)*x**i
     enddo
     !
  end function poly_eval
  !
  recursive subroutine poly_reset(obj,order)
     implicit none
     type(poly_t) :: obj
     integer, optional :: order
     !
     if (present(order)) then
        if (obj%alloc) call poly_reset(obj)
        allocate(obj%c(0:order))
        obj%order=order
        obj%alloc=.true.
     else
        if (allocated(obj%c)) deallocate(obj%c) 
        obj%order=0
        obj%alloc=.false.
     endif
     !
  end subroutine
 !
end module mpa_m

