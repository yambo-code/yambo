!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): DALV,AF
!
!------------------------------------------------------------
! Multipole interpolation:
!                         - analytical solution for 1-3 poles
!                         - Linear solver for n poles*
!                         - Pade-Thiele solver for n poles*
!
! Failure condition for the position of the poles*
!
! *DA. Leon et al, PRB 104, 115157 (2021)
!
! Notes:
!
!   1) X(w) is approximated as a sum of poles
!   2) Form of one pole: -P/(w**2-Q) = 2*E*R/(w**2-E**2)
!   3) The input are two w and X(w) for each pole
!   4) The output are E and R coefficients
!   5) Use real(R), imaginary(I) or complex(C) w
!
! **The module works for scalar polarizabilities, so if 
!   one wants the solution for matrix element X(G,G',q)
!   then RQ_solver should be called for each G, G' and q.  
!------------------------------------------------------------
!
! GS forced Padé in double precision, this strongly stabilizes the MPA
!    change of parallism with Padé in SP leads to QP differences in meV range or larger
!    with Padé in DP, QP differences are within the SP error
!
module mpa_m
 !
 use pars,            ONLY:SP,DP,cZERO,cONE,cI
 use functions,       ONLY:NAN
 use matrix,          ONLY:PAR_matrix
 use gpu_m,           ONLY:have_gpu
 !
#include <y_memory.h>
#include <dev_defs.h>

 complex(SP),      allocatable, target :: MPA_freqs(:)
 type(PAR_matrix), allocatable, target :: MPA_E_par(:)
 type(PAR_matrix), allocatable, target :: MPA_R_par(:)
 complex(SP), allocatable DEV_ATTR, target :: MPA_E_par_d(:,:,:)
 complex(SP), allocatable DEV_ATTR, target :: MPA_R_par_d(:,:,:)
 !
 public :: mpa_RE_solver,err_func_X
 public :: MPA_alloc,MPA_free
 !
contains
 !
 subroutine MPA_alloc(d)
   implicit none
   integer :: d(4)
   !
   call MPA_free()
   YAMBO_ALLOC(MPA_freqs,(d(4)))
 end subroutine

 subroutine MPA_free()
   YAMBO_FREE(MPA_freqs)
 end subroutine
 !
 subroutine mpa_cond1(z, E, PPcond_rate)
   !
   complex(DP), intent(in)     :: z(2)
   complex(DP), intent(inout)  :: E
   real(DP),    intent(out)    :: PPcond_rate
   !
   PPcond_rate=0._DP
   !
   if(NAN(real(abs(E),kind=SP)) .or. abs(E)<0.00001_DP) then
     E=abs(z(2))-cI*epsilon(1._DP) 
     PPcond_rate=1._DP  
   elseif(real(E)>0._DP) then     
     E = sqrt(E)
   else
     E = sqrt(-conjg(E)) ! DALV: PPA uses E = 1._SP
     PPcond_rate=1._DP 
   endif
   !
   ! DALV: since MPA uses complex poles 
   !       we need to guarantee the time ordering
   !
   if(real(E)<0._DP) E=-E
   if(aimag(E)>-epsilon(1._DP)) E=real(E,SP)-cI*epsilon(1._DP)
   !
 end subroutine mpa_cond1
 !
 function pole_is_out(i, wmax, thr, E)
   implicit none
   !
   integer ,    intent(in)     :: i
   real(DP),    intent(in)     :: wmax, thr
   complex(DP), intent(inout)  :: E(:)
   !
   integer     :: j
   logical     :: pole_is_out
   !
   pole_is_out=.false.
   !
   if(real(E(i),SP)>wmax) pole_is_out=.true.
   !
   j=1
   do while(j<i.and..not.pole_is_out)
     !
     if(abs(real(E(i),DP)-real(E(j),DP))<thr) then
       !
       pole_is_out=.true.
       !
       if(abs(real(E(j),DP))>max(abs(aimag(E(j))),abs(aimag(E(i))))) then
         E(j)=(real(E(j),DP)+real(E(i),DP))/2._DP-cI*max(abs(aimag(E(j))),abs(aimag(E(i))))
       else
         E(j)=(real(E(j),DP)+real(E(i),DP))/2._DP-cI*min(abs(aimag(E(j))),abs(aimag(E(i))))
       endif
       !
     endif
     !
     j=j+1
     !
   enddo
   !
 endfunction pole_is_out
 !
 subroutine mpa_cond(np, npr, z, E, PPcond)
   !
   integer,     intent(in)     :: np
   integer,     intent(out)    :: npr
   logical,     intent(out)    :: PPcond(np)
   complex(DP), intent(in)     :: z(2*np)
   complex(DP), intent(inout)  :: E(np)
   !
   integer     :: i, j
   complex(DP) :: Eaux(np)
   real(DP)    :: wmax, thr=0.00001_DP
   !
   npr=np
   !
   !DALV: we were using the square before, 
   !      now we use 1.5* the extreme of the interval
   wmax=maxval(sqrt(real(z,SP)))*1.5_DP 
   !
   i=1
   do while(i<=npr)
     !
     Eaux(i)=sqrt(E(i))
     !
     do while(pole_is_out(i,wmax,thr,Eaux).and.i<=npr)
       Eaux(i)=sqrt(E(npr))
       PPcond(npr)=.true.
       npr=npr-1
     enddo
     !
     if(.not.pole_is_out(i,wmax,thr,Eaux).and.abs(real(Eaux(i),SP))<abs(aimag(Eaux(i)))) PPcond(i)=.false.
     Eaux(i)=max(abs(real(Eaux(i),SP)),abs(aimag(Eaux(i))))-cI*min(abs(real(Eaux(i),SP)),abs(aimag(Eaux(i))))
     !
     i=i+1
     !
   enddo
   !
   E(1:npr)=Eaux(1:npr)
   if(npr<np) then
     E(npr+1:np)=1._DP-cI*epsilon(1._DP)
     PPcond(npr+1:np)=.true.
   endif
   !
 end subroutine mpa_cond
 !
 subroutine mpa_E_1p_solver(z, x, E, PPcond_rate) 
   !
   ! DALV: analytical form of the E position of the 1 pole solution
   !
   implicit none
   !
   complex(DP), intent(in)   :: z(2)
   complex(DP), intent(in)   :: x(2)
   complex(DP), intent(out)  :: E
   real(DP),    intent(out)  :: PPcond_rate
   !
   E = (x(1)*z(1)**2 -x(2)*z(2)**2 )/(x(1)-x(2))
   call mpa_cond1(z, E, PPcond_rate)
   !
 end subroutine mpa_E_1p_solver
 !
 subroutine mpa_R_fit(np,npr, w, xq, R, E)
   implicit none
   integer,     intent(in)   :: np, npr
   complex(DP), intent(in)   :: w(2*np)
   complex(DP), intent(in)   :: xq(2*np), E(np)
   complex(DP), intent(out)  :: R(np)
   !
   complex(DP)               :: A(2*np,npr), B(2*np)
   integer                   :: i, k, info, rank, lwork, lwmax 
   parameter                 (lwmax=1000)
   real(DP)                  :: rcond
   integer                   :: iwork(3*npr*0+11*npr)
   real(DP)                  :: S(npr), rwork(10*npr+2*npr*25+8*npr*0+3*25+26*26)
   complex(DP)               :: work(lwmax)
   !
   do k = 1, 2*np
     B(k) = xq(k)
     do i = 1, npr
       A(k,i) = 2._DP*E(i)/(w(k)**2 -E(i)**2)
     enddo
   enddo 
   !
   rcond=-1._DP
   !
! #ifdef _DOUBLE
   call zgelsd(2*np,npr,1,A,2*np,B,2*np,S,rcond,rank,work,-1,   rwork,iwork,info)
   lwork=min(lwmax,int(work(1)))
   call zgelsd(2*np,npr,1,A,2*np,B,2*np,S,rcond,rank,work,lwork,rwork,iwork,info)
! #else
!   call cgelsd(2*np,npr,1,A,2*np,B,2*np,S,rcond,rank,work,-1,   rwork,iwork,info)
!   lwork=min(lwmax,int(work(1)))
!   call cgelsd(2*np,npr,1,A,2*np,B,2*np,S,rcond,rank,work,lwork,rwork,iwork,info)
! #endif
   !
   R(1:npr)=B(1:npr)
   if (npr<np) R(npr+1:np)=0.0_DP
   !
 end subroutine mpa_R_fit
 !
 subroutine mpa_R_solver(np, w, xq, R, E, rcond)
   implicit none
   integer,     intent(in)   :: np
   complex(DP), intent(in)   :: w(2*np)
   complex(DP), intent(in)   :: xq(2*np), E(np)
   complex(DP), intent(out)  :: R(np)
   real(DP),    intent(out)  :: rcond
   !
   complex(DP) :: A(np,np)
   real(DP)    :: anorm
   integer     :: i, k, info, ind(np)    
   complex(DP) :: rwork(2*np), work(2*np)
   !
   anorm=abs(2._DP*E(1)/(w(1)**2 -E(1)**2))
   !
   do k = 1, np
     R(k) = xq(k)
     do i = 1, np
       A(k,i) = 2._DP*E(i)/(w(k)**2 -E(i)**2)
       if(abs(A(k,i))>anorm) anorm=abs(A(k,i))
     enddo
   enddo 
   !
!  #ifdef _DOUBLE
   call zgecon('I', np, A, np, anorm, rcond, work, rwork, info)
   call zgesv(np, 1, A, np, ind, R, np, info) 
!  #else
!    call cgecon('I', np, A, np, anorm, rcond, work, rwork, info)
!    call cgesv(np, 1, A, np, ind, R, np, info) 
!  #endif
   !
 end subroutine mpa_R_solver
 !
 subroutine mpa_E_solver(np, npr, z, xq, E, PPcond, econd)
   implicit none
   integer,     intent(in)   :: np
   integer,     intent(out)  :: npr
   complex(DP), intent(in)   :: z(2*np)
   complex(DP), intent(in)   :: xq(2*np)
   complex(DP), intent(out)  :: E(np)
   logical,     intent(out)  :: PPcond(np)
   real(DP),    intent(out)  :: econd(2)
   !
   complex(DP) :: Z1(np,np), Z2(np,np), M1(np,np), M2(np,np), v1(np)
   complex(DP) :: M(np,np), b(np), Companion(np,np)
   complex(DP) :: rwork(2*np),work(2*np),VR(np),VL(np),S(np),RCE(np,np),RCV(np,np),ABNRM
   integer     :: i, j, info, ind(np), ILO,IHI    
   real(DP)    :: rcond, anorm, Wm
   !
   PPcond(:)=.true.
   Wm = maxval(abs(z(1:np)))
   !
   do j = 1, np
     ind(j) = j
     v1(j)  = xq(j   )*z(j   )**np 
     b (j)  = xq(j+np)*z(j+np)**np 
     Z1(j,1) = 1._DP
     Z2(j,1) = 1._DP  
     M1(j,1) = -xq(j) 
     M2(j,1) = -xq(j+np) 
     M (j,1) = 0._DP
     M (j,j) = 1._DP
     !
     do i = 2, np
       Z1(j,i) =  (z(j)/Wm)**(i-1)
       Z2(j,i) =  (z(j+np)/Wm)**(i-1)
       M1(j,i) = -xq(j   )*Z1(j,i)
       M2(j,i) = -xq(j+np)*Z2(j,i)
       if(i/=j) M(j,i) = 0._DP 
     enddo
     !
   enddo
   !
   anorm=abs(Z1(1,1))
   !
   do j = 1, np
     do i = 1, np
       if(abs(Z1(j,i))>anorm) anorm=abs(Z1(j,i))
     enddo
   enddo
   !
! #ifdef _DOUBLE
   call zgecon('I', np, Z1, np, anorm, econd(1), work, rwork, info)
   call zgesv(np, np, Z1, np, ind, M, np,info) 
! #else
!   call cgecon('I', np, Z1, np, anorm, econd(1), work, rwork, info)
!   call cgesv(np, np, Z1, np, ind, M, np,info) 
! #endif
   !
   M  = matmul(Z2,M)
   M2 = matmul(M,M1) - M2
   b  = matmul(M,v1) - b
   !
   M = 0.0_DP
   !
   do i =1,np
     M(i,i) = 1.0_DP
   enddo
   !
   anorm=abs(M2(1,1))
   !
   do j = 1, np
     do i = 1, np
       if(abs(M2(j,i))>anorm) anorm=abs(M2(j,i))
     enddo
   enddo
   !
! #ifdef _DOUBLE
   call zgecon('I', np, M2, np, anorm, econd(2), work, rwork, info)
   call zgesv(np, 1, M2, np, ind, b, np, info)
! #else
!   call cgecon('I', np, M2, np, anorm, econd(2), work, rwork, info)
!   call cgesv(np, 1, M2, np, ind, b, np, info)
! #endif
   !
   Companion=0._DP
   Companion(1,np) = -b(1)
   !
   do i = 2, np
     Companion(i,np)  = -b(i)/Wm**(i-1)
     Companion(i,i-1) = 1._DP
   enddo
   !
! #ifdef _DOUBLE
   call zgeev( 'N', 'N', np, Companion, np, E, VL, 1, VR, 1, work, 2*np, rwork, info )
! #else
!   call cgeev( 'N', 'N', np, Companion, np, E, VL, 1, VR, 1, work, 2*np, rwork, info )
! #endif
   !
   ! DALV: here we need to force real(E) to be positive.
   ! This is because of the way the residue integral is performed, later.
   !
   call mpa_cond(np, npr, z, E, PPcond)
   !
 end subroutine mpa_E_solver
 !
 subroutine mpa_E_solver_Pade(np, npr, z, X, E, PPcond)
   implicit none
   integer,     intent(in)   :: np
   integer,     intent(out)  :: npr
   complex(DP), intent(in)   :: z(2*np)
   complex(DP), intent(in)   :: X(2*np)
   complex(DP), intent(out)  :: E(np)
   logical,     intent(out)  :: PPcond(np)
   !
   complex(DP) :: c(2*np), b(np+1), Companion(np,np)
   complex(DP) :: c_m1(2*np), b_m1(np+1), b_m2(np+1)
   complex(DP) :: rwork(2*np),work(2*np),VR(np),VL(np)
   integer     :: i, j, info   
   real(DP)    :: rcond, anorm, Wm
   !
   PPcond(:)=.true.
   b_m1(1)=1._DP
   b_m2(1)=1._DP
   b_m1(2:)=0._DP
   b_m2(2:)=0._DP
   b=b_m1
   c=X
   c_m1=c
   !
   do i=2, 2*np
     !
     do j=i, 2*np
       c(j)=(c_m1(i-1)-c_m1(j))/((z(j)-z(i-1))*c_m1(j))
     enddo
     !
     c_m1=c
     !
     do j=1, np+1
       b(j)=b_m1(j)-z(i-1)*c(i)*b_m2(j)
     enddo
     !
     do j=1, np
       b_m2(np+2-j)=c(i)*b_m2(np+1-j)
     enddo
     !
     do j=1, np
       b(j+1)=b(j+1)+b_m2(j+1)
     enddo
     !
     b_m2=b_m1
     b_m1=b
     !
   enddo
   !
   Companion=0._SP
   Companion(1,np) = -b(1)/b(np+1)
   !
   do i = 2, np
     Companion(i,np)  = -b(i)/b(np+1)
     Companion(i,i-1) = 1._DP
   enddo
   !
! #ifdef _DOUBLE
   call zgeev( 'N', 'N', np, Companion, np, E, VL, 1, VR, 1, work, 2*np, rwork, info )
! #else
!   call cgeev( 'N', 'N', np, Companion, np, E, VL, 1, VR, 1, work, 2*np, rwork, info )
! #endif
   !
   ! DALV: here we need to force real(E) to be positive.
   ! This is because of the way the residue integral is performed, later.
   !
   call mpa_cond(np, npr, z, E, PPcond)
   !
 end subroutine mpa_E_solver_Pade
 !
 subroutine mpa_RE_solver(np, w, x, R, E,mpa_sol,MPred,PPcond_rate,MP_err,cond_num)
   implicit none
   !
   integer,      intent(in)   :: np
   complex(SP),  intent(in)   :: w(2*np), x(2*np)
   complex(SP),  intent(out)  :: R(np), E(np)
   character(2), intent(in)   :: mpa_sol
   logical,      intent(out)  :: MPred
   real(SP),     intent(out)  :: PPcond_rate,MP_err
   !
   integer  :: i,npr
   logical  :: PPcond(np)
   real(SP) :: cond_num(2)
   complex( DP) :: w_dummy(2*np), x_dummy(2*np),R_dummy(np), E_dummy(np)
   real(DP) :: PPcond_rate_dummy,MP_err_dummy,cond_num_dummy(2) 
   !
   PPcond_rate_dummy=0._DP
   !
   ! GS Pass intent-in in double precision 
#ifdef _DOUBLE
   w_dummy = w
   x_dummy = x
#else
   w_dummy = cmplx(w, kind=DP)
   x_dummy = cmplx(x, kind=DP)
#endif
   !
   select case(np)
   case(1)
     !
     call mpa_E_1p_solver(w_dummy, x_dummy, E_dummy(1), PPcond_rate_dummy)
     call mpa_R_fit(1, 1, w_dummy, x_dummy, R_dummy, E_dummy)
   !
   ! DALV: we can also use the particular solutions in the cases of 2 and 3 poles
   !
   case default
     !
     if(mpa_sol=='LA') then
       call mpa_E_solver(np, npr, w_dummy**2, x_dummy, E_dummy, PPcond, cond_num_dummy)
       cond_num_dummy=log10(1._DP/cond_num_dummy)
     else ! DALV: we use Pade-Thiele instead (mpa_sol='PT')
       cond_num_dummy=0._DP
       call mpa_E_solver_Pade(np, npr, w_dummy**2, x_dummy, E_dummy, PPcond)
     endif
     !
     if(npr<np) MPred=.true.
     !
     call mpa_R_fit(np, npr, w_dummy, x_dummy, R_dummy, E_dummy)
     !
     do i=1,np
       if(.not.PPcond(i)) PPcond_rate=PPcond_rate+abs(R_dummy(i))
     enddo
     !
     PPcond_rate_dummy=PPcond_rate_dummy/sum(abs(R_dummy(:)))
     !
  end select
   !
   MP_err_dummy=err_func_X(np, R_dummy, E_dummy, w_dummy, x_dummy)
   ! 
   ! GS Get intent-out either in single or double precision 
#ifdef _DOUBLE
   R = R_dummy
   E = E_dummy
   PPcond_rate=PPcond_rate_dummy
   MP_err=MP_err_dummy
   cond_num=cond_num_dummy
#else
   R = cmplx(R_dummy, kind=SP)
   E = cmplx(E_dummy, kind=SP)
   PPcond_rate=real(PPcond_rate_dummy, kind=SP)
   MP_err=real(MP_err_dummy, kind=SP)
   cond_num=real(cond_num_dummy, kind=SP)
#endif
   !
 end subroutine mpa_RE_solver
 !
 !
 function err_func_X(np, R, E, w, x)
   implicit none
   !
   integer ,    intent(in)  :: np
   complex(DP), intent(in)  :: x(2*np), w(2*np)
   complex(DP), intent(in)  :: R(np), E(np)
   !
   complex(DP) :: aux
   real(DP)    :: err_func_X
   integer     :: i,j
   !
   err_func_X = 0._DP
   !
   do j=1, 2*np
     !
     aux = cZERO
     !
     do i=1, np
       aux = aux + 2._DP*E(i)*R(i)/(w(j)**2-E(i)**2)     
     enddo
     !
     err_func_X = err_func_X + abs( aux - x(j) )**2
     !
   enddo  
   !
   ! DALV: this is the Standard Deviation relative to the maximum: 
   !       (Coefficient of variation)
   !
   err_func_X = sqrt(err_func_X/(2._DP*np-1._DP))/maxval(abs(X(:)))
   !
 endfunction err_func_X
 !
 !
 ! ----------------------------------------------------------------
 ! ------- DALV: Not in use ---------------------------------------
 ! ----------------------------------------------------------------
 !
 subroutine mpa_RE_1p_solver(z, x, R, E, PPcond_rate) 
   !
   ! DALV: analytical form of the 1 pole solution
   !
   implicit none
   !
   complex(DP), intent(in)   :: z(2)
   complex(DP), intent(in)   :: x(2)
   complex(DP), intent(out)  :: R, E
   real(DP),    intent(out)  :: PPcond_rate
   !
   E = (x(1)*z(1)**2 -x(2)*z(2)**2 )/(x(1)-x(2))
   !
   call mpa_cond1(z, E, PPcond_rate)
   !
   R = x(2)*(z(2)**2-E**2)/(2._SP*E)
   !
 end subroutine mpa_RE_1p_solver
 !
 !
 subroutine mpa_RQ_2p_solver(w, x, r, q) 
   implicit none
   !
   complex(SP), intent(in)   :: w(4)
   complex(SP), intent(in)   :: x(4)
   complex(SP), intent(out)  :: r(2), q(2)
   !
   integer     :: i, j
   complex(SP) :: a, b, c ! DALV: coefficients of the w^2 polynomial
   complex(SP) :: den, det
   !
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
   q(1) = -(b + sqrt(det))/2._SP
   q(2) = -(b - sqrt(det))/2._SP
   !
   if(abs(a)>epsilon(1._SP)) then
     q(1)=q(1)/a
     q(2)=q(2)/a
   endif
   !
   den  =  (q(1)-q(2))*(w(2)-w(3))*(w(2)+w(3))
   r(1) = -(q(1) - w(2)**2)*(q(1) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(2)*(x(2) - x(3)) )
   r(2) =  (q(2) - w(2)**2)*(q(2) - w(3)**2)*( x(3)*w(3)**2 - x(2)*w(2)**2  + q(1)*(x(2) - x(3)) )
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
   complex(SP), intent(in)  :: w(6)
   complex(SP), intent(in)  :: x(6)
   integer,     intent(in)  :: t
   !
   complex(SP) :: f1
   !
   f1 = (x(1)-x(2)) *w(3)**2 *w(t)**2 *(w(3)**2 - w(t)**2) -  &
        (x(1)-x(3)) *w(2)**2 *w(t)**2 *(w(2)**2 - w(t)**2) +  &
        (x(1)-x(t)) *w(2)**2 *w(3)**2 *(w(2)**2 - w(3)**2) +  &
        (x(2)-x(3)) *w(1)**2 *w(t)**2 *(w(1)**2 - w(t)**2) -  &
        (x(2)-x(t)) *w(1)**2 *w(3)**2 *(w(1)**2 - w(3)**2) +  &
        (x(3)-x(t)) *w(1)**2 *w(2)**2 *(w(1)**2 - w(2)**2)
   !
 end function
 !
 !
 function f2(t, w, x) 
   implicit none
   complex(SP), intent(in)  :: w(6)
   complex(SP), intent(in)  :: x(6)
   integer,     intent(in)  :: t
   !
   complex(SP) :: f2
   !
   f2 = (x(1)-x(2)) *w(1)**2 *w(2)**2 *(w(3)**4 - w(t)**4) -  &
        (x(1)-x(3)) *w(1)**2 *w(3)**2 *(w(2)**4 - w(t)**4) +  &
        (x(1)-x(t)) *w(1)**2 *w(t)**2 *(w(2)**4 - w(3)**4) +  &
        (x(2)-x(3)) *w(2)**2 *w(3)**2 *(w(1)**4 - w(t)**4) -  &
        (x(2)-x(t)) *w(2)**2 *w(t)**2 *(w(1)**4 - w(3)**4) +  &
        (x(3)-x(t)) *w(3)**2 *w(t)**2 *(w(1)**4 - w(2)**4)
   !
 end function
 !
 !
 function f3(t, w, x) 
   implicit none
   complex(SP), intent(in)  :: w(6)
   complex(SP), intent(in)  :: x(6)
   integer,     intent(in)  :: t
   !
   complex(SP) :: f3
   !
   f3 = (x(1)-x(2)) *w(1)**4 *w(2)**4 *(w(3)**2 - w(t)**2) -  &
        (x(1)-x(3)) *w(1)**4 *w(3)**4 *(w(2)**2 - w(t)**2) +  &
        (x(1)-x(t)) *w(1)**4 *w(t)**4 *(w(2)**2 - w(3)**2) +  &
        (x(2)-x(3)) *w(2)**4 *w(3)**4 *(w(1)**2 - w(t)**2) -  &
        (x(2)-x(t)) *w(2)**4 *w(t)**4 *(w(1)**2 - w(3)**2) +  &
        (x(3)-x(t)) *w(3)**4 *w(t)**4 *(w(1)**2 - w(2)**2)
   !
 end function
 !
 !
 function f4(t, w, x) 
   implicit none
   complex(SP), intent(in)  :: w(6)
   complex(SP), intent(in)  :: x(6)
   integer,     intent(in)  :: t
   !
   complex(SP) :: f4
   !
   f4 = x(1) *w(1)**6 * (w(2)**2 - w(3)**2) * (w(2)**2 - w(t)**2) * (w(3)**2 - w(t)**2) - &
        x(2) *w(2)**6 * (w(1)**2 - w(3)**2) * (w(1)**2 - w(t)**2) * (w(3)**2 - w(t)**2) + &
        x(3) *w(3)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(t)**2) * (w(2)**2 - w(t)**2) - &
        x(t) *w(t)**6 * (w(1)**2 - w(2)**2) * (w(1)**2 - w(3)**2) * (w(2)**2 - w(3)**2)
   !
 end function
 !
 !
 subroutine mpa_RQ_3p_solver(w, x, r, q) 
   implicit none
   complex(SP), intent(in)   :: w(6)
   complex(SP), intent(in)   :: x(6)
   complex(SP), intent(out)  :: r(3), q(3)
   !
   integer     :: i, j
   complex(SP) :: a, b, c, d ! DALV: coefficients of the w^2 polynomial
   complex(SP) :: D1, D2, den, deR 
   real(SP)    :: t=0.33333333333333334_SP 
   complex(SP) :: s=(1._SP, 1.7320508075688772_SP)
   !
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
   q(1) = -( den*(b- den/2._SP**(t)) + 2.0_SP**(t)*D1)/3.0_SP
   q(2) = -( den*(b+ conjg(s)* den/2._SP**(4._SP*t)) - s* D1/2._SP**(2._SP*t)  )/3._SP
   q(3) = -( den*(b+ s*den/2._SP**(4._SP*t)) - conjg(s)* D1/2._SP**(2._SP*t)  )/3._SP
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
         -(w(3)**2-q(2))*(w(3)**2-q(3))*( (w(1)**2-q(2))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(2)) )*x(3) ) 
   !
   r(2) = (w(1)**2-q(2))*(w(2)**2-q(2))*(w(3)**2-q(2))*(                                                         &
         +(w(1)**2-q(1))*(w(1)**2-q(3))*( (w(2)**2-q(1))*(w(3)**2-q(3)) - (w(2)**2-q(3))*(w(3)**2-q(1)) )*x(1)   &
         -(w(2)**2-q(1))*(w(2)**2-q(3))*( (w(1)**2-q(1))*(w(3)**2-q(3)) - (w(1)**2-q(3))*(w(3)**2-q(1)) )*x(2)   &
         +(w(3)**2-q(1))*(w(3)**2-q(3))*( (w(1)**2-q(1))*(w(2)**2-q(3)) - (w(1)**2-q(3))*(w(2)**2-q(1)) )*x(3) ) 
   !
   r(3) = (w(1)**2-q(3))*(w(2)**2-q(3))*(w(3)**2-q(3))*(                                                        &
         -(w(1)**2-q(1))*(w(1)**2-q(2))*( (w(2)**2-q(1))*(w(3)**2-q(2)) - (w(2)**2-q(2))*(w(3)**2-q(1)) )*x(1)  & 
         +(w(2)**2-q(1))*(w(2)**2-q(2))*( (w(1)**2-q(1))*(w(3)**2-q(2)) - (w(1)**2-q(2))*(w(3)**2-q(1)) )*x(2)  & 
         -(w(3)**2-q(1))*(w(3)**2-q(2))*( (w(1)**2-q(1))*(w(2)**2-q(2)) - (w(1)**2-q(2))*(w(2)**2-q(1)) )*x(3) )
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
end module mpa_m


