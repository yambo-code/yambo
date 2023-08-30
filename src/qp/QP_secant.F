!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
!@brief Secant solver for the quasi-particle equation
!
integer function QP_secant(x1,f1,x2,f2,acc,nstep)
 !
 use pars, ONLY:SP
 !
 implicit none
 real(SP)     :: f1,f2,x1,x2,acc
 integer      :: nstep
 !
 ! Work Space
 !
 real(SP):: Dx
 !
 QP_secant=0
 if (abs(f1)<=acc) QP_secant=-1
 if (abs(f2)<=acc) QP_secant=-2
 if (abs(f1)<=acc.or.abs(f2)<=acc) return
 if (abs(f1)<abs(f2)) then
   QP_secant=2
   Dx=-f2*(x1-x2)/(f1-f2)
   Dx=Dx/(1+nstep/20)
   x2=x2+Dx
 else if (abs(f1)>abs(f2)) then
   QP_secant=1
   Dx=-f1*(x1-x2)/(f1-f2)
   Dx=Dx/(1+nstep/20)
   x1=x1+Dx
 else if (abs(f1)==abs(f2)) then
   QP_secant=2
   x2=x2+abs(f1)/(2+nstep/20)
 endif
 !
end function
