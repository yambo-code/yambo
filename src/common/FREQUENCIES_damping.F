!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
function FREQUENCIES_damping(W,E)
 !
 use pars,      ONLY:SP
 use frequency, ONLY:w_samp
 implicit none
 !
 real(SP)      :: FREQUENCIES_damping,E,beta
 type(w_samp)  :: W
 ! 
 ! Work Space
 !
 real(SP):: e1,e2,d1,d2,E_
 !
 e1=W%er(1)
 e2=W%er(2)
 d1=W%dr(1)
 d2=W%dr(2)
 E_=E
 !
 if (W%er(1)<0._SP .and. W%er(2)<=0._SP) then
   e1=-W%er(2)
   e2=-W%er(1)
   E_=-E
 endif
 !
 if (W%er(1)<0._SP .and. W%er(2)>0._SP ) then
   !
   ! Cannot use Fermi damping is the E range is not everywhere positve
   !
   W%damp_reference=0._SP
   if (E>=0._SP) then
     e1=0._SP
     e2=W%er(2)
   else
     e1=0._SP
     e2=-W%er(1)
     E_=-E
   endif
 endif
 !
 ! To use Fermi we need that d2/2 > d1 
 !
 if (d2/2._SP<= d1) then
   W%damp_reference=0._SP
 endif
 !
 if (W%damp_reference==0._SP) then
   !
   !           /(E2,D2)
   !          /
   !         /
   !   (E1,D1) 
   !
   FREQUENCIES_damping=d1+(d2-d1)/(e2-e1)*(E_-e1)
 else
   beta=1._SP/(W%damp_reference-e1)*log( (d2-d1)/d1 )
   FREQUENCIES_damping=d2/(1._SP+exp(-beta*(E_-W%damp_reference)))
 endif
 !
end function
