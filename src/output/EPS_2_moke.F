!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine EPS_2_moke(wv,nw,eps_ii,eps_ij,moke)
 ! 
 use pars,          ONLY:SP,cZERO
 use units,         ONLY:DEG2RAD
 !
 implicit none
 !
 integer              :: nw
 complex(SP)          :: wv(nw),eps_ii(nw),eps_ij(nw),moke(nw)
 !
 ! Work Space
 !
 integer            :: iw
 complex(SP)        :: wk
 !
 do iw=1,nw
   moke(iw)=cZERO
   wk= eps_ii(iw)*sqrt(eps_ii(iw)+1._SP)
   if (real(wk)==0._SP.and.aimag(wk)==0._SP) cycle
   moke(iw)=-eps_ij(iw)/wk/DEG2RAD
 enddo 
 !
end subroutine
