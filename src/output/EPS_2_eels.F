!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
logical function EPS_2_eels(wv,nw,be_verbose,ordering,eps,eel)
 ! 
 use pars,          ONLY:SP,pi,cZERO,cI
 use units,         ONLY:HA2EV
 !
 implicit none
 !
 integer              :: nw
 complex(SP)          :: wv(nw)
 logical              :: be_verbose
 character(2)         :: ordering
 complex(SP) ,optional:: eps(nw),eel(nw)
 !
 ! Work Space
 !
 integer            :: iw
 complex(SP)        :: eps_extended(nw),eps_kk(nw),wv_kk(nw)
 !
 EPS_2_eels=.TRUE.
 !
 if (.not.present(eps)) then
   if (ordering=='Tr'.or.ordering=='Ta') then
     if (real(wv(1))<-real(wv(1))) then
       EPS_2_eels=.FALSE.
       if (be_verbose) call warning(' [EPS_2_eels] Impossible to evaluate the EELS')
       return
     endif
   endif
   return
 endif
 !
 if (trim(ordering)=='T'.or.trim(ordering)=='R') then
   eel(:)=-1._SP/eps(:)
   return
 endif
 !
 if ( (maxval(aimag(eps))/aimag(eps(1))<100._SP   .or. &
&      maxval(aimag(eps))/aimag(eps(nw))<100._SP) .and. be_verbose) then
   !
   call warning(' [EPS_2_eels] Possible inaccuracy in the EPS prolungation')
   !
 endif
 !
 ! extend eps to the negative frequencies
 !
 eps_kk         =-1._SP/pi*aimag(eps)
 eps_kk(1)      =cZERO
 eps_kk(nw)     =cZERO
 eps_extended   =cZERO
 call Kramers_Kronig(eps_kk,real(wv),nw,eps_extended,real(wv)+cI*1.E-4_SP/HA2EV,nw,cZERO)
 !
 do iw=1,nw
   eps_kk(iw)=1./pi*aimag(eps(nw-iw+1))
   wv_kk(iw)=-wv(nw-iw+1)
 enddo
 eps_kk(1)      =cZERO
 eps_kk(nw)     =cZERO
 call Kramers_Kronig(eps_kk,real(wv_kk),nw,eps_extended,real(wv)+cI*1.E-4_SP/HA2EV,nw,cZERO)
 !
 eel=-1._SP/(eps_extended+1._SP)
 !
end function
