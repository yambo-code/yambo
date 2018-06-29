!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
logical function OPTICS_eels(wv,nw,be_verbose,ordering,eps,eel)
 ! 
 use pars,          ONLY:SP,pi,cZERO,cI
 use units,         ONLY:HA2EV
 use com,           ONLY:warning
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
 OPTICS_eels=.TRUE.
 !
 if (.not.present(eps)) then
   if (ordering=='Tr'.or.ordering=='Ta') then
     if (real(wv(1))<-real(wv(1))) then
       OPTICS_eels=.FALSE.
       if (be_verbose) call warning(' [OPTICS_eels] Impossible to evaluate the EELS')
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
   call warning(' [OPTICS_eels] Possible inaccuracy in the EPS prolungation')
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
 forall(iw=1:nw) eps_kk(iw)=1./pi*aimag(eps(nw-iw+1))
 forall(iw=1:nw) wv_kk(iw)=-wv(nw-iw+1)
 eps_kk(1)      =cZERO
 eps_kk(nw)     =cZERO
 call Kramers_Kronig(eps_kk,real(wv_kk),nw,eps_extended,real(wv)+cI*1.E-4_SP/HA2EV,nw,cZERO)
 !
 eel=-1._SP/(eps_extended+1._SP)
 !
end function
