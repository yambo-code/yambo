!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
logical function EPS_check(wv,nw,be_verbose,ordering)
 !
 ! Check if epsilon is computed or if it can be extended to the negative axes
 ! 
 use pars,          ONLY:SP
 !
 implicit none
 !
 integer              :: nw
 complex(SP)          :: wv(nw)
 logical              :: be_verbose
 character(2)         :: ordering
 !
 EPS_check=.TRUE.
 !
 if (ordering=='Tr'.or.ordering=='Ta') then
   if (real(wv(1))<-real(wv(1))) then
     EPS_check=.FALSE.
     if (be_verbose) call warning(' [EPS_check] Impossible to extend EPS in the w<0 axis')
     return
   endif
 endif
 !
end function EPS_check
!
!
subroutine EPS_extend(wv,nw,be_verbose,ordering,eps,eps_extended)
 !
 ! extend epsilon to the negative axes if needed
 ! This subroutines always works with (eps - 1.) to avoid numerical noise
 ! 
 use pars,          ONLY:SP,pi,cZERO,cI
 use units,         ONLY:HA2EV
 use com,           ONLY:msg
 !
 implicit none
 !
 integer              :: nw
 complex(SP)          :: wv(nw)
 logical              :: be_verbose
 character(2)         :: ordering
 complex(SP)          :: eps(nw),eps_extended(nw)
 !
 ! Work Space
 !
 integer            :: iw
 complex(SP)        :: eps_kk(nw),wv_kk(nw)
 !
 if (trim(ordering)=='T'.or.trim(ordering)=='R') then
   eps_extended=eps
   return
 endif
 !
 if ( be_verbose ) then
   !
   call msg('s',' EPS prolungation into the w<0 axis')
   !
   if ( (maxval(aimag(eps))/aimag(eps(1))<100._SP .or. &
&        maxval(aimag(eps))/aimag(eps(nw))<100._SP)  ) &
&    call warning(' [EPS_extend] Possible inaccuracy in the EPS prolungation')
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
   eps_kk(iw)=1._SP/pi*aimag(eps(nw-iw+1))
   wv_kk(iw)=-wv(nw-iw+1)
 enddo
 eps_kk(1)      =cZERO
 eps_kk(nw)     =cZERO
 call Kramers_Kronig(eps_kk,real(wv_kk),nw,eps_extended,real(wv)+cI*1.E-4_SP/HA2EV,nw,cZERO)
 !
end subroutine EPS_extend
