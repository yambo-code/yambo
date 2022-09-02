!
!        Copyright (C) 2000-2022 the YAMBO team
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
