!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, IM
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
subroutine PP_scatterBamp_init(isc)
 !
 ! init USPP workspace in isc scatterBamp argument
 !
 use pars,          ONLY:SP
 use qe_pseudo_m,   ONLY:lmaxq,g,gg,nhm,nat,nspin
 use collision_el,  ONLY:elemental_collision
 !
#include<memory.h>
 !
 ! vars
 !
 type(elemental_collision)  ::isc

 !
 ! Workspace
 !
 integer :: ngm_l, ig

 if (isc%ngrho<=0) return
 !
 ngm_l=isc%ngrho
 !
 if (.not.allocated(isc%qmod)) then
   YAMBO_ALLOC( isc%qmod,(ngm_l))
 endif
 if (.not.allocated(isc%qgm)) then
   YAMBO_ALLOC( isc%qgm,(ngm_l))
 endif
 if (.not.allocated(isc%ylmk0)) then
   YAMBO_ALLOC( isc%ylmk0,(ngm_l, lmaxq*lmaxq) )
 endif
 if (.not.allocated(isc%becprod)) then
   YAMBO_ALLOC( isc%becprod,(nhm,nhm,nat,nspin) )
 endif
 !
 call ylmr2(lmaxq*lmaxq, ngm_l, g, gg, isc%ylmk0)
 do ig = 1, ngm_l
    isc%qmod(ig) = sqrt(gg(ig))
 enddO
 !
 return
 !
end subroutine PP_scatterBamp_init

