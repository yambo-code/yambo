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
 use pars,          ONLY:SP,DP
 use qe_pseudo_m,   ONLY:lmaxq,g,gg,nhm,nat,nspin,tpiba
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
 integer :: ngm, ig
 integer :: iGo,iq,isym
 real(DP):: q(3)
 real(DP), allocatable :: q_plus_g(:,:)
 real(DP), allocatable :: q_plus_g_m(:)

 if (isc%ngrho<=0) return
 !
 ngm=isc%ngrho
 !
 if (.not.allocated(isc%qmod)) then
   YAMBO_ALLOC( isc%qmod,(ngm))
 endif
 if (.not.allocated(isc%qgm)) then
   YAMBO_ALLOC( isc%qgm,(ngm))
 endif
 if (.not.allocated(isc%ylmk0)) then
   YAMBO_ALLOC( isc%ylmk0,(ngm, lmaxq*lmaxq) )
 endif
 if (.not.allocated(isc%becprod)) then
   YAMBO_ALLOC( isc%becprod,(nhm,nhm,nat,nspin) )
   isc%becprod=0.0
 endif
 !
 YAMBO_ALLOC(q_plus_g,(3,ngm))
 YAMBO_ALLOC(q_plus_g_m,(ngm))
 !
 iGo=isc%qs(1)
 iq=isc%qs(2)
 isym=isc%qs(3)
 !
 q(:)=0.0
 !
 ! g in tpiba units
 do ig = 1, ngm
   q_plus_g(:,ig) = q(:) + g(:,ig)
   q_plus_g_m(ig) = sum(q_plus_g(:,ig)**2)
 enddo
 !
 call ylmr2(lmaxq*lmaxq, ngm, q_plus_g, q_plus_g_m, isc%ylmk0)
 !
 do ig = 1, ngm
    isc%qmod(ig) = sqrt(q_plus_g_m(ig))
 enddO
 !
 YAMBO_FREE(q_plus_g)
 YAMBO_FREE(q_plus_g_m)
 !
 return
 !
end subroutine PP_scatterBamp_init

