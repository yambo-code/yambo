!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP
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
module PHEL
 !
 use pars,           ONLY:SP,schlen
 !
#include<memory.h>
 !
 character(schlen):: PH_Self_Energy_mode="dressed-dressed" ! "bare-dressed" (="dressed-bare") / "bare-bare"
 !
 ! ... Double-grid
 integer              :: PH_DbGd_points=-1
 logical              :: l_PH_uses_E_FineGd=.FALSE.
 !
 ! ... Maps
 integer,     allocatable :: KpQ_io_map(:,:)
 !
 ! ... Scatterings
 !
 ! table:
 ! 1 -> il [ph mode], 2 -> iq [ph q ibz], 3 -> ikbz [k bz], 4 -> ik_m_q [k-q (i)bz],
 ! 5 -> ik_fg [k (i)bz DbGd], 6 -> ip_fg [k-q (i)bz DbGd], 7 -> i_n [bnd], 8 -> i_np [bnd]
 type phel_scatterings
   integer               :: N=0        ! Number of accepted transitions
   integer, allocatable  :: table(:,:) ! Indices of accepted transitions: see above
 end type phel_scatterings             
 !
 type(phel_scatterings)  :: phel_info
 !
 contains
   !
   subroutine PHEL_free( )
      YAMBO_FREE(phel_info%table)
      YAMBO_FREE(KpQ_io_map)
   end subroutine
   !
end module
