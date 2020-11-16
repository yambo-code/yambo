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
 use pars,           ONLY:schlen
 !
 integer          :: PH_DbGd_points=0
 logical          :: l_PH_uses_E_FineGd=.FALSE.
 character(schlen):: PH_Self_Energy_mode="dressed-dressed" ! "bare-dressed" (="dressed-bare") / "bare-bare"
 !
 type phel_scatterings
   real(SP)    :: transitions(3)  ! Number of transitions: 1-> accepted, 2-> total, 3-> ratio
   integer, allocatable  :: table(:,5) ! Indices of accepted transitions: 1-> il, 2-> iq, 3-> ik, 4-> i_n, 5-> i_np
 end type phel_scatterings
 !
end module
