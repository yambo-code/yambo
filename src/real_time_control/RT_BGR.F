!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine RT_BGR(E,k,BGR)
 !
 ! This routine calculates the BGR
 !
 use pars,                ONLY:SP
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp
 use real_time,           ONLY:l_update_QP_Energies
 use interfaces,          ONLY:OCCUPATIONS_Gaps
 !
 implicit none
 !
 type(levels),  intent(inout) :: E
 type(bz_samp), intent(in)    :: k
 real(SP),      intent(out)   :: BGR(2)
 !
 ! Workspace
 !
 integer :: Index_gap(2)
 !
 if(.not.l_update_QP_Energies) return
 !
 ! Evaluation of the contribution to the minimal gap
 !
 call OCCUPATIONS_Gaps(E,I_dir=Index_gap)
 !
 BGR(1)=E%E_dir_gap(1)
 BGR(2)=E%E_ind_gap(1)
 !
end subroutine RT_BGR
