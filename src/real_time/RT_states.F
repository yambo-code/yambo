!
!  Copyright (C) 2000-2012 A. Marini and the YAMBO team 
!            http://www.yambo-code.org
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
subroutine RT_states(E)
 ! 
 use electrons,      ONLY:levels
 use real_time,      ONLY:RT_energy_window,RT_k_point,RT_px
 use SC,             ONLY:SC_bands
 use pars,           ONLY:zero_dfl
 use R_lattice,      ONLY:nkibz
 use par_indexes_m,  ONLY:par_indexes
 !
 implicit none
 !
 type(levels), intent(in) :: E
 !
 ! Work space
 !
 integer :: ib,ik
 !
 if (.not.allocated(RT_k_point)) then
   !
   allocate(RT_k_point(nkibz))
   !
   RT_k_point=.FALSE.
   !
 endif
 !
 if (all(RT_energy_window<zero_dfl)) then
   !
   RT_k_point=.TRUE.
   !
   call par_indexes(RT_px,(/nkibz/))
   !
   return
   !
 else
   !
   do ik=1,nkibz
     !
     do ib=SC_bands(1),SC_bands(2)
       !
       if (E%E(ib,ik,1)>RT_energy_window(1) .and. E%E(ib,ik,1)<RT_energy_window(2)) then
        !
         RT_k_point(ik)=.TRUE.
         !
       endif
       !
     enddo
     !
   enddo
   !
 endif
 !
end subroutine RT_states
