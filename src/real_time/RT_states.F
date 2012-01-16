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
integer function RT_states(E)
 ! 
 use electrons,      ONLY:levels
 use real_time,      ONLY:RT_energy_window,RT_px
 use SC,             ONLY:SC_bands
 use pars,           ONLY:zero_dfl
 use R_lattice,      ONLY:nkibz
 use par_indexes_m,  ONLY:par_indexes
 use stderr,         ONLY:intc
 use com,            ONLY:warning
 use par_proc_m,     ONLY:pp_indexes_reset,pp_indexes
 !
 implicit none
 !
 type(levels), intent(in) :: E
 !
 ! Work space
 !
 integer           :: ib,ik,nk_todo,ik_todo,SC_bands_new(2)
 logical           :: RT_k_point(nkibz),RT_b(SC_bands(2),nkibz)
 type(pp_indexes)  :: p_loc
 !
 RT_k_point=.FALSE.
 RT_b      =.FALSE.
 !
 ! Pre-allocation of CPU tables
 !
 call pp_indexes_reset(RT_px)
 !
 call par_indexes(RT_px,(/nkibz/))
 !
 if (all(RT_energy_window<zero_dfl)) then
   !
   RT_k_point=.TRUE.
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
         RT_b(ib,ik)   =.TRUE.
         !
       endif
       !
     enddo
     !
   enddo
   !
   nk_todo=count(RT_k_point)
   !
   call pp_indexes_reset(p_loc)
   !
   call par_indexes(p_loc,(/nk_todo/))
   !
   ik_todo=0
   !
   do ik=1,nkibz
     !
     RT_px%element_1D(ik)=.FALSE.
     !
     if (.not.RT_k_point(ik)) cycle
     !
     ik_todo=ik_todo+1
     !
     if (p_loc%element_1D(ik_todo)) RT_px%element_1D(ik)=.TRUE.
     !
   enddo
   !
   call pp_indexes_reset(p_loc)
   !
   ! Redefine SC_bands ?
   !
   SC_bands_new=SC_bands
   !
   do ib=SC_bands(1),SC_bands(2)-1
     !
     if (.not.any(RT_b(ib,:)).and.     any(RT_b(ib+1,:))) SC_bands_new(1)=ib+1
     if (     any(RT_b(ib,:)).and..not.any(RT_b(ib+1,:))) SC_bands_new(2)=ib
     !
   enddo
   !
 endif
 !
 RT_states=nk_todo
 !
 if (any((/SC_bands/=SC_bands_new/))) then
   SC_bands =SC_bands_new
   call warning('Bands reduced to '//trim(intc(SC_bands(1)))//'-'//trim(intc(SC_bands(2))))
 endif
 !
end function RT_states
