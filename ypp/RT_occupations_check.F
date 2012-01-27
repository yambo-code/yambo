! 
! Copyright (C) 2000-2012 A. Marini and the YAMBO team
!              http://www.yambo-code.org
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
integer function RT_occupations_check(E,k)
 !
 use pars,           ONLY:zero_dfl
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use QP_m,           ONLY:OCC_ctl
 use real_time,      ONLY:OCCUPATIONS_IO_and_interpolate,RT_step,max_rt_steps_disk
 use SC,             ONLY:SC_bands,RT_occupations
 use IO_m,           ONLY:LOG,NONE
 use com,            ONLY:warning
 use YPP,            ONLY:RT_occupations_ref,OCC_T_ref,OCC_deltaT,OCC_T_range
 implicit none
 type(bz_samp)      :: k
 type(levels)       :: E
 !
 call OCCUPATIONS_IO_and_interpolate(E,'G',k=k,Time=OCC_T_range(1),imposed_COM=LOG)
 !
 if (.not.allocated(RT_occupations_ref)) then 
   !
   allocate(RT_occupations_ref(SC_bands(1):SC_bands(2),k%nibz))
   !
   RT_occupations_ref=0.
   !
 endif
 !
 if (OCC_T_ref>=0.) then
   !
   call OCCUPATIONS_IO_and_interpolate(E,'G',k=k,Time=OCC_T_ref,imposed_COM=NONE)
   !
   RT_occupations_ref=RT_occupations
   !
 endif
 !
 if (.not.allocated(RT_occupations)) then
   !
   call warning('Occupations database not found in :'//trim(OCC_ctl(3))//'. Switching to standard DOS')
   !
   RT_occupations_check=-1
   !
   return
   !
 endif
 !
 call OCCUPATIONS_IO_and_interpolate(E,'G',k=k,Time=OCC_T_range(2))
 !
 if (OCC_deltaT<max_rt_steps_disk*RT_step) then
   ! 
   OCC_deltaT=max_rt_steps_disk*RT_step
   ! 
 else
   !
   OCC_deltaT=nint(OCC_deltaT/max_rt_steps_disk/RT_step)*max_rt_steps_disk*RT_step
   !
 endif
 !
 RT_occupations_check=nint( (OCC_T_range(2)-OCC_T_range(1))/OCC_deltaT )+1
 !
end function RT_occupations_check
