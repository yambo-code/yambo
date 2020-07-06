!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_IO_type_time_steps(what)
 !
 use real_time,      ONLY:RT_step,RT_dyn_step
 use RT_control,     ONLY:CARR_RT_IO_t,OBS_RT_IO_t,RT_IO_t,GF_RT_IO_t,OUTPUT_RT_IO_t,&
&                         SETUP_RT_IO_type,CACHE_OBS_INTERVAL_time_INPUT,TIME_adjust,&
&                         CACHE_OBS_INTERVAL_time,CACHE_OBS_steps,RT_control_free,&
&                         RT_control_alloc
 !use units
 ! 
 implicit none
 !
 character(*) :: what
 !
 if (what=="INIT".or.what=="UPDATE".or.what=="RESTART") then
   !
   call SETUP_RT_IO_type(CARR_RT_IO_t,who="CARR",what=what)
   call SETUP_RT_IO_type(OUTPUT_RT_IO_t,who="OUTPUT",what=what)
   call SETUP_RT_IO_type(GF_RT_IO_t,who="GF",what=what)
   call SETUP_RT_IO_type(OBS_RT_IO_t,who="OBS",what=what)
   !
 endif
 !
 if (what=="INIT") then
   !
   CACHE_OBS_INTERVAL_time_INPUT=TIME_adjust(CACHE_OBS_INTERVAL_time_INPUT,RT_step)
   !
   ! CACHE interval => OBS I/O steps. Here I impose the OBS I/O and CACHE steps to be consistent
   !
   OBS_RT_IO_t%INTERVAL_time_INPUT=TIME_adjust(OBS_RT_IO_t%INTERVAL_time_INPUT,CACHE_OBS_INTERVAL_time_INPUT)
   OBS_RT_IO_t%INTERVAL_steps = nint(OBS_RT_IO_t%INTERVAL_time_INPUT/RT_step)
   !
   CACHE_OBS_INTERVAL_time=CACHE_OBS_INTERVAL_time_INPUT
   CACHE_OBS_steps=OBS_RT_IO_t%INTERVAL_steps/nint(CACHE_OBS_INTERVAL_time/RT_step)
   !
   return
   !
 endif
 !
 if (what=="UPDATE".or.what=="RESTART") then
   !
   CACHE_OBS_INTERVAL_time = TIME_adjust(CACHE_OBS_INTERVAL_time,RT_dyn_step)
   !
   ! CACHE interval => OBS I/O steps. Here I impose the OBS I/O and CACHE steps to be consistent
   !
   OBS_RT_IO_t%INTERVAL_time=TIME_adjust(OBS_RT_IO_t%INTERVAL_time,CACHE_OBS_INTERVAL_time)
   OBS_RT_IO_t%INTERVAL_steps = nint(OBS_RT_IO_t%INTERVAL_time/RT_step)
   !
   CACHE_OBS_steps=OBS_RT_IO_t%INTERVAL_steps/nint(CACHE_OBS_INTERVAL_time/RT_dyn_step)/nint(RT_dyn_step/RT_step)
   !
   call RT_control_free("cache")
   call RT_control_alloc("cache")
   !
   return
   !
 endif
 !
end subroutine RT_IO_type_time_steps
