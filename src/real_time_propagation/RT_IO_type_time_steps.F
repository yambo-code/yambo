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
 ! Note that all _INPUT = _INPUT, values given and reported in INPUT file
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
   call SETUP_RT_IO_type(CARR_RT_IO_t,what=="INIT")
   call SETUP_RT_IO_type(OUTPUT_RT_IO_t,what=="INIT")
   call SETUP_RT_IO_type(GF_RT_IO_t,what=="INIT")
   call SETUP_RT_IO_type(OBS_RT_IO_t,what=="INIT")
   !
 endif
 !
 if (what=="INIT") then
   !
   ! CACHE interval => OBS I/O steps. Here I impose the OBS I/O and CACHE steps to be consistent
   !
   CACHE_OBS_INTERVAL_time_INPUT=TIME_adjust(CACHE_OBS_INTERVAL_time_INPUT,RT_step)
   OBS_RT_IO_t%INTERVAL_time_INPUT=TIME_adjust(OBS_RT_IO_t%INTERVAL_time_INPUT,CACHE_OBS_INTERVAL_time_INPUT)
   !
   CACHE_OBS_INTERVAL_time=CACHE_OBS_INTERVAL_time_INPUT
   CACHE_OBS_steps=OBS_RT_IO_t%INTERVAL_steps/nint(CACHE_OBS_INTERVAL_time/RT_step)
   !
   return
   !
 endif
 !
 if (what=="RESTART") then
   !
   !if (NE_time_step_update_jump>0._SP) OBS_RT_IO_t%INTERVAL_time=TIME_adjust(OBS_RT_IO_t%INTERVAL_time,NE_time_step_update_jump)
   !
   CACHE_OBS_INTERVAL_time=TIME_adjust(CACHE_OBS_INTERVAL_time,RT_dyn_step)
   !
   CACHE_OBS_steps=OBS_RT_IO_t%INTERVAL_steps/nint(CACHE_OBS_INTERVAL_time/RT_dyn_step)/nint(RT_dyn_step/RT_step)
   !
   call RT_control_free("cache")
   call RT_control_alloc("cache")
   !
   !OUTPUT_RT_IO_t%INTERVAL_time  = max(TIME_adjust(OUTPUT_RT_IO_t%INTERVAL_time,RT_dyn_step),RT_dyn_step)
   !OUTPUT_RT_IO_t%INTERVAL_steps = nint(OUTPUT_RT_IO_t%INTERVAL_time/RT_step)
   !OUTPUT_RT_IO_t%last_point     = NE_i_time - OUTPUT_RT_IO_t%INTERVAL_steps
   !
   return
   !
 endif
 !
 if (what=="UPDATE") then
   !
   ! ... and the dT update steps (NE_time_step_update_jump)
   !
   !NE_time_step_update_jump = TIME_adjust(NE_time_step_update_jump,CARR_RT_IO_t%INTERVAL_time)
   !OBS_RT_IO_t%INTERVAL_time=TIME_adjust(OBS_RT_IO_t%INTERVAL_time,NE_time_step_update_jump)
   !
   ! ... OBS cache
   !
   CACHE_OBS_INTERVAL_time = TIME_adjust(CACHE_OBS_INTERVAL_time,RT_dyn_step)
   CACHE_OBS_steps=OBS_RT_IO_t%INTERVAL_steps/nint(CACHE_OBS_INTERVAL_time/RT_dyn_step)/nint(RT_dyn_step/RT_step)
   call RT_control_free("cache")
   call RT_control_alloc("cache")
   !
   !write (*,*) "UPDATE@",NE_i_time*RT_step/FS2AUT
   !write (*,*) "dT",RT_dyn_step/FS2AUT
   !
   ! ... post setups
   !
   !GF_RT_IO_t%INTERVAL_time=TIME_adjust(GF_RT_IO_t%INTERVAL_time,CARR_RT_IO_t%INTERVAL_time)
   !
   return
   !
 endif
 !
end subroutine RT_IO_type_time_steps
