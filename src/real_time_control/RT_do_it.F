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
logical function RT_do_it(what)
 !
 use RT_control,     ONLY:OBS_RT_IO_t,GF_RT_IO_t,CARR_RT_IO_t,CACHE_OBS_INTERVAL_time,&
&                         CACHE_OBS_last_point,OUTPUT_RT_IO_t,NE_i_start_time,STOP_the_dynamics_NOW
 use real_time,      ONLY:NE_i_time,RT_step,NE_time_step_update_jump,NE_time_step_update_last_point
 use parser_m,       ONLY:parser
 use it_tools,       ONLY:it_reset
 !
 implicit none
 !
 character(*) :: what
 !
 ! Work Space
 !
 integer :: INTERVAL_steps,i_grp,last_point
 logical, external :: RT_Dynamics_is_over
 !
 RT_do_it=.FALSE.
 !
 ! Selected Time points to perform dedicated operations
 !======================
 last_point = 1
 if (what=="OBS") then
   INTERVAL_steps=OBS_RT_IO_t%INTERVAL_steps
   last_point    =OBS_RT_IO_t%last_point
 endif
 if (what=="CARR") then
   INTERVAL_steps=CARR_RT_IO_t%INTERVAL_steps
   last_point    =CARR_RT_IO_t%last_point
 endif
 if (what=="OUT") then
   INTERVAL_steps=OUTPUT_RT_IO_t%INTERVAL_steps
   last_point    =OUTPUT_RT_IO_t%last_point
 endif
 if (what=="G") then
   INTERVAL_steps=GF_RT_IO_t%INTERVAL_steps
   last_point    =GF_RT_IO_t%last_point
 endif
 if (what=="OBS_cache") then
   INTERVAL_steps=nint(CACHE_OBS_INTERVAL_time/RT_step)
   last_point    =CACHE_OBS_last_point
 endif
 if (what=="deltaT") then
   INTERVAL_steps=nint(NE_time_step_update_jump/RT_step)
   last_point    =NE_time_step_update_last_point
 endif
 !
 i_grp=nint(real(NE_i_time-last_point)/real(INTERVAL_steps))
 !
 ! Do we do it?
 !==============
 RT_do_it= (NE_i_time-last_point)==(i_grp*INTERVAL_steps) 
 !
 ! Do we STOP now ?
 !==================
 if (what=="OUT".and.RT_do_it) then
   call it_reset(0)
   call parser('STOP_NOW',STOP_the_dynamics_NOW)
 endif
 !
 ! Is the Dynamics over ?
 !========================
 if (RT_Dynamics_is_over( ).or.NE_i_time==NE_i_start_time) then
   last_point=-1
   INTERVAL_steps=-1
   RT_do_it=.TRUE.
   return
 endif
 !
end function
