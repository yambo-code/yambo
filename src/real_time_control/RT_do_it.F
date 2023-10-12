!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
logical function RT_do_it(what)
 !
 use RT_control,     ONLY:OBS_RT_IO_t,Gless_RESTART_RT_IO_t,CARR_RT_IO_t,CACHE_OBS_INTERVAL_time,&
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
   INTERVAL_steps=Gless_RESTART_RT_IO_t%INTERVAL_steps
   last_point    =Gless_RESTART_RT_IO_t%last_point
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
 if (what=="G".and.RT_do_it) then
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
