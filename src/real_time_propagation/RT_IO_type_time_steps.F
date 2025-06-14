!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM 
!
subroutine RT_IO_type_time_steps(what)
 !
 use real_time,      ONLY:RT_step,RT_dyn_step
 use RT_control,     ONLY:CARR_RT_IO_t,OBS_RT_IO_t,RT_IO_t,Gless_RESTART_RT_IO_t,OUTPUT_RT_IO_t,&
&                         SETUP_RT_IO_type,CACHE_OBS_INTERVAL_time_INPUT,TIME_adjust,&
&                         CACHE_OBS_INTERVAL_time,CACHE_OBS_steps,RT_control_free,&
&                         RT_control_alloc,Vbands_RT_IO_T,Gless_RT_IO_t,SAVE_G_history
 ! 
 implicit none
 !
 character(*) :: what
 !
 if (what=="INIT".or.what=="UPDATE".or.what=="RESTART") then
   !
   call SETUP_RT_IO_type(CARR_RT_IO_t,who="CARR",what=what)
   call SETUP_RT_IO_type(OUTPUT_RT_IO_t,who="OUTPUT",what=what)
   call SETUP_RT_IO_type(Gless_RESTART_RT_IO_t,who="GF",what=what)
   call SETUP_RT_IO_type(Vbands_RT_IO_t,who="Vb",what=what)
   call SETUP_RT_IO_type(OBS_RT_IO_t,who="OBS",what=what)
   if (SAVE_G_history) call SETUP_RT_IO_type(Gless_RT_IO_t,who="GF_history",what=what)
   !
 endif
 !
 if (what=="INIT") then
   !
   CACHE_OBS_INTERVAL_time_INPUT=max(TIME_adjust(CACHE_OBS_INTERVAL_time_INPUT,RT_step),RT_step)
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
   CACHE_OBS_INTERVAL_time = max(TIME_adjust(CACHE_OBS_INTERVAL_time,RT_dyn_step),RT_dyn_step)
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
