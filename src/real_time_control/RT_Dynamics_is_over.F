!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
logical function RT_Dynamics_is_over()
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:NE_steps,NE_i_time,RT_dyn_step,NE_time,NE_tot_time,RT_step
 use RT_control,     ONLY:RT_NAN_found,STOP_the_dynamics_NOW
 !
 implicit none
 !
 real(SP)   ::dT_left,dT_right      
 !
 RT_Dynamics_is_over=.FALSE.
 !
 if (STOP_the_dynamics_NOW.or.RT_NAN_found.or.NE_i_time>=NE_steps) then
   RT_Dynamics_is_over=.TRUE.
   return
 endif
 !
 !-------------t---------T---------t'------------------
 !             | dT_left | dT_right|
 !
 ! T =NE_tot_time
 ! t =NE_time
 ! t'=NE_time+RT_dyn_step
 !
 dT_right=NE_time+RT_dyn_step-NE_tot_time
 dT_left =NE_tot_time-NE_time
 !
 !-------------t'------------T--
 !             | dT_right <0 | 
 !
 if (dT_right<0._SP) return
 !
 ! t' beyond NE_steps
 !
 if ( NE_i_time+nint(RT_dyn_step/RT_step) > NE_steps) then
   RT_Dynamics_is_over=.TRUE.
   return
 endif
 !
 !-------------t---------T-------------t'------------------
 !             | dT_left < dT_right    |
 !
 if (abs(dT_left)<dT_right) then
   RT_Dynamics_is_over=.TRUE.
   return
 endif
 !
end function
