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
logical function RT_Dynamics_is_over()
 !
 use pars,           ONLY:SP
 use parser_m,       ONLY:parser
 use real_time,      ONLY:NE_steps,NE_i_time,RT_dyn_step,NE_time,NE_tot_time,RT_step
 use RT_control,     ONLY:RT_NAN_found
 use it_tools,       ONLY:it_reset
 !
 implicit none
 !
 logical    ::STOP_now
 real(SP)   ::dT_left,dT_right      
 !
 call it_reset(0)
 call parser('STOP_NOW',STOP_now)
 !
 RT_Dynamics_is_over=.FALSE.
 !
 if (STOP_now.or.RT_NAN_found.or.NE_i_time>=NE_steps) then
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
