!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_update_TIME_CONF_descriptions( )
 ! 
 use units,          ONLY:AUT2FS
 use real_time,      ONLY:NE_time,RT_life_extrap_times,RT_dyn_step,RT_is_dynamically_dephased,&
&                         NE_time_step_update_jump
 use RT_control,     ONLY:n_TIME_STEP_CONF_descriptions,TIME_STEP_CONF_description,&
&                         OBS_RT_IO_t,OUTPUT_RT_IO_t,GF_RT_IO_t
 use stderr,         ONLY:real2ch
 implicit none
 !
 ! Work Space
 !
 character(11) :: ch
 !
 ch=" [T="//trim(real2ch(NE_time*AUT2FS))
 !
 if (n_TIME_STEP_CONF_descriptions>94) return
 !
 n_TIME_STEP_CONF_descriptions=n_TIME_STEP_CONF_descriptions+1
 TIME_STEP_CONF_description(n_TIME_STEP_CONF_descriptions)=ch//"] Time step       [fs]:"//&
&                                                          trim(real2ch(RT_dyn_step*AUT2FS))
 !
 n_TIME_STEP_CONF_descriptions=n_TIME_STEP_CONF_descriptions+1
 TIME_STEP_CONF_description(n_TIME_STEP_CONF_descriptions)=ch//"] Update time     [fs]:"//&
&                                                          trim(real2ch(NE_time_step_update_jump*AUT2FS))
 !
 n_TIME_STEP_CONF_descriptions=n_TIME_STEP_CONF_descriptions+1
 TIME_STEP_CONF_description(n_TIME_STEP_CONF_descriptions)=ch//"] I/O steps(J-P-C)[fs]:"//&
&                                                          trim(real2ch(OBS_RT_IO_t%INTERVAL_time*AUT2FS))
 !
 n_TIME_STEP_CONF_descriptions=n_TIME_STEP_CONF_descriptions+1
 TIME_STEP_CONF_description(n_TIME_STEP_CONF_descriptions)=ch//"] I/O steps  (OUT)[fs]:"//&
&                                                          trim(real2ch(OUTPUT_RT_IO_t%INTERVAL_time*AUT2FS))
 !
 n_TIME_STEP_CONF_descriptions=n_TIME_STEP_CONF_descriptions+1
 TIME_STEP_CONF_description(n_TIME_STEP_CONF_descriptions)=ch//"] I/O steps   (GF)[fs]:"//&
&                                                          trim(real2ch(GF_RT_IO_t%INTERVAL_time*AUT2FS))
 !
 if (RT_is_dynamically_dephased) then
   !
   n_TIME_STEP_CONF_descriptions=n_TIME_STEP_CONF_descriptions+1
   TIME_STEP_CONF_description(n_TIME_STEP_CONF_descriptions)=ch//"] Life Inter-Intra[fs]:"//&
&                                                            trim(real2ch(RT_life_extrap_times(1)*AUT2FS))//" - "//&
&                                                            trim(real2ch(RT_life_extrap_times(2)*AUT2FS))
   !
 endif
 !
end subroutine RT_update_TIME_CONF_descriptions
