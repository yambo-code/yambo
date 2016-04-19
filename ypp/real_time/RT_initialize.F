!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
subroutine RT_initialize()
 !
 use YPP,            ONLY:X_rt,i_t_start,i_t_end,&
&                         W_RT_induced_step,RED_steps,&
&                         N_pumps,J_steps,J_time_step
 use com,            ONLY:msg
 use pars,           ONLY:pi,SP
 use units,          ONLY:AUT2FS
 use fields,         ONLY:Efield
 !
 implicit none
 !
 ! Work Space
 !
 real(SP) :: Total_time(2)
 integer  :: i_t,i_t_field_range(2),i_Probe
 !
 ! Time Configuration
 !====================
 !
 call RT_time_configuration_setup("CURRENT",J_time_step,Total_time,J_steps)
 !
 i_Probe=1
 X_rt%time_range=(/Efield(i_Probe)%t_initial,Total_time(2)/)
 !
 i_t_start=nint(X_rt%time_range(1)/J_time_step)+1
 i_t_end  =nint(X_rt%time_range(2)/J_time_step)+1
 !
 i_t_field_range= (/Efield(i_Probe)%t_initial_indx,J_steps/)
 !
 if (N_pumps>0.and.trim(Efield(i_Probe)%ef_name)=='DELTA') i_t_start=i_t_field_range(1)
 !
 if ( i_t_start<i_t_field_range(1) ) i_t_start=i_t_field_range(1)
 if ( i_t_end  >i_t_field_range(2) ) i_t_end  =i_t_field_range(2)
 !
 X_rt%time_range=(/i_t_start-1,i_t_end-1/)*J_time_step
 !
 RED_steps=i_t_end-i_t_start+1
 W_RT_induced_step=2._SP*pi/(real(RED_steps-1,SP)*J_time_step)  ! Frequency step in real-time dynamics
 !
 call msg('s','[RT] Time range [fs]:',(/real(i_t_start-1),real(i_t_end-1)/)*J_time_step*AUT2FS)
 !
 if(X_rt%damp_factor==0._SP) X_rt%damp_factor=W_RT_induced_step
 !
end subroutine
