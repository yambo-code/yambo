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
&                         J_steps,J_time_step
 use pars,           ONLY:pi,SP
 !
 implicit none
 !
 ! Time Configuration
 !====================
 call RT_time_configuration_setup("CURRENT",J_time_step,X_rt%time_range,J_steps)
 !
 ! AM April 2016. I do not understand the need of introducing i_t_start,i_t_end, RED_steps. I think they were
 ! related to some partial use of J/P. But these can be out-of-date at the moment.
 !
 i_t_start=1
 i_t_end  =J_steps
 !
 ! DEBUG>
 !
 !X_rt%time_range=(/Efield(1)%t_initial,Total_time(2)/)
 !!
 !i_t_start=nint(X_rt%time_range(1)/J_time_step)+1
 !i_t_end  =nint(X_rt%time_range(2)/J_time_step)+1
 !!
 !i_t_field_range= (/Efield(1)%t_initial_indx,J_steps/)
 !!
 !if (N_pumps>0.and.trim(Efield(1)%ef_name)=='DELTA') i_t_start=i_t_field_range(1)
 !!
 !if ( i_t_start<i_t_field_range(1) ) i_t_start=i_t_field_range(1)
 !if ( i_t_end  >i_t_field_range(2) ) i_t_end  =i_t_field_range(2)
 !!
 !X_rt%time_range=(/i_t_start-1,i_t_end-1/)*J_time_step
 !J_steps        =i_t_end-i_t_start+1
 !!
 ! DEBUG<
 !
 ! Frequency step & dumping
 !==========================
 RED_steps=J_steps
 W_RT_induced_step=2._SP*pi/(real(RED_steps-1,SP)*J_time_step)  
 if(X_rt%damp_factor==0._SP) X_rt%damp_factor=W_RT_induced_step
 !
end subroutine
