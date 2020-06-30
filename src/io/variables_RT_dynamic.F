!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
integer function variables_RT_dynamic(ID)
 !
 use pars,           ONLY:SP,IP
 use units,          ONLY:AUT2FS
 use RT_output_m,    ONLY:RT_desc,n_RT_desc_static,TIME_STEP_desc
 use real_time,      ONLY:RT_dyn_step, &
&                         NE_initial_time_step_update,NE_time_step_update_jump,&
&                         NE_time,NE_i_time,&
&                         NE_time_step_update_jump_SAVE, &
&                         l_Update_Time_Step,&
&                         NE_step_update_treshold,RT_MAX_step,RT_life_extrap_times
 use IO_m,           ONLY:io_variable_elemental,def_variable_elemental,io_status
 !
 implicit none
 !
 integer            :: ID
 !
 logical            :: l_disk_flag
 integer, external  :: io_descriptors
 !
 RT_desc%n=n_RT_desc_static
 !
 ! RT_DYNAMICS
 !
 call def_variable_elemental(ID,VAR="RT_TIMEs_NOW",VAR_SZ=5,VAR_KIND=SP,MENU=0) 
 call io_variable_elemental(ID,&
&    VAR=" [RT] Time step         (now)[fs]",R0=RT_dyn_step,DB_R0=RT_dyn_step,WARN=.true.,OP=(/"=="/),UNIT=AUT2FS,&
&    DESCRIPTOR=RT_desc)
 call io_variable_elemental(ID,&
&    VAR=" [RT] NE time          (done)[fs]",R0=NE_time,WARN=.true.,OP=(/"=="/),UNIT=AUT2FS,DESCRIPTOR=RT_desc)
 call io_variable_elemental(ID,&
&    VAR=" [RT] NE steps         (done)[fs]",I0=NE_i_time,WARN=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
 !
 ! GPL_EXCLUDE_START
 !
 call io_variable_elemental(ID,&
&    VAR=" [RT] Lif Extr Inte/Intr(now)[fs]",R1=RT_life_extrap_times,DB_R1=RT_life_extrap_times,&
&    DESCRIPTOR=RT_desc,UNIT=AUT2FS)
 !
 ! Delta T Update
 !   
 call def_variable_elemental(ID,VAR="DELTA_T_UPDATE",VAR_SZ=6,VAR_KIND=SP,MENU=0) 
 call io_variable_elemental(ID,L0=l_Update_Time_Step,DB_L0=l_disk_flag,&
&    VAR=' [dT] Time-Dependent Time step   ',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS)
 if (l_disk_flag) then
   call io_variable_elemental(ID,R0=NE_initial_time_step_update,&
&    VAR=' [dT] Starting Time          [fs]',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS)
   call io_variable_elemental(ID,R0=NE_time_step_update_jump_SAVE,&
&    VAR=' [dT] Update interval        [fs]',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS)
   call io_variable_elemental(ID,R0=NE_step_update_treshold,&
&    VAR=' [dT] Update interval tresh [o/o]',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc)
   call io_variable_elemental(ID,R0=RT_MAX_step,&
&    VAR=' [dT] Max step in dynamics   [fs]',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS)
 endif
 ! ... for RESTART purposes...
 call io_variable_elemental(ID,R0=NE_time_step_update_jump, DB_R0=NE_time_step_update_jump,CHECK=.FALSE.)
 !
 variables_RT_dynamic=io_descriptors(TIME_STEP_desc,"TIME_STEP",ID,.FALSE.)
 !
 variables_RT_dynamic      =io_status(ID)
 !
end function variables_RT_dynamic
