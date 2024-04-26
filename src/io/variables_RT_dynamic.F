!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
integer function variables_RT_dynamic(ID)
 !
 use pars,           ONLY:SP,IP,CR_YIO
 use units,          ONLY:AUT2FS
 use RT_output_m,    ONLY:RT_desc,n_RT_desc_static,TIME_STEP_desc
 use RT_lifetimes,   ONLY:RT_life_extrap_times,RT_life_extrap_times
 use real_time,      ONLY:RT_dyn_step,NE_initial_time_step_update,NE_time_step_update_jump,&
&                         NE_time,NE_i_time,NE_time_step_update_jump_INPUT, &
&                         l_Update_Time_Step,NE_step_update_treshold,RT_MAX_step
 use IO_m,           ONLY:io_status
 use IO_int,         ONLY:io_variable_elemental,def_variable_elemental
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
&    VAR=" [RT] Time step(now)",R0=RT_dyn_step,DB_R0=RT_dyn_step,WARN=.true.,OP=(/"=="/),TERMINATOR="[fs]",UNIT=AUT2FS,&
&    DESCRIPTOR=RT_desc)
 call io_variable_elemental(ID,&
&    VAR=" [RT] NE time(done)",R0=NE_time,WARN=.true.,OP=(/"=="/),UNIT=AUT2FS,DESCRIPTOR=RT_desc,TERMINATOR="[fs]")
 call io_variable_elemental(ID,&
&    VAR=" [RT] NE steps(done)",I0=NE_i_time,WARN=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,TERMINATOR="[fs]")
 !
 call io_variable_elemental(ID,VAR=" [RT] Lif Extr Inte/Intr(now)",R1=RT_life_extrap_times,DB_R1=RT_life_extrap_times,&
&    DESCRIPTOR=RT_desc,UNIT=AUT2FS,TERMINATOR="[fs]")
 !
 ! Delta T Update
 !   
 call def_variable_elemental(ID,VAR="DELTA_T_UPDATE",VAR_SZ=6,VAR_KIND=SP,MENU=0) 
 call io_variable_elemental(ID,L0=l_Update_Time_Step,DB_L0=l_disk_flag,&
&    VAR=' [dT] Time-Dependent Time step',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS)
 if (l_disk_flag) then
   call io_variable_elemental(ID,R0=NE_initial_time_step_update,&
&    VAR=' [dT] Starting Time',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS,TERMINATOR="[fs]")
   call io_variable_elemental(ID,R0=NE_time_step_update_jump_INPUT,&
&    VAR=' [dT] Update interval',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS,TERMINATOR="[fs]")
   call io_variable_elemental(ID,R0=NE_step_update_treshold,&
&    VAR=' [dT] Update interval tresh',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,TERMINATOR="[o/o]")
   call io_variable_elemental(ID,R0=RT_MAX_step,&
&    VAR=' [dT] Max step in dynamics',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=RT_desc,UNIT=AUT2FS,TERMINATOR="[fs]")
 endif
 ! ... for RESTART purposes...
 call io_variable_elemental(ID,R0=NE_time_step_update_jump, DB_R0=NE_time_step_update_jump,CHECK=.FALSE.)
 !
 variables_RT_dynamic=io_descriptors(TIME_STEP_desc,"TIME_STEP",ID,.FALSE.)
 !
 call def_variable_elemental(ID,VAR="",VAR_SZ=0,VAR_KIND=CR_YIO,MENU=1)
 !
 variables_RT_dynamic      =io_status(ID)
 !
end function variables_RT_dynamic
