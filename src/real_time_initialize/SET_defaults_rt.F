!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SET_defaults_rt(INSTR,IND,OD,COM_DIR)
 !
 use pars,       ONLY:SP,cZERO
 use C_driver,   ONLY:code_branch
 use units,      ONLY:FS2AUT,HA2EV
 use RT_output_m,ONLY:n_RT_headings,N_RT_o_files,RT_o_file_long,RT_o_file_short,RT_o_file_is_open,&
&                     RT_headings,YPP_append,Gr_desc,TIME_STEP_desc,RT_desc
 use RT_control, ONLY:CARR_RT_IO_t,OBS_RT_IO_t,OUTPUT_RT_IO_t,Gless_RESTART_RT_IO_t,SAVE_G_history,RT_ctl_db,&
&                     CACHE_OBS_last_point,CACHE_OBS_INTERVAL_time,CACHE_OBS_INTERVAL_time_INPUT,   &
&                     CACHE_OBS_steps,CACHE_OBS_steps_now,RT_NAN_found
 use RT_lifetimes,ONLY:RT_life_extrap_times
 use real_time,  ONLY:RT_step,NE_steps,NE_time,Integrator_name,RT_deph_range,&
&                     Gr_kind,two_alpha,RAD_LifeTime,Phase_LifeTime,&
&                     NE_tot_time,NE_MEM_treshold,RT_eh_en,l_update_SOC,&
&                     DbGd_EE_percent,RT_scatt_tresh,integrator_step,RT_nk,&
&                     i_MEM_old,i_MEM_prev,i_MEM_now,G_MEM_steps,RT_step_manual_prefactor,&
&                     RT_deph_deg_thresh,RT_deph_cv_only,NE_time_step_update_manual,&
&                     NE_time_step_update_last_point,NE_time_step_update_jump,NE_time_step_update_jump_INPUT, &
&                     NE_initial_time_step_update,NE_step_update_treshold
 use fields,     ONLY:Efield,Efield_reset,n_ext_fields_max,n_ext_fields,A_vecpot_reset,   &
&                     A_tot,A_ind,A_ext
 use plasma,     ONLY:PLASMA_redux_percent
 use descriptors,ONLY:IO_desc_reset
 !
 implicit none
 !
 character(*) :: INSTR,IND,OD,COM_DIR
 !
 ! Work Space 
 !
 integer           :: i1,i2
 !
 ! Printed reals format lengths 
 !
 include 'branch.inc'
 !
 ! RT control
 !
 RT_ctl_db="none"
 !
 ! Real Time
 !
 n_ext_fields=0
 do i1=1,n_ext_fields_max
   call Efield_reset(Efield(i1))
   Efield(i1)%t_initial=RT_step
 enddo
 !
 call A_vecpot_reset(A_tot)
 call A_vecpot_reset(A_ext)
 call A_vecpot_reset(A_ind)
 !
 ! RT dynamics 
 !
 l_update_SOC=.FALSE.
 !
 ! RT current 
 !
 RT_step_manual_prefactor=0._SP
 RT_step=0.010*FS2AUT
 NE_steps=1
 NE_time=0._SP
 NE_tot_time= 1000._SP*FS2AUT
 !
 NE_time_step_update_last_point = 1
 NE_time_step_update_jump       = 0._SP
 NE_time_step_update_jump_INPUT  = 0._SP
 NE_initial_time_step_update    = 0._SP
 NE_step_update_treshold        = 0.1_SP
 NE_time_step_update_manual     =-1.0_SP
 !
 CACHE_OBS_steps      = 0
 CACHE_OBS_steps_now  = 0
 CACHE_OBS_last_point = 0
 CACHE_OBS_INTERVAL_time       = 0._SP
 CACHE_OBS_INTERVAL_time_INPUT  = 0._SP
 !
 G_MEM_steps = 2
 i_MEM_old   = 0
 i_MEM_prev  = 0
 i_MEM_now   = 1
 !
 integrator_step=1
 Integrator_name='RK2'
 !
 RAD_LifeTime = 0._SP
#if defined _NL
 RAD_LifeTime = -1._SP*FS2AUT  ! Automatic set equal to the dephasing
#endif
 Phase_LifeTime = 0._SP
 RT_deph_deg_thresh = 1.E-5_SP
 RT_deph_cv_only =.FALSE.
 RT_deph_range=-1._SP
 !
 NE_MEM_treshold=0._SP
 RT_nk=0
 RT_eh_en=0._SP
 RT_scatt_tresh=-1._SP/HA2EV
 PLASMA_redux_percent=100._SP
 DbGd_EE_percent=0._SP
 !
 ! Scattering
 !
 RT_life_extrap_times(1)=-1._SP*FS2AUT
 RT_life_extrap_times(2)=-1._SP*FS2AUT
 !
 ! G_retarded
 !
 Gr_kind="HS"
 two_alpha=1
 !
 ! RT_ctl
 !
 OBS_RT_IO_t%INTERVAL_time_INPUT              =2._SP*FS2AUT
 CARR_RT_IO_t%INTERVAL_time_INPUT             =2._SP*FS2AUT
 OUTPUT_RT_IO_t%INTERVAL_time_INPUT           =5._SP*FS2AUT
 Gless_RESTART_RT_IO_t%INTERVAL_time_INPUT    =1._SP*FS2AUT
 SAVE_G_history=.FALSE.
 RT_NAN_found  =.FALSE.
 !
 ! RT_output
 !
 n_RT_headings=0
 N_RT_o_files=0
 RT_o_file_is_open=.FALSE.
 RT_o_file_long= " "
 RT_o_file_short= " "
 RT_headings= " "
 YPP_append=" "
 !
 call IO_desc_reset(RT_desc)
 call IO_desc_reset(Gr_desc)
 call IO_desc_reset(TIME_STEP_desc)
 !
end subroutine
