!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SET_defaults(INSTR,IND,OD,COM_DIR)
 !
 use debug,      ONLY:dbg
 use pars,       ONLY:SP,cZERO
 use C_driver,   ONLY:code_branch
 use units,      ONLY:HA2EV,FS2AUT,kWCMm22AU
 use LOGO,       ONLY:ID_logo,ID_logo_stderr
 use global_XC,  ONLY:EXT_NONE
 use X_m,        ONLY:current_iq,self_detect_E_range,X_FILL_UP_matrix_only,eps_2_alpha,l_drude,&
&                     alpha_dim,use_X_DbGd,X_DbGd_nkpts,Chi_mode,skip_cond_sum_rule,&
&                     q_plus_G_direction,Q_Shift_Order,&
&                     l_X_terminator,X_terminator_E,global_gauge,&
&                     Chi_linalg_mode,X_use_lin_sys,X_use_gpu,X_DbGd_percentual
 use QP_m,       ONLY:QP_dSc_steps,QP_n_W_freqs,QP_G_Zoom_treshold,&
&                     QP_dSc_test,QP_solver,QP_G_damp,QP_dSc_delta,&
&                     QP_cg_percent,QP_n_states,SC_E_threshold, &
&                     QP_Sc_steps,QP_G_er,QP_G_ir,QP_G_dr,SC_band_mixing,QP_G_solver,&
&                     COHSEX_use_empties,On_Mass_Shell_approx,&
&                     Vnlxc_kind,Vxc_kind,l_extended_output,l_GW_terminator,&
&                     GW_terminator_E
 use QP_CTL_m,   ONLY:QP_ctl_user,QP_ctl_DB_user
 use functions,  ONLY:bose_E_cut
 use D_lattice,  ONLY:i_space_inv,inv_index,n_atoms_species_max,n_atomic_species,mag_syms,&
&                     input_Tel_is_negative,non_periodic_directions,lattice,Bose_Temp,    &
&                     molecule_position,l_0D,l_1D,l_2D,l_3D
 use pseudo,     ONLY:pp_n_l_times_proj_max,pp_kbv_dim
 use R_lattice,  ONLY:n_g_shells,ng_closed,bse_scattering,coll_scattering,&
&                     Xk_grid_is_uniform,RIM_id_epsm1_reference,RIM_epsm1,&
&                     RIM_anisotropy,RIM_ng,RIM_W_ng,RIM_n_rand_pts,nqibz,q0_def_norm,&
&                     cutoff_presets,k_map,qindx_S_max_Go,RIMW_type
 use electrons,  ONLY:n_spin,n_sp_pol,n_spinor,filled_tresh,l_spin_orbit,n_spin_den,eval_magn
 use parallel_m, ONLY:ncpu,CPU_str_reset,CREATE_hosts_COMM
 use com,        ONLY:isec,depth,secnm,previous_secmode,of_name,n_ofs_defined,of_opened,of_unit,max_open_ofs,&
&                     more_io_path,core_io_path,com_path,repfile,grid_paths,fat_log,exp_user
 use stderr,     ONLY:win_size,tty_size,logfile,set_real_printed_length,log_as_a_file
 use LIVE_t,     ONLY:log_line_to_dump,log_line,nhash,ct
 use wave_func,  ONLY:wf_ng,wf_norm_test,wf_nb_io,wf_nb_io_groups,WF,WF_buffer,WF_buffered_IO
 use FFT_m,      ONLY:fft_dim_loaded,fft_size,fft_dim,fft_multiplier
 use IO_m,       ONLY:io_reset,max_io_units,serial_number,frag_WF
 use BS_solvers, ONLY:BSS_mode,BSS_n_freqs,BSS_er,BSS_dr,BSS_P_dir,BSS_E_dir,BSS_Q_dir,BSS_first_eig,&
&                     Haydock_threshold,Haydock_iterIO,Haydock_iterMAX,BSS_uses_DbGd,BSS_Wd,BSS_n_eig,&
&                     BSS_damp_reference,BSS_Vnl_included,BSS_uses_GreenF,BSS_inversion_mode,BSS_target_E,&
&                     BSS_perturbative_width,K_INV_EPS,K_INV_PL,K_INV_PI_PH,BSS_desc,BSS_ydiago_solver,BSS_trange_E
 use descriptors,ONLY:IO_desc_reset
#if defined _SLEPC
 use BS_solvers, ONLY:BSS_slepc_extraction,BSS_slepc_ncv,BSS_slepc_tol,BSS_slepc_maxit,&
 &                    BSS_slepc_precondition,BSS_slepc_approach,BSS_slepc_matrix_format,BSS_slepc_mpd
#endif
 use BS,         ONLY:BS_n_g_W,BS_eh_en,BS_identifier,BS_q,BS_eh_win,MAX_BSK_LIN_size,&
&                     BS_K_dim,BS_not_const_eh_f,BSK_mode,l_BSE_kernel_complete,&
&                     BS_K_is_ALDA,BSE_mode,BSE_prop,BSE_L_kind,BS_K_cutoff,BS_perturbative_SOC,BSK_IO_mode
 use TDDFT,      ONLY:FXC_type,FXC_n_g_corr,FXC_mode,&
&                     FXC_per_memstps,FXC_LRC_alpha,FXC_PF_alpha,FXC_LRC_beta,FXC_SVD_digits,&
&                     FXC_is_retarded,TDDFT_mode
 use ACFDT,      ONLY:ACFDT_n_lambda,ACFDT_n_freqs,ACFDT_E_range
 use zeros,      ONLY:zero_norm,k_iku_zero,k_rlu_zero,G_iku_zero,G_mod_zero,zero_dfl
 use y_memory,   ONLY:MEMs,LARGE_MEMs,N_MEM_max,N_MEM_SAVE_max,MEM_element_init
 use xc_functionals,  ONLY:GS_xc_FUNCTIONAL,GS_xc_KIND,GS_exx_FRACTION,GS_exx_SCREENING 
 use BS,         ONLY:l_BS_anomalous_Hall
 use PHOTOLUM,   ONLY:PL_weights
#if defined _YAML_OUTPUT
 use com,         ONLY:depth_yaml
#endif
 use timing_m,    ONLY:timing_allocate,nclockx
 use openmp,      ONLY:OPENMP_initialize,OPENMP_update,master_thread,omp_is_off
 use gpu_m,       ONLY:gpu_setup,have_gpu
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
 call set_real_printed_length()
 !
 ! Fat Log?
 !
 fat_log    = .FALSE.
 if (index(INSTR,'fatlog')>0) fat_log=.TRUE.
 !
 ! Experienced User
 !
 exp_user    = .FALSE.
 if (index(INSTR,'expuser')>0) exp_user=.TRUE.
 !
 ! CPU structures
 !
 call CPU_str_reset()
 !
 ! OpenMP
 !
#if defined _OPENMP
 omp_is_off    = .FALSE.
 if (index(INSTR,'noopenmp')>0) omp_is_off=.TRUE.
#else
 omp_is_off    = .TRUE.
#endif
 !
 call OPENMP_initialize( )
 call OPENMP_update(master_thread)
 !
 ! Node name
 !
 call CREATE_hosts_COMM( )
 !
 ! CUDA environment defs
 !
 call gpu_setup()
 !
 ! Stack Size 
 !
 call remove_stack_limit()
 !
 ! Debug
 !
 call dbg('reset')
 !
 ! Clocks
 !
 call timing_allocate(nclockx)
 !
 call MEM_element_init(MEMs,N_MEM_max)
 call MEM_element_init(LARGE_MEMs,N_MEM_SAVE_max)
 !
 ! ZEROs 
 !
 zero_norm =zero_dfl
 k_iku_zero=zero_dfl
 k_rlu_zero=zero_dfl
 G_iku_zero=zero_dfl
 G_mod_zero=1.E-5
 !
 ! PATHS  ...
 !
 core_io_path=IND
 more_io_path=OD
 com_path=COM_DIR
 !
 ! WFs fragmentation, this is for the interfaces
 !
 frag_WF = (.not.index(INSTR,'nodbfr')>0 .or. index(INSTR,'fragnb')>0)
 !
 ! Logical Setup (Mainly for interfaces and ypp. As far as yambo is concerned this call is done in init.F)
 !
 call SET_logicals()
 !
 ! TTY size 
 !
 call win_size(tty_size)
 call ct(INIT=.TRUE.)
 log_as_a_file=ncpu>1.or.tty_size<0
 !
 !I/O 
 !
 serial_number=0
 !
 !com
 !
 isec=0
 depth=-1
 secnm=' '
 previous_secmode=' '
 of_name=' '
 n_ofs_defined=0
 of_opened=' '
 of_unit=0
 of_unit(max_open_ofs)=-1
#if defined _YAML_OUTPUT
 depth_yaml=0
#endif
 !
 !LOGO
 !
 ID_logo=-1
 ID_logo_stderr=-1
 !
 !LOG/REP files
 !
 repfile=" "
 logfile=" "
 !
 !Timing
 !
 log_line_to_dump=.FALSE.
 log_line=' '
 nhash=40
 !
 !functions
 !
 bose_E_cut=0.1_SP
 !
 !D_lattice
 !
 input_Tel_is_negative=.FALSE.
 non_periodic_directions='none'
 molecule_position=0._SP
 lattice='Unknown'
 Bose_Temp=-1./HA2EV
 l_3D=.TRUE.
 l_2D=.FALSE.
 l_1D=.FALSE.
 l_0D=.FALSE.
 !
 ! R_lattice
 !
 n_g_shells=0
 nqibz=0
 ng_closed=0
 coll_scattering=.FALSE.
 bse_scattering=.FALSE.
 Xk_grid_is_uniform=.TRUE.
 q0_def_norm=1.E-5_SP
 k_map%g0_idx  =-1
 k_map%q_step  =-1
 k_map%max_kdir= 0
 qindx_S_max_Go=-1
 !
 ! RIM
 !
 RIM_id_epsm1_reference=0
 RIM_epsm1=0.
 RIM_anisotropy=0._SP
 RIM_ng=0
 RIM_n_rand_pts=0
 !
 ! RIM-W
 !
 RIM_W_ng=0
 RIMW_type="default"
 !
 ! CUTOFF
 !
 call cutoff_presets()
 !
 ! D_lattice 
 !
 n_atoms_species_max=0
 n_atomic_species=0
 i_space_inv=-1
 inv_index=0
 mag_syms=.FALSE.
 !
 ! Pseudo
 !
 pp_n_l_times_proj_max=0
 pp_kbv_dim=0
 !
 ! Electrons
 !
 n_spin=1
 n_sp_pol=1
 n_spinor=1
 n_spin_den=1
 l_spin_orbit       = .FALSE.
 filled_tresh       =0.00001_SP
 !
 ! Magnetization and density
 !
 eval_magn      = .FALSE.
 !
 !wave_func
 !
 WF%b=0
 WF%k=0
 WF%space=' '
 WF_buffer%b=0
 WF_buffer%k=0
 WF_buffer%space=' '
 WF_buffered_IO=.FALSE.
 wf_ng=0
 wf_norm_test=.TRUE.
 wf_nb_io=0
 wf_nb_io_groups=1
 !
 !FFT
 !
 fft_dim_loaded=0
 fft_size=0
 fft_dim=0
 fft_multiplier=(/1,1,1/)
 !
 do i1=1,max_io_units
   call io_reset(i1)
 enddo
 !
 !X
 !
 Chi_mode=' '
 Chi_linalg_mode="lin_sys"
 X_use_lin_sys=.FALSE.
 X_use_gpu=have_gpu
 current_iq=0
 X_DbGd_nkpts=0
 self_detect_E_range=.FALSE.
 X_FILL_UP_matrix_only=.FALSE.
 use_X_DbGd=.FALSE.
 X_DbGd_percentual=-1.
 eps_2_alpha=1._SP
 alpha_dim='adim'
 global_gauge='length'
 grid_paths=' '
 skip_cond_sum_rule=.FALSE.
 q_plus_G_direction=0._SP
 Q_Shift_Order=1
 l_X_terminator=.FALSE.
 X_terminator_E=40._SP/HA2EV
 !
 !QPm
 !
 QP_n_states=0
 QP_dSc_steps=2
 QP_G_Zoom_treshold=0._SP
 QP_Sc_steps=100
 QP_n_W_freqs=100
 QP_dSc_test=.FALSE.
 QP_solver=' '
 QP_G_damp=0.1/HA2EV
 QP_dSc_delta=0.1/HA2EV
 QP_G_solver='ra'
 QP_G_er=(/-10._SP/HA2EV,10._SP/HA2EV/)
 QP_G_ir=(/0._SP/HA2EV,0._SP/HA2EV/)
 QP_G_dr=0.1/HA2EV
 QP_cg_percent=100._SP
 COHSEX_use_empties=.FALSE.
 On_Mass_Shell_approx=.FALSE.
 SC_E_threshold=0.01/HA2EV
 SC_band_mixing=100._SP
 Vnlxc_kind='Fock'
 Vxc_kind='DFT'
 l_extended_output=.FALSE.
 l_GW_terminator=.FALSE.
 GW_terminator_E=1.5_SP
 !
 ! QP_ctl control
 !
 do i1=1,3
   QP_ctl_DB_user(i1)%INTERP_shell_factor=20._SP
   QP_ctl_DB_user(i1)%INTERP_N_neigh=1
   QP_ctl_DB_user(i1)%INTERP_DbGd_mode="NN"
   QP_ctl_DB_user(i1)%action="none"
   do i2=1,3
     QP_ctl_user(i1,i2)%E=(/0.,1.,1./)
     QP_ctl_user(i1,i2)%Wc=0._SP
     QP_ctl_user(i1,i2)%Wv=0._SP
     QP_ctl_user(i1,i2)%Wc_E_ref=0._SP
     QP_ctl_user(i1,i2)%Wv_E_ref=0._SP
     QP_ctl_user(i1,i2)%Wc_dos=0._SP
     QP_ctl_user(i1,i2)%Wv_dos=0._SP
     QP_ctl_user(i1,i2)%Z=(1._SP,0._SP)
   enddo
 enddo
 !
 ! BS/BSS
 !
 BS_n_g_W=-1
 BS_eh_en=(/-1._SP,-1._SP/)/HA2EV
 BS_identifier=0
 BS_q=1
 BS_eh_win=100._SP
 BSE_L_kind='BAR'
 BSE_mode='resonant'
 BSE_prop='abs'
 BSK_mode=' '
 BSK_IO_mode='2D_standard'
 l_BSE_kernel_complete=.false.
 TDDFT_mode=' '
 FXC_mode="G-def"
 BSS_Wd=cZERO
 l_drude=.false.
 BS_K_dim=0
 BS_K_cutoff=0.0000_SP
 BSS_mode=' '
 BSS_inversion_mode='pf'
 BSS_ydiago_solver='s'
#if defined _ELPA
 BSS_ydiago_solver='e'
#endif
 BSS_er=(/0._SP,10._SP/)/HA2EV
 BSS_dr=0.1_SP/HA2EV
 BSS_E_dir=(/1._SP,0._SP,0._SP/)
 BSS_P_dir=(/0._SP,1._SP,0._SP/)
 BSS_Q_dir=(/0._SP,0._SP,1._SP/)
 BSS_uses_DbGd=.FALSE.
 BSS_damp_reference=0._SP
 BS_K_is_ALDA=.FALSE.
 BS_not_const_eh_f=.FALSE.
 BS_perturbative_SOC=.FALSE.
 Haydock_threshold=-0.02_SP
 Haydock_iterMAX=1000
 Haydock_iterIO=100
 BSS_Vnl_included=.FALSE.
 BSS_uses_GreenF=.FALSE.
 BSS_perturbative_width=.FALSE.
 K_INV_PI_PH%what = "PH_PI"
 K_INV_PL%what     = "PLS"
 K_INV_PL%treshold =0.5_SP
 K_INV_EPS%what    = "EPS"
 K_INV_EPS%treshold=0.01_SP
 K_INV_EPS%max_treshold=40._SP
 BSS_n_freqs=100
 BSS_n_eig=0
 BSS_first_eig=1
 BSS_target_E=0._SP
 BSS_trange_E=-1._SP/HA2EV
 MAX_BSK_LIN_size=45000*45000 ! 46341 is the sqare root of the maximum integer in IP: 2147483647
 !
 call IO_desc_reset(BSS_desc)
 !
#if defined _SLEPC
 BSS_slepc_matrix_format='shell'
 BSS_slepc_approach='none'
 BSS_slepc_precondition='none'
 BSS_slepc_extraction='ritz'
 BSS_slepc_maxit=0
 BSS_slepc_ncv=0
 BSS_slepc_mpd=0
 BSS_slepc_tol=1E-6_SP
#endif
 !
 ! TDDFT
 !
 FXC_type='rpa'
 FXC_n_g_corr=1
 FXC_per_memstps=100._SP
 FXC_LRC_alpha=0._SP
 FXC_PF_alpha="CUR"
 FXC_LRC_beta=0._SP
 FXC_SVD_digits=0
 FXC_is_retarded=.FALSE.
 !
 ! ACFDT
 !
 ACFDT_n_lambda=1
 ACFDT_n_freqs=10
 ACFDT_E_range=(/100.,1000./)/HA2EV
 !
 ! xc_functionals  
 !
 GS_xc_FUNCTIONAL=-1             ! unknow 
 GS_xc_KIND=-1                   ! unknow 
 GS_exx_FRACTION=0.0             ! no EXX part  
 GS_exx_SCREENING=0.0            ! no screening
 !
 l_BS_anomalous_Hall=.false.
 PL_weights=(/1._SP,1._SP,1._SP/)
 !
end subroutine
