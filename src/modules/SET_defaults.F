!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine SET_defaults(INSTR,IND,OD,COM_DIR)
 !
 ! GPL_EXCLUDE_START
 use debug,      ONLY:dbg
 ! GPL_EXCLUDE_END
 use C_driver,   ONLY:code_branch
 use pars,       ONLY:SP,cZERO
 use units,      ONLY:HA2EV,FS2AUT,kWCMm22AU
 use LOGO,       ONLY:ID_logo,ID_logo_stderr
 use X_m,        ONLY:current_iq,self_detect_E_range,X_FILL_UP_matrix_only,eps_2_alpha,l_drude,&
&                     alpha_dim,use_X_DbGd,X_DbGd_nkpts,Chi_mode,skip_cond_sum_rule,&
&                     eval_alpha,&
&                     q_plus_G_direction,Q_Shift_Order,n_X_descs,&
&                     l_X_terminator,X_terminator_E,global_gauge,&
&                     Chi_linalg_mode,X_use_lin_sys,X_DbGd_percentual
 use DIPOLES,    ONLY:use_covariant_approach,use_real_space_approach,&
&                     Vnl_commutator_warning,use_shifted_grids_approach,use_g_space_approach
 use QP_m,       ONLY:QP_dSc_steps,QP_n_W_freqs,QP_G_Zoom_treshold,&
&                     QP_dSc_test,QP_solver,QP_G_damp,QP_dSc_delta,&
&                     QP_cg_percent,QP_n_states,SC_E_threshold, &
&                     QP_Sc_steps,QP_G_er,QP_G_dr,SC_band_mixing,&
&                     GWo_SC_done,GWo_iterations,&
&                     COHSEX_use_empties,On_Mass_Shell_approx,&
&                     Vnlxc_kind,Vxc_kind,l_extended_output,l_GW_terminator,&
&                     GW_terminator_E
 use QP_CTL_m,   ONLY:QP_ctl_user,QP_ctl_db,QP_ctl_interp_neigh
 use functions,  ONLY:bose_E_cut
 use D_lattice,  ONLY:i_space_inv,inv_index,n_atoms_species_max,n_atomic_species,mag_syms,&
&                     input_Tel_is_negative,non_periodic_directions,lattice,Bose_Temp
 use pseudo,     ONLY:pp_n_l_times_proj_max,pp_kbv_dim
 use R_lattice,  ONLY:n_g_shells,ng_closed,bse_scattering,coll_scattering,&
&                     Xk_grid_is_uniform,RIM_id_epsm1_reference,RIM_epsm1,&
&                     RIM_anisotropy,RIM_ng,RIM_n_rand_pts,nqibz,q0_def_norm,&
&                     cutoff_presets,k_map
 use electrons,  ONLY:n_spin,n_sp_pol,n_spinor,filled_tresh,l_spin_orbit,n_spin_den,eval_magn
 use parallel_m, ONLY:ncpu,CPU_str_reset,PARALLEL_host_name
 use com,        ONLY:isec,depth,secnm,previous_secmode,of,opened_of,of_unit,max_open_ofs,&
&                     more_io_path,core_io_path,com_path,repfile,grid_paths
 use stderr,     ONLY:win_size,tty_size,logfile,set_real_printed_length,log_as_a_file
 use LIVE_t,     ONLY:log_line_to_dump,log_line,nhash,ct
 use wave_func,  ONLY:wf_ng,wf_norm_test,wf_nb_io,wf_nb_io_groups,WF,WF_buffer,WF_buffered_IO
 use FFT_m,      ONLY:fft_dim_loaded,fft_size,fft_dim,fft_multiplier
 use IO_m,       ONLY:io_reset,max_io_units,serial_number,mk_dir,frag_WF
 use BS_solvers, ONLY:BSS_mode,BSS_n_freqs,BSS_n_descs,BSS_er,BSS_dr,&
&                     BSS_q0,Haydock_threshold,Haydock_iterIO,BSS_uses_DbGd,BSS_Wd,&
&                     BSS_damp_reference,BSS_Vnl_included,BSS_uses_GreenF,BSS_inversion_mode,&
&                     BSS_add_antiresonant,BSS_perturbative_width,K_INV_EPS,K_INV_PL
#if defined _SLEPC
 use BS_solvers, ONLY:BSS_slepc_extraction, BSS_slepc_ncv, BSS_slepc_tol, BSS_target_E, Slepc_shell
#endif
 use BS,         ONLY:BS_n_g_W,BS_eh_en,BS_identifier,BS_q,BS_eh_win,&
&                     BS_res_mode,BS_K_dim,BS_cpl_mode,BS_not_const_eh_f,BSK_mode,&
&                     BS_K_is_ALDA,BSE_mode,L_kind,BS_perturbative_SOC,BS_K_has_been_calculated_loaded,&
&                     l_restart_bse
 use TDDFT,      ONLY:FXC_description,FXC_type,FXC_n_descs,FXC_n_g_corr,&
&                     FXC_per_memstps,FXC_LRC_alpha,FXC_PF_alpha,FXC_LRC_beta,FXC_SVD_digits,&
&                     FXC_is_retarded,TDDFT_mode
 use ACFDT,      ONLY:ACFDT_n_lambda,ACFDT_n_freqs,ACFDT_E_range
#if defined _ELPH 
 use ELPH,       ONLY:ph_modes,elph_nb,gsqF_energy_steps,eval_G_using_KK,&
&                     elph_Ham_bands,elph_Ham_ik,elph_branches,RES_tresh,FAN_deltaE_treshold,DW_deltaE_treshold
#endif
 use zeros,      ONLY:zero_norm,k_iku_zero,k_rlu_zero,G_iku_zero,zero_dfl
 use memory,     ONLY:MEMs,LARGE_MEMs,N_MEM_max,N_MEM_SAVE_max,MEM_element_init
 use xc_functionals,  ONLY:GS_xc_FUNCTIONAL,GS_xc_KIND,GS_exx_FRACTION,GS_exx_SCREENING 
#if defined _KERR
 use drivers,    ONLY:l_kerr,l_anomalous_Hall
#endif
#if defined _PL
 use PHOTOLUM,   ONLY:PL_weights
#endif
#if defined _SC 
 use SC,         ONLY:SC_iterations,SC_rho_threshold,SC_potential,OEP_approx,l_NSC_shot,&
&                     compatible_SC_DB,SC_cycle_mixing,SC_fft_size,found_SC_DB,&
&                     l_Mean_Potential,SC_up_W_iters,l_SC_diagonal,&
&                     l_SC_nl_mix
#endif
#if defined _SC || defined _RT || defined _QED || defined _NL
 use collision_ext,  ONLY:COLLISIONS_naming,COLLISIONS_cutoff
 use hamiltonian,    ONLY:H_potential
#endif
#if defined _RT || defined _YPP_RT
 use RT_output_m,ONLY:n_RT_headings,N_RT_o_files,RT_o_file_long,RT_o_file_short,RT_headings,YPP_append
 use RT_control, ONLY:OBS_RT_IO_t,OUTPUT_RT_IO_t,GF_RT_IO_t,SAVE_G_history,RT_ctl_db
 use real_time,  ONLY:RT_step,NE_steps,NE_time,Integrator_name,RT_life_extrap_times,      &
&                     Gr_kind,two_alpha,l_RT_CCA_Kernel,l_lftm_fit_temperatures,          &
&                     RAD_LifeTime,Phase_LifeTime,EP_abs_prefactor,                       &
&                     NE_tot_time,NE_MEM_treshold,EE_prefactor,EP_prefactor,              &
&                     RT_eh_en,l_lftm_fit_stable,Nfitted_lifetimes,RT_Tfit_lifetimes,     &
&                     RT_Efit_lifetimes,l_update_SOC,                                     &
&                     DbGd_EE_percent,RT_scatt_tresh,integrator_step,RT_nk,               &
&                     RAD_prefactor
 use fields,     ONLY:Efield,Efield_reset,n_ext_fields_max,n_ext_fields,A_vecpot_reset,   &
&                     A_tot,A_ind,A_ext
 use plasma,     ONLY:PLASMA_redux_percent
#endif
#if defined _NL
 use nl_optics,  ONLY:NL_correlation,NL_er,n_frequencies,NL_damping,NL_LRC_alpha,         &
&                     NL_verb_name,NL_bands,n_order
 use fields,     ONLY:Efield,Efield_reset
#endif
#if defined _MAGNETIC
 use magnetic,    ONLY:MAG_B,MAG_radius,MAG_hamiltonian_type,MAG_landau,MAG_pauli,&
&                      MAG_gauge,MAG_psi,MAG_theta,phase_trick
#endif
 use timing_m,    ONLY:timing_allocate,nclockx
 use openmp,      ONLY:OPENMP_initialize,OPENMP_update,master_thread,omp_is_off
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
 ! GPL_EXCLUDE_START
 include 'branch.inc'
 ! GPL_EXCLUDE_END
 !
 call set_real_printed_length()
 !
 ! CPU structures
 !
 call CPU_str_reset()
 !
 ! OpenMP
 !
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
 ! Stack Size 
 !
 call remove_stack_limit()
 !
 ! GPL_EXCLUDE_START
 ! Debug
 !
 call dbg('reset')
 ! GPL_EXCLUDE_END
 !
 ! Clocks
 !
 call timing_allocate(nclockx)
 !
 call MEM_element_init(MEMs,N_MEM_max)
 call MEM_element_init(LARGE_MEMs,N_MEM_SAVE_max)
 !
 ! Node name
 !
 call PARALLEL_host_name( )
 !
 ! ZEROs 
 !
 zero_norm =zero_dfl
 k_iku_zero=zero_dfl
 k_rlu_zero=zero_dfl
 G_iku_zero=zero_dfl
 !
 ! PATHS  ...
 !
 core_io_path=IND
 more_io_path=OD
 com_path=COM_DIR
 !
 ! ... created (core only)
 !
 call mk_dir(core_io_path)
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
 of=' '
 opened_of=' '
 of_unit=0
 of_unit(max_open_ofs)=-11
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
 lattice='Unknown'
 Bose_Temp=-1./HA2EV
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
 !
 ! RIM
 !
 RIM_id_epsm1_reference=0
 RIM_epsm1=0.
 RIM_anisotropy=0._SP
 RIM_ng=0
 RIM_n_rand_pts=0
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
 filled_tresh=0.00001
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
 current_iq=0
 X_DbGd_nkpts=0
 self_detect_E_range=.FALSE.
 X_FILL_UP_matrix_only=.FALSE.
 use_shifted_grids_approach = .FALSE.
 use_covariant_approach     = .FALSE.
 use_real_space_approach    = .FALSE.
 use_g_space_approach       = .TRUE.
 use_X_DbGd=.FALSE.
 X_DbGd_percentual=1.
 eps_2_alpha=1._SP
 alpha_dim='adim'
 global_gauge='length'
 grid_paths=' '
 skip_cond_sum_rule=.FALSE.
 eval_alpha=.FALSE.
 Vnl_commutator_warning=.FALSE.
 q_plus_G_direction=0._SP
 Q_Shift_Order=1
 n_X_descs=0
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
 QP_G_er=(/-10._SP/HA2EV,10._SP/HA2EV/)
 QP_G_dr=0.1/HA2EV
 QP_cg_percent=100._SP
 GWo_iterations=0
 GWo_SC_done=.FALSE.
 COHSEX_use_empties=.FALSE.
 On_Mass_Shell_approx=.FALSE.
 SC_E_threshold=0.01/HA2EV
 SC_band_mixing=100._SP
 Vnlxc_kind='HF'
 Vxc_kind='LDA'
 l_extended_output=.FALSE.
 l_GW_terminator=.FALSE.
 GW_terminator_E=1.5_SP
 !
 ! QP_ctl control
 !
 QP_ctl_db="none"
 QP_ctl_interp_neigh=1
 do i1=1,3
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
#if defined _RT
 !
 ! RT control
 !
 RT_ctl_db="none"
 !
#endif
 !
 ! BS/BSS
 !
 l_restart_bse = .true.
 BS_n_g_W=-1
 BS_eh_en=(/-1._SP,-1._SP/)/HA2EV
 BS_identifier=0
 BS_q=1
 BS_eh_win=100._SP
 BS_res_mode='xc'
 BS_cpl_mode='none'
 L_kind='Lbar'
 BSE_mode='retarded'
 BSK_mode=' '
 TDDFT_mode=' '
 BSS_add_antiresonant=.FALSE.
 BSS_Wd=cZERO
 l_drude=.false.
 BS_K_dim=0
 BSS_mode=' '
 BSS_inversion_mode='pf'
 BSS_n_descs=0
 BSS_er=(/0._SP,10._SP/)/HA2EV
 BSS_dr=0.1_SP/HA2EV
 BSS_q0=(/1._SP,0._SP,0._SP/)
 BSS_uses_DbGd=.FALSE.
 BSS_damp_reference=0._SP
 BS_K_is_ALDA=.FALSE.
 BS_not_const_eh_f=.FALSE.
 BS_perturbative_SOC=.FALSE.
 BS_K_has_been_calculated_loaded=.FALSE.
 Haydock_threshold=-0.02_SP
 Haydock_iterIO=100
 BSS_Vnl_included=.FALSE.
 BSS_uses_GreenF=.FALSE.
 BSS_perturbative_width=.FALSE.
 K_INV_PL%what     = "PLS"
 K_INV_PL%treshold =0.5_SP
 K_INV_EPS%what    = "EPS"
 K_INV_EPS%treshold=0.01_SP
 BSS_n_freqs=100
 !
#if defined _SLEPC
 BSS_slepc_extraction='ritz'
 BSS_slepc_ncv=0
 BSS_slepc_tol=1E-6_SP
 BSS_target_E=0
 Slepc_shell=.TRUE.
#endif
 !
 ! TDDFT
 !
 FXC_description=""
 FXC_type='rpa'
 FXC_n_descs=0
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
#if defined _KERR
 l_kerr=.false.
 l_anomalous_Hall=.false.
#endif
 !
#if defined _PL
 PL_weights=(/1._SP,1._SP,1._SP/)
#endif
 !
#if defined _ELPH 
 !
 ! ELPH
 !
 ph_modes=0
 elph_nb=0
 elph_branches=0
 gsqF_energy_steps=2
 eval_G_using_KK=.FALSE.
 elph_Ham_bands=0
 elph_Ham_ik=1
 RES_tresh=0.01_SP
 DW_deltaE_treshold=1.E-6_SP/HA2EV
 FAN_deltaE_treshold=1.E-6_SP/HA2EV
#endif
 !
#if defined _SC || defined _RT || defined _QED
 !
 call COLLISIONS_naming( 0 , "all" )
 !
 ! COLLISIONS I/O
 !
 COLLISIONS_cutoff=0.0005_SP
 H_potential="NONE"
 !
#endif
 !
#if defined _SC
 ! SC
 !
 SC_fft_size=0
 SC_iterations=100
 SC_cycle_mixing=0.5_SP
 SC_rho_threshold=1.E-5_SP
 SC_up_W_iters=0
 SC_potential='NONE'
 OEP_approx='n'
 compatible_SC_DB=.FALSE.
 found_SC_DB=.FALSE.
 l_Mean_Potential=.FALSE.
 l_SC_diagonal=.FALSE.
 l_SC_nl_mix =.FALSE.
 l_NSC_shot  =.FALSE.
 !
#endif
 !
#if defined _RT
 !
 ! Real Time
 !
 n_ext_fields=0
 do i1=1,n_ext_fields_max
   call Efield_reset(Efield(i1))
 enddo
 !
 call A_vecpot_reset(A_tot)
 call A_vecpot_reset(A_ext)
 call A_vecpot_reset(A_ind)
 !
 ! RT dynamics 
 !
 l_RT_CCA_Kernel=.FALSE.
 l_update_SOC=.FALSE.
 !
 ! RT current 
 !
 RT_step=0.010*FS2AUT
 NE_steps=1
 NE_time=0._SP
 NE_tot_time= 1000._SP*FS2AUT
 integrator_step=1
 Integrator_name='RK2'
 RAD_LifeTime = 0._SP
 Phase_LifeTime = 0._SP
 NE_MEM_treshold=0.
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
 ! FITs
 !
 l_lftm_fit_stable=.FALSE.
 l_lftm_fit_temperatures=.FALSE.
 Nfitted_lifetimes=0
 RT_Tfit_lifetimes=0._SP
 RT_Efit_lifetimes=0._SP
 RAD_prefactor=0._SP
 EE_prefactor=0._SP
 EP_prefactor=0._SP
 EP_abs_prefactor=0._SP
 !
 ! RT_ctl
 !
 OBS_RT_IO_t%INTERVAL_time=2._SP*FS2AUT
 OUTPUT_RT_IO_t%INTERVAL_time=5._SP*FS2AUT
 SAVE_G_history=.FALSE.
 GF_RT_IO_t%INTERVAL_time=1._SP*FS2AUT
 !
 ! RT_output
 !
 n_RT_headings=0
 N_RT_o_files=0
 RT_o_file_long= " "
 RT_o_file_short= " "
 RT_headings= " "
 YPP_append=" "
 !
#endif
 !
#if defined _NL
 !
 NE_tot_time   =  -1._SP*FS2AUT 
 Integrator_name='INVINT'
 NL_bands      =(/0,0/)
 NL_correlation="IPA"
 NL_er         =(/0.2_SP,8._SP/)/HA2EV
 n_frequencies =80
 n_order    = 0
 NL_damping    =0.2_SP/HA2EV
 NL_LRC_alpha  =0._SP
 call Efield_reset(Efield(1))
 Efield(1)%frequency=0.1_SP/HA2EV
 Efield(1)%intensity=1000._SP*kWCMm22AU
 Efield(1)%FWHM=2._SP*FS2AUT
 Efield(1)%ef_name="SOFTSIN"
 NL_verb_name='low'
 !
#endif
 !
#if defined _MAGNETIC
 !
 ! Magnetic
 !
 MAG_B=0._SP
 MAG_radius=0._SP
 MAG_hamiltonian_type=' '
 MAG_pauli=.FALSE.
 MAG_landau=.FALSE.
 MAG_gauge='SYMM'
 MAG_psi=0._SP
 MAG_theta=0._SP
 phase_trick=.FALSE.
 !
#endif
 !
end subroutine
