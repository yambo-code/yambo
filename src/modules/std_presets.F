!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team
!              http://www.yambo-code.org
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
subroutine std_presets(INSTR,IND,OD,JS,COM_DIR)
 !
 use drivers
 use pars,       ONLY:SP
 use units,      ONLY:HARTREE,FS2AUT,AU2Vmm
 use LOGO,       ONLY:ID_logo,ID_logo_stderr
 use fields,     ONLY:global_gauge,grid_path
 use X_m,        ONLY:current_iq,self_detect_E_range,half_X_mat_only,eps_2_alpha,&
&                      alpha_dim,use_X_RIM,X_RIM_nkpts,Dipole_bands_ordered,&
&                      eval_alpha,Vnl_commutator_warning,Dipole_uses_shifted_grids,&
&                      q_plus_G_direction,Q_Shift_Order
 use QP_m,       ONLY:QP_dSc_steps,QP_n_W_freqs,QP_G_Zoom_treshold,&
&                     QP_dSc_test,QP_solver,QP_G_damp,QP_dSc_delta,&
&                     QP_W_er,QP_W_dr,QP_cg_percent,QP_n_states,&
&                     QP_ctl_db,QP_ctl_interp_neigh,SC_E_threshold, &
&                     QP_Sc_steps,QP_G_er,QP_G_dr,SC_band_mixing,&
&                     QP_W_partially_done,GWo_SC_done,GWo_iterations,&
&                     COHSEX_use_empties,QP_ctl_E,QP_ctl_W,QP_ctl_Z,On_Mass_Shell_approx,&
&                     Vnlxc_kind,Vxc_kind,l_extended_output 
 use functions,  ONLY:bose_E_cut
 use D_lattice,  ONLY:i_space_inv,n_atoms_species_max,n_atomic_species,&
&                     input_Tel_commands_over_header,non_periodic_directions,lattice,Bose_Temp
 use pseudo,     ONLY:pp_n_l_comp_max,pp_kbv_dim
 use R_lattice,  ONLY:n_g_shells,ng_closed,bse_scattering,&
&                     Xk_grid_is_uniform,RIM_id_epsm1_reference,RIM_epsm1,&
&                     RIM_anisotropy,RIM_ng,RIM_n_rand_pts,nqibz,q0_def_norm,&
&                     cutoff_presets
 use electrons,  ONLY:n_spin,n_sp_pol,n_spinor,BZ_RIM_nbands,BZ_RIM_tot_nkpts,&
&                     filled_tresh, spin_orbit,Boltz_tau,Boltz_Efield
 use par_proc_m, ONLY:myid,master_cpu,ncpu,n_nodes,local_master_cpu
 use com,        ONLY:isec,depth,secnm,previous_secmode,of,opened_of,&
&                     of_unit,write_to_of,write_to_report,max_open_ofs,&
&                     more_io_path,core_io_path,jobstr,com_path
 use stderr,     ONLY:win_size,tty_size,write_to_log,logfile,set_real_printed_length
 use timing,     ONLY:log_line_to_dump,log_line,nhash,live_timing_is_on,&
&                     parallel_timing,ct
 use wave_func,  ONLY:wf_b,wf_k,wf_ng,wf_space,wf_norm_test,wf_nb_io,wf_nb_io_groups
#if defined _DISTRIBUTED 
 use WF_distribute, ONLY:Distributed_Memory
#endif
 use FFT_m,      ONLY:fft_dim_loaded,fft_size,fft_dim,fft_multiplier
 use IO_m,       ONLY:io_reset,max_io_units,IO_write,serial_number,IO_read,mk_dir,&
&                     Fragmented_IO,Fragmented_IO_save 
 use BS,         ONLY:BS_n_g_W,BS_eh_en,BS_identifier,BS_q,BS_eh_win,&
&                     BS_res_mode,BS_K_dim,BS_cpl_mode,BS_not_const_eh_f,&
&                     BSS_mode,BSS_n_freqs,BSS_n_descs,BSS_er,BSS_dr,&
&                     BSS_q0,Haydock_treshold,BS_K_is_ALDA,BSS_uses_RIM,BSS_damp_reference,&
&                     BS_columns,BSS_Vnl_included,BSS_uses_GreenF
 use TDDFT,      ONLY:FXC_description,FXC_type,FXC_n_descs,FXC_n_g_corr,&
&                     FXC_per_memstps,FXC_LRC_alpha,FXC_LRC_beta,FXC_SVD_digits,&
&                     FXC_is_causal
 use ACFDT,      ONLY:ACFDT_n_lambda,ACFDT_n_freqs,ACFDT_E_range
 use fragments,  ONLY:Parallel_Fragments
#if defined _ELPH 
 use ELPH,       ONLY:ph_modes,elph_nb,gsqF_energy_steps,eval_G_using_KK,nQ_memory,&
&                     elph_Ham_bands,elph_Ham_ik,elph_branches
#endif
 use memory_m,   ONLY:mem_reset
 use zeros,      ONLY:zero_norm,k_iku_zero,k_rlu_zero,G_iku_zero,zero_dfl
 use xc_functionals,  ONLY:GS_xc_FUNCTIONAL,GS_xc_KIND 
#if defined _SC
 use SC,         ONLY:SC_iterations,SC_rho_threshold,SC_potential,OEP_approx,&
&                     compatible_SC_DB,SC_cycle_mixing,SC_fft_size,found_SC_DB
 use collisions_IO,    ONLY:n_collisions,COLLISIONS_cutoff
 use collisions_CACHE, ONLY:COLLISIONS_cache_size
#endif
#if defined _RT
 use real_time,  ONLY:RT_step,NE_steps, Thermal_steps, Integrator_name,            &
&                     RT_diagonalization_steps, l_NE_simulate_RPA,  &
&                     SwitchOnTime,max_rt_steps_disk,l_RT_induced_field,&
&                     l_RT_diagonalize,RT_is_dumped,QP_LifeTime,Phase_LifeTime,&
&                     NE_Hxc_jumps,NE_tot_time,&
&                     l_RT_diamagnetic,l_RT_probe,l_RT_pump_and_probe,NE_tot_time,NE_Hxc_jumps
 use fields,     ONLY:Efield,n_ext_fields
#endif
#if defined _TWO_LEVELS
 use real_time,  ONLY:l_3levels,dip,tau_diag,tau_offdiag
#endif
#if defined _MAGNETIC
 use magnetic,    ONLY:MAG_B,MAG_radius,MAG_hamiltonian_type,MAG_landau,MAG_pauli,&
&                      MAG_gauge,MAG_psi,MAG_theta,average_el_mag_rescaling,      &
&                      average_el_mag_dim,phase_trick
#endif
 !
 implicit none
 !
 character(*) :: INSTR,IND,OD,JS,COM_DIR
 integer      :: i1,i2
 !
 ! Printed reals format lengths 
 !
 call set_real_printed_length()
 !
 ! Stack Size 
 !
 call remove_stack_limit()
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
 ! ... and Job string
 !
 jobstr=JS
 !
 ! PAR PROC Setup
 !
 master_cpu          =myid==0
 local_master_cpu(1) =.TRUE.
 local_master_cpu(2:)=.TRUE.
 !
 ! TTY size 
 !
 call win_size(tty_size)
 call ct(INIT=.TRUE.)
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
 write_to_of=master_cpu
 write_to_report=master_cpu
 !
 !LOGO
 !
 ID_logo=-1
 ID_logo_stderr=-1
 !
 !stderr
 !
 write_to_log=master_cpu
 write (logfile,'(2a)') trim(more_io_path),'/l_stderr'
 !
 !Timing
 !
 log_line_to_dump=.false.
 log_line=' '
 nhash=20
 parallel_timing=.false.
 live_timing_is_on=.true.
 !
 !functions
 !
 bose_E_cut=0.1
 !
 !D_lattice
 !
 input_Tel_commands_over_header=.false.
 non_periodic_directions='none'
 lattice='Unknown'
 Bose_Temp=-1./HARTREE
 !
 ! R_lattice
 !
 n_g_shells=0
 nqibz=0
 ng_closed=0
 bse_scattering=.false.
 Xk_grid_is_uniform=.true.
 q0_def_norm=1.E-5
 !
 ! RIM
 !
 RIM_id_epsm1_reference=0
 RIM_epsm1=0.
 RIM_anisotropy=0.
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
 !
 ! Pseudo
 !
 pp_n_l_comp_max=0
 pp_kbv_dim=0
 !
 !drivers
 !
 list_dbs=.false.
 infile_editing=.false.
 l_setup=.false.
 l_rim=.false.
 l_col_cut=.false.
 l_optics=.false.
 l_HF_and_locXC=.false.
 l_em1d=.false.
 l_em1s=.false.
 l_ppa=.false.
 l_cohsex=.false.
 l_gw0=.false.
 l_bse=.false.
 l_chi=.false.
 l_bss=.false.
 l_bs_fxc=.false.
 l_acfdt=.false.
 l_el_corr=.false.
 l_ph_corr=.false.
 l_sc_run=.false.
 l_sc_hf=.false.
 l_sc_cohsex=.false.
 l_sc_exx=.false.
 l_sc_contains_lda=.false.
 l_sc_magnetic=.false.
 l_collisions_IO=.false.
 l_elph_Hamiltonian=.false.
 !
 !electrons
 !
 n_spin=1
 n_sp_pol=1
 n_spinor=1
 spin_orbit       = .false.
 BZ_RIM_nbands=0
 BZ_RIM_tot_nkpts=0
 filled_tresh=0.00001
 !
 !memory
 !
 call mem_reset()
 !
 !wave_func
 !
 wf_b=0
 wf_k=0
 wf_ng=0
 wf_space=' '
 wf_norm_test=.true.
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
 !IO
 !
 serial_number=0
 IO_read=.true.
 IO_write=master_cpu
 n_nodes=1
 !
 Parallel_Fragments=.FALSE.
 Fragmented_IO=index(INSTR,'dbfrag')>0
 Fragmented_IO_save=Fragmented_IO
#if defined _DISTRIBUTED
 Distributed_Memory=index(INSTR,'parmem')>0.and.ncpu>1
#endif
 !
 do i1=1,max_io_units
   call io_reset(i1)
 enddo
 !
 !X
 !
 current_iq=0
 X_RIM_nkpts=0
 self_detect_E_range=.false.
 half_X_mat_only=.false.
 Dipole_bands_ordered=.true.
 Dipole_uses_shifted_grids=.false.
 use_X_RIM=.false.
 eps_2_alpha=1._SP
 alpha_dim='adim'
 global_gauge='length'
#if defined _RT
 global_gauge='velocity'
#endif
 grid_path='none'
 eval_alpha=.FALSE.
 Vnl_commutator_warning=.FALSE.
 q_plus_G_direction=0.
 Q_Shift_Order=1
 !
 !QPm
 !
 QP_n_states=0
 QP_dSc_steps=2
 QP_G_Zoom_treshold=0.
 QP_Sc_steps=100
 QP_n_W_freqs=100
 QP_dSc_test=.false.
 QP_solver=' '
 QP_G_damp=0.1/HARTREE
 QP_dSc_delta=0.1/HARTREE
 QP_W_er=0.
 QP_W_dr=0.1/HARTREE
 QP_W_partially_done=.false.
 QP_G_er=(/-10._SP/HARTREE,10._SP/HARTREE/)
 QP_G_dr=0.1/HARTREE
 QP_cg_percent=100.
 GWo_iterations=0
 GWo_SC_done=.false.
 COHSEX_use_empties=.false.
 On_Mass_Shell_approx=.false.
 SC_E_threshold=0.01/HARTREE
 SC_band_mixing=100.
 Vnlxc_kind='HF'
 Vxc_kind='LDA'
 l_extended_output=.FALSE.
 !
 ! QP_ctl control
 !
 QP_ctl_db="none"
 QP_ctl_interp_neigh=1
 forall (i1=1:3,i2=1:2) QP_ctl_E(i1,:,i2)=(/0.,1.,1./)
 forall (i1=1:3,i2=1:2) QP_ctl_W(i1,:,i2)=0.
 forall (i1=1:3,i2=1:2) QP_ctl_Z(i1,i2)=(1.,0.)
 !
 ! BS/BSS
 !
 BS_n_g_W=1
 BS_eh_en=(/-1.,-1./)/HARTREE
 BS_identifier=0
 BS_q=1
 BS_eh_win=100.
 BS_res_mode='xc'
 BS_cpl_mode='none'
 BS_K_dim=0
 BSS_mode=' '
 BSS_n_freqs=100
 BSS_n_descs=0
 BSS_er=(/0.,10./)/HARTREE
 BSS_dr=.1/HARTREE
 BSS_q0=(/1.,0.,0./)
 BSS_uses_RIM=.FALSE.
 BSS_damp_reference=0.
 BS_K_is_ALDA=.FALSE.
 BS_not_const_eh_f=.FALSE.
 Haydock_treshold=-0.02
 BS_columns=0
 BSS_Vnl_included=.FALSE.
 BSS_uses_GreenF=.FALSE.
 !
 ! TDDFT
 !
 FXC_description=""
 FXC_type='rpa'
 FXC_n_descs=0
 FXC_n_g_corr=1
 FXC_per_memstps=100.
 FXC_LRC_alpha=0.
 FXC_LRC_beta=0.
 FXC_SVD_digits=0
 FXC_is_causal=.false.
 !
 ! ACFDT
 !
 ACFDT_n_lambda=1
 ACFDT_n_freqs=10
 ACFDT_E_range=(/100.,1000./)/HARTREE
 !
 ! xc_functionals  
 !
 GS_xc_FUNCTIONAL=-1             ! unknow 
 GS_xc_KIND=-1                   ! unknow 
 !
#if defined _ELPH 
 !
 ! ELPH
 !
 ph_modes=0
 elph_nb=0
 elph_branches=0
 gsqF_energy_steps=2
 eval_G_using_KK=.false.
 nQ_memory=0
 elph_Ham_bands=0
 elph_Ham_ik=1
#endif
 !
#if defined _SC
 ! SC
 !
 SC_fft_size=0
 SC_iterations=100
 SC_cycle_mixing=0.5
 SC_rho_threshold=1.E-5
 SC_potential='none'
 OEP_approx='n'
 compatible_SC_DB=.FALSE.
 found_SC_DB=.FALSE.
 COLLISIONS_cutoff=0.001
 !
 ! COLLISIONS I/O
 !
 n_collisions=0
 COLLISIONS_cache_size=-1.0
 COLLISIONS_cutoff=0.0005
 !
 ! RT current (in use in SC_driver)
 !
 l_RT_diamagnetic=.FALSE.
 l_RT_probe=.FALSE.
 l_RT_pump_and_probe=.FALSE.
 !
#endif
#if defined _RT
 !
 ! Real Time
 !
 do i1=1,n_ext_fields
   Efield(i1)%t_initial    =0.*FS2AUT
   Efield(i1)%int_t_initial=0
   Efield(i1)%frequency    =0.
   Efield(i1)%intensity    =0.
   Efield(i1)%damping      =0.
   Efield(i1)%ef_name      ='none'
   Efield(i1)%versor       =(/1.0,0.0,0.0/)
 enddo
 !
 !
 RT_step=0.002*FS2AUT
 NE_steps=1
 NE_Hxc_jumps=0
 Thermal_steps=0
 SwitchOnTime = 1./8.
 Integrator_name='RK2'
 QP_LifeTime = 10.0*FS2AUT
 Phase_LifeTime = 10.0*FS2AUT
 RT_is_dumped=.FALSE.
 l_NE_simulate_RPA=.FALSE.
 RT_diagonalization_steps=-1
 l_RT_diagonalize=.FALSE.
 max_rt_steps_disk=200
 l_RT_induced_field=.FALSE.
 NE_tot_time= 5*QP_LifeTime
 !
#endif
 !
#if defined _TWO_LEVELS
 !
 l_3levels     =.FALSE.
 tau_offdiag   =0._SP
 tau_diag      =0._SP    
 dip           =0._SP            
 !
#endif
 !
#if defined _MAGNETIC
 !
 ! Magnetic
 !
 MAG_B=0.
 MAG_radius=0.
 MAG_hamiltonian_type=' '
 MAG_pauli=.FALSE.
 MAG_landau=.FALSE.
 MAG_gauge='SYMM'
 MAG_psi=0.
 MAG_theta=0.
 average_el_mag_dim='Bohr_magneton/Bohr^3'
 average_el_mag_rescaling=1.
 phase_trick=.false.
 !
#endif
 !
#if defined _BOLTZMANN || defined _MAGNETIC
 !
 ! Electric field and current
 !
 Boltz_Efield=1./AU2Vmm
 Boltz_tau=10.*FS2AUT
 !
#endif
 !
end subroutine
