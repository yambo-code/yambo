!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DSAM
!
subroutine YPP_SET_defaults()
 !
 use YPPm,           ONLY:DOS_broadening,DOS_E_range,r_hole,deg_energy ,EXCITON_weight_treshold,&
                          WF_multiplier,SOC_split,BANDS_bands,BANDS_steps,&
&                         coo_in,coo_out,DOS_E_steps,INTERP_grid ,l_amplitude,l_bands,l_current,l_density  ,&
&                         l_dos,l_exc_wf   ,l_mag,l_norm_to_one,l_sort,l_sp_wf,l_spin,l_wannier,mag_dir    ,&
&                         N_path_pts,ncell ,output_fname,p_dir,p_format   ,perturbative_SOC_path,&
&                         plot_title,PtsPath,seed_name,SOC_bands_buffer,SOC_bands_to_map,state_ctl ,V_value,&
&                         WF_ref,what_to_write,SP,HA2EV,E_field,Nel_fac,DOS_bands,AVE_exc,l_interp,  &
&                         BSiq,l_PROJECT_atom,PDOS_atoms,PDOS_kinds,PDOS_wfcs,PDOS_l,PDOS_j,PDOS_m,&
&                         EXCITON_Res_treshold,EXCITON_E_treshold,l_PROJECT_line,l_PROJECT_plane,K_grid,&
&                         DIPs_C_bands,DIPs_V_bands,DIPs_E_range
 use YPP_symm,       ONLY:wf_ng_cut
 use units,          ONLY:FS2AUT
 use interpolate,    ONLY:INTERP_shell_factor,interpls,max_interpls
 use electrons,      ONLY:n_bands
 use com,            ONLY:write_to_report
 use stderr,         ONLY:write_to_log  
 use wave_func,      ONLY:wf_ng
 use LIVE_t,         ONLY:live_timing_is_on
 !
 implicit none
 !
 ! Switch off all kind of logs (will be reactivated in INIT_ypp)
 !
 write_to_report   =.FALSE.   
 live_timing_is_on =.FALSE.   
 write_to_log      =.FALSE.   
 !
 ! Projections
 !
 l_PROJECT_atom  =.false.
 l_PROJECT_line  =.false.
 l_PROJECT_plane =.false.
 !
 ! DOS
 !
 l_dos=.false.
 DOS_broadening=0.1_SP/HA2EV
 DOS_E_range=(/1._SP,-1._SP/)/HA2EV
 DOS_E_steps=500
 DOS_bands=(/1,n_bands/)
 WF_ref=0
 !
 PDOS_atoms=-1
 PDOS_kinds=-1
 PDOS_wfcs =-1
 PDOS_l=-1
 PDOS_j=-1._SP
 PDOS_m=-10
 !
 ! SYMMETRIES
 !
 wf_ng_cut=0
 !
 ! BANDS
 !
 BANDS_bands=(/1,n_bands/)
 BANDS_steps=10
 l_bands=.false.
 INTERP_shell_factor=20._SP
 INTERP_grid = (/-1, -1, -1/)
 !
 ! Interpolation
 !
 interpls(1:max_interpls)%ndim=0
 !
 ! PLOT
 !
 p_dir      ='1'
 mag_dir    ='X'
 p_format   ='g'
 l_exc_wf   =.false.
 l_density  =.false.
 l_current  =.false.
 V_value    ='RE'
 l_mag=.false.
 l_sp_wf=.false.
 l_norm_to_one=.false.
 output_fname=' '
 plot_title=' '
 !
 ! DIPOLES
 !
 DIPs_C_bands=(/1,n_bands/)
 DIPs_V_bands=(/1,n_bands/)
 DIPs_E_range=-1./HA2EV
 !
 ! EXC WF
 !
 l_spin=.false.
 l_sort=.false.
 l_amplitude=.false.
 l_interp=.false.
 ncell = (/1, 1, 1/) 
 r_hole= (/0._SP,0._SP,0._SP/)
 state_ctl = '0 - 0'
 deg_energy =0.01_SP/HA2EV
 EXCITON_Res_treshold =0.0_SP
 EXCITON_E_treshold =0.0_SP
 EXCITON_weight_treshold =0.05_SP
 WF_multiplier=1._SP
 BSiq=1
 AVE_exc='h'
 !
 ! Wannier interface
 !
 l_wannier=.FALSE.
 seed_name=" "
 what_to_write=" "
 !
 ! BZ grids
 !
 coo_in="rlu"
 coo_out="rlu"
 PtsPath= ' '
 N_path_pts=0
 K_grid=0
 !
 ! WFs perturbative SOC
 !
 perturbative_SOC_path=""
 SOC_bands_to_map=(/ 1, n_bands /)
 SOC_bands_buffer=1
 SOC_split=0.1_SP/HA2EV
 !
end subroutine YPP_SET_defaults
