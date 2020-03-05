!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS,AM
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
subroutine YPP_SET_defaults()
 !
 use YPPm,           ONLY:DOS_broadening,DOS_E_range,r_hole,deg_energy ,EXCITON_weight_treshold,&
                          WF_multiplier,gamma_radius,SOC_split,BANDS_bands,BANDS_steps,&
&                         coo_in,coo_out,DOS_E_steps,INTERP_grid ,l_amplitude,l_bands,l_current,l_density  ,&
&                         l_dos,l_exc_wf   ,l_mag,l_norm_to_one,l_sort,l_sp_wf,l_spin,l_wannier,mag_dir    ,&
&                         N_path_pts,ncell ,ord_dgrid_ipol ,output_fname,p_dir,p_format   ,perturbative_SOC_path,&
&                         plot_title,PtsPath,seed_name,SOC_bands_buffer,SOC_bands_to_map,state_ctl ,V_value,&
&                         w90_fname,wannier_bands,WF_ref,what_to_write,SP,HA2EV,E_field,Nel_fac,DOS_bands,  &
&                         BSiq,l_PROJECT_atom,PDOS_atoms,PDOS_kinds,PDOS_wfcs,PDOS_l,PDOS_j,PDOS_m,&
&                         EXCITON_Res_treshold,EXCITON_E_treshold,l_PROJECT_line,l_PROJECT_plane
 use YPP_symm,       ONLY:wf_ng_cut
 use units,          ONLY:FS2AUT
 use interpolate,    ONLY:INTERP_shell_factor,interpls,max_interpls
 use electrons,      ONLY:n_bands
 use com,            ONLY:write_to_report
 use stderr,         ONLY:write_to_log  
 use wave_func,      ONLY:wf_ng
 use LIVE_t,         ONLY:live_timing_is_on
#if defined _YPP_SC
 use YPPm,           ONLY:l_mean_potential
#endif
#if defined _YPP_MAGNETIC
 use YPPm,           ONLY:B_field,B_psi,B_theta,l_angular_momentum,l_position
#endif
#if defined _YPP_RT
 use YPP_real_time,  ONLY:RT_conf,Eh_pumped_Nel,Eh_pumped_pair_BZ_regions,Eh_pumped_pair_BZ_width,&
&                         Eh_pumped_pair_deltaE,Eh_pumped_pair_energy,Eh_pumped_pair_width,l_RealTime,l_RT_dos,&
&                         l_RT_abs,l_RT_bands,l_RT_DBs,l_RT_density,l_RT_energy,l_RT_lifetimes   ,l_RT_occupations,&
&                         l_RT_time  ,l_RT_X,Eh_pumped_pair_n_BZ_regions,Probe_Keyword ,Pump_Keyword,X_order,&
&                         Eh_mu,Eh_temp,h_mu_autotune_thr,RT_p_format
 use fields,         ONLY:n_ext_fields
#endif
#if defined _YPP_ELPH
 use YPP_ELPH,       ONLY:elph_dbs_path, elph_Ef, elph_gamma_broad, elph_steps, l_atomic_amplitude, l_eliashberg,&
&                         l_gkkp, l_phonons, ph_broad, ph_freqs_file, ph_modes_file
 use ELPH,           ONLY:gsqF_energy_steps
#endif
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
 ! RT OCCUPATIONS
 !
#if defined _YPP_RT
 !
 l_RealTime=.FALSE.
 l_RT_DBs  =.FALSE.
 !
 ! Absorption, occupations, lifetimes, density
 !
 l_RT_X           =.FALSE.
 l_RT_abs         =.FALSE.
 l_RT_occupations =.FALSE.
 l_RT_lifetimes   =.FALSE.
 l_RT_density     =.FALSE.
 !
 ! PP kind
 !
 l_RT_time  =.FALSE.
 l_RT_energy=.FALSE.
 l_RT_dos   =.FALSE.
 l_RT_bands =.FALSE.
 !
#endif
 !
 ! BANDS
 !
 BANDS_bands=(/1,n_bands/)
 BANDS_steps=10
 l_bands=.false.
 INTERP_shell_factor=20._SP
 INTERP_grid = (/-1, -1, -1/)
#if defined _YPP_RT
 Nel_fac=1._SP
#endif
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
#if defined _YPP_SC
 l_mean_potential=.false.
#endif
 l_norm_to_one=.true.
 output_fname=' '
 plot_title=' '
 !
 ! EXC WF
 !
 l_spin=.false.
 l_sort=.false.
 l_amplitude=.false.
 ncell = (/1, 1, 1/) 
 r_hole= (/0._SP,0._SP,0._SP/)
 state_ctl = '0 - 0'
 deg_energy =0.01_SP/HA2EV
 EXCITON_Res_treshold =0.0_SP
 EXCITON_E_treshold =0.0_SP
 EXCITON_weight_treshold =0.05_SP
 WF_multiplier=1._SP
 BSiq=1
 !
#if defined _YPP_ELPH
 !
 ! ELPH
 !
 l_phonons=.false.
 l_gkkp=.false.
 l_eliashberg=.false.
 l_atomic_amplitude=.false.
 elph_steps=200
 ph_broad=0.01_SP/HA2EV
 elph_gamma_broad=0._SP
 elph_Ef=0._SP
 elph_dbs_path='.'
 ph_freqs_file='none'
 ph_modes_file='none'
 gsqF_energy_steps=1
 !
#endif
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
 !
 ! BZ RIM
 !
 gamma_radius=0._SP
 !
 ! WFs perturbative SOC
 !
 perturbative_SOC_path=""
 SOC_bands_to_map=(/ 1, n_bands /)
 SOC_bands_buffer=1
 SOC_split=0.1_SP/HA2EV
 !
 ! BXSF interpolation
 !
 w90_fname="./w90.bxsf"
 wannier_bands=(/1,n_bands/)
 ord_dgrid_ipol = 0
 !
 ! GPL_EXCLUDE_START
 !
#if defined _YPP_RT
 !
 !REMOVE SYMMETRIES
 !
 E_field=0._SP
 !
 !ANALIZE RESPONSE
 !
 RT_conf%T_range(:)  =real((/-FS2AUT,-FS2AUT/),SP)
 RT_conf%W_n_steps   =200
 RT_conf%W_range(:)  =(/0._SP,20./HA2EV/)
 Probe_Keyword ="none"
 Pump_Keyword  ="none"
 X_order       =1
 n_ext_fields  =1
 !
 !MANUAL PUMP
 !
 Eh_pumped_pair_n_BZ_regions=1
 Eh_pumped_pair_deltaE=-1._SP/HA2EV
 Eh_pumped_pair_energy=-1._SP/HA2EV
 Eh_pumped_pair_width=0.1_SP/HA2EV
 Eh_pumped_pair_BZ_width=0.1_SP
 Eh_pumped_pair_BZ_regions=-1.0_SP
 Eh_pumped_Nel=1._SP
 !
 !FERMI FUNCTION PUMP
 !
 Eh_mu   =(/-0.001_SP/HA2EV,0.001_SP/HA2EV/)
 Eh_temp =(/0._SP/HA2EV,0._SP/HA2EV/)
 h_mu_autotune_thr = 0._SP
 !
 !TR abs
 !
 RT_p_format="3col" 
 !
#endif
 !
 ! ANGULAR MOMENTUM & ELECTRON DISPLACEMENT
 !
#if defined _YPP_MAGNETIC
 !
 l_angular_momentum=.false.
 l_position=.false.
 !
 !REMOVE SYMMETRIES
 !
 E_field=0._SP
 B_field=0._SP
 B_theta=0._SP
 B_psi=0._SP
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
end subroutine YPP_SET_defaults
