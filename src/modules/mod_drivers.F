!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module drivers
 !
 ! Generic  Drivers
 !
 logical :: list_dbs             =.FALSE.
 logical :: infile_editing       =.FALSE.
 logical :: Finite_Tel           =.FALSE.
 !
 ! Specific Drivers
 !
 logical :: l_setup              =.FALSE.
 logical :: l_rim                =.FALSE.
 logical :: l_rim_w              =.FALSE.
 logical :: l_col_cut            =.FALSE.
 logical :: l_acfdt              =.FALSE.
 !
 ! Real Time
 !
 logical :: l_real_time          =.FALSE.
 logical :: l_sc_run             =.FALSE.
 logical :: l_sc_electric        =.FALSE.
 logical :: l_rt_carriers_in_use =.FALSE.
 logical :: l_sc_magnetic        =.FALSE.
 logical :: l_eval_collisions    =.FALSE.
 logical :: l_use_collisions     =.FALSE.
 logical :: l_nl_optics          =.FALSE.
 logical :: l_nl_p_and_p         =.FALSE.
 !
 ! The self-energy correlation "kind"
 !
 logical :: l_elphoton_corr      =.FALSE.
 logical :: l_elel_corr          =.FALSE.
 logical :: l_elph_corr          =.FALSE.
 logical :: l_phel_corr          =.FALSE.
 !
 ! The non-eq. self-energy scattering "kind"
 !
 logical :: l_elphoton_scatt     =.FALSE.
 logical :: l_elel_scatt         =.FALSE.
 logical :: l_elph_scatt         =.FALSE.
 logical :: l_phel_scatt         =.FALSE.
 !
 ! Electron-Phonon specific flags
 !
 logical :: l_elph_Hamiltonian   =.FALSE.
 !
 ! Real Axis non SC Self-energies
 !
 logical :: l_HF_and_locXC       =.FALSE.
 logical :: l_life               =.FALSE.
 logical :: l_gw0                =.FALSE.
 logical :: l_ppa                =.FALSE.
 logical :: l_mpa                =.FALSE.
 logical :: l_cohsex             =.FALSE.
 !
 ! The optical runlevels ...
 !
 logical :: l_optics             =.FALSE.
 logical :: l_chi                =.FALSE.
 logical :: l_dipoles            =.FALSE.
 logical :: l_bse                =.FALSE.
 logical :: l_bss                =.FALSE.
 !
 ! The screening runlevels ...
 !
 logical :: l_screen             =.FALSE.
 logical :: l_X                  =.FALSE.
 logical :: l_em1d               =.FALSE.
 logical :: l_em1dx              =.FALSE.
 logical :: l_em1s               =.FALSE.
 !
 ! ... and the kernel
 !
 logical :: l_rpa_IP             =.FALSE.
 logical :: l_td_hartree         =.FALSE.
 logical :: l_tddft              =.FALSE.
 logical :: l_alda_fxc           =.FALSE.
 logical :: l_bs_fxc             =.FALSE.
 logical :: l_lrc_fxc            =.FALSE.
 logical :: l_pf_fxc             =.FALSE.
 logical :: l_td_hf              =.FALSE.
 logical :: l_td_sex             =.FALSE.
 !
 ! XC kinds (Potential)
 !
 logical :: l_sc_exx             =.FALSE.
 logical :: l_sc_srpa            =.FALSE.
 logical :: l_sc_ip              =.FALSE.
 logical :: l_sc_hartree         =.FALSE.
 logical :: l_sc_fock            =.FALSE.
 logical :: l_sc_coh             =.FALSE.
 logical :: l_sc_sex             =.FALSE.
 logical :: l_sc_is_libDFT       =.FALSE.
 !
 ! OEP approximations
 !
 logical :: l_oep_exact          =.FALSE.
 logical :: l_oep_kli            =.FALSE.
 logical :: l_oep_ceda           =.FALSE.
 logical :: l_oep_slater         =.FALSE.
 !
 ! Collisions
 !
 logical :: l_use_Hxc_collisions =.FALSE.
 logical :: l_use_COH_collisions =.FALSE.
 !
 ! TESTING
 !
 logical :: l_slk_test           =.FALSE.
 logical :: l_gpu_test           =.FALSE.
 !
end module drivers
