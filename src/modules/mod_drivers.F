!
!        Copyright (C) 2000-2018 the YAMBO team
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
 logical :: l_col_cut            =.FALSE.
 logical :: l_acfdt              =.FALSE.
 !
 ! Real Time 
 !
 logical :: l_photolum           =.FALSE.
 logical :: l_real_time          =.FALSE.
 logical :: l_sc_run             =.FALSE.
 logical :: l_rt_carriers_in_use =.FALSE.
 logical :: l_sc_magnetic        =.FALSE.
 logical :: l_eval_collisions    =.FALSE.
 logical :: l_use_collisions     =.FALSE.
 !
 ! The self-energy correlation "kind"
 !
 logical :: l_elphoton_corr      =.FALSE.
 logical :: l_elel_corr          =.FALSE.
 logical :: l_elph_corr          =.FALSE.
 !
 ! The non-eq. self-energy scattering "kind"
 !
 logical :: l_elphoton_scatt     =.FALSE.
 logical :: l_elel_scatt         =.FALSE.
 logical :: l_elph_scatt         =.FALSE.
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
 logical :: l_cohsex             =.FALSE.
 !
 ! The optical runlevels ...
 !
 logical :: l_optics             =.FALSE.
 logical :: l_chi                =.FALSE.
 logical :: l_em1d               =.FALSE.
 logical :: l_em1s               =.FALSE.
 logical :: l_dipoles            =.FALSE.
 logical :: l_bse                =.FALSE.
 logical :: l_bsk                =.FALSE.
 logical :: l_bss                =.FALSE.
 logical :: l_kerr               =.FALSE.
 logical :: l_anomalous_Hall     =.FALSE.
 !
 ! ... and the kernel
 !
 logical :: l_rpa_IP             =.FALSE.
 logical :: l_td_hartree         =.FALSE.
 logical :: l_tddft              =.FALSE.
 logical :: l_alda_fxc           =.FALSE.
 logical :: l_bs_fxc             =.FALSE.
 logical :: l_lrc_fxc            =.FALSE.
 logical :: l_td_hf              =.FALSE.
 logical :: l_W_eh               =.FALSE.
 logical :: l_W_eh_diag          =.FALSE.
 logical :: l_W_eh_cpl           =.FALSE.
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
 !
end module drivers
