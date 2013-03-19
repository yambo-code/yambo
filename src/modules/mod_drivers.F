!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
module drivers
 !
 ! Generic  Drivers
 !
 logical :: list_dbs
 logical :: infile_editing
 logical :: Finite_Tel
 !
 ! Specific Drivers
 !
 logical :: l_setup     ! Initial setup
 logical :: l_rim       ! Random Integration Method
 logical :: l_col_cut   ! Coloumb potential cutoff
 logical :: l_acfdt     ! Total Energy
 !
 ! Real Time & SC
 !
 logical :: l_real_time 
 logical :: l_sc_run
 logical :: l_sc_magnetic
 logical :: l_collisions_IO
 logical :: l_carrier_dynamics  
 !
 ! The self-energy correlation "kind"
 !
 logical :: l_el_corr
 logical :: l_ph_corr
 !
 ! Electron-Phonon specific flags 
 !
 logical :: l_elph_Hamiltonian
 !
 ! Real Axis non SC Self-energies
 !
 logical :: l_HF_and_locXC   ! HF + local XC
 logical :: l_life    ! GoWo lifetims
 logical :: l_gw0     ! GWo main logical
 logical :: l_ppa     ! PPA
 logical :: l_cohsex  ! COHSEX
 !
 ! The optical runlevels ...
 !
 logical :: l_optics
 logical :: l_chi
 logical :: l_em1d
 logical :: l_em1s
 logical :: l_bse
 !
 ! ... the kernel ...
 !
 logical :: l_rpa_IP
 logical :: l_td_hartree
 logical :: l_tddft
 logical :: l_alda_fxc
 logical :: l_bs_fxc
 logical :: l_lrc_fxc
 logical :: l_td_hf
 logical :: l_W_eh
 logical :: l_W_eh_diag
 logical :: l_W_eh_cpl
 !
 ! ... and the solvers
 !
 logical :: l_bss
 !
 ! The XC types (Vxc)
 !
 logical :: l_sc_exx
 logical :: l_sc_srpa
 logical :: l_sc_hf
 logical :: l_sc_cohsex
 logical :: l_sc_contains_lda
 !
 ! OEP approximations
 !
 logical :: l_oep_exact
 logical :: l_oep_kli
 logical :: l_oep_ceda
 logical :: l_oep_slater
 !
 !
end module drivers
