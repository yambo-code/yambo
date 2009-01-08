!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
 logical :: l_setup
 logical :: l_rim
 logical :: l_col_cut
 logical :: l_acfdt
 !
 logical :: l_el_corr
 logical :: l_ph_corr
 !
 logical :: l_xxvxc
 logical :: l_ppa
 logical :: l_gw0
 logical :: l_life
 logical :: l_chosex
 !
 logical :: l_optics
 logical :: l_chi
 logical :: l_em1d
 logical :: l_em1s
 !
 logical :: l_bse
 logical :: l_bss
 logical :: l_td_hf
 !
 logical :: l_sc_run
 logical :: l_sc_hartree
 logical :: l_sc_exx
 logical :: l_sc_exxc
 logical :: l_sc_hf
 logical :: l_sc_chosex
 logical :: l_sc_pz
 logical :: l_sc_lda_x
 !
 logical :: l_alda_fxc
 logical :: l_bs_fxc
 logical :: l_lrc_fxc
 !
end module drivers
