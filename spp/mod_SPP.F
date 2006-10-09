
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 module SPP
!==========
  use pars
  implicit none
!
! K analysis
!===============================================================
  logical  :: lkanalyze
  real(SP) :: Kgw_1(3)
  character(schlen) :: Kcoo_in,Kcoo_out
!
! Exc WF
!===============================================================
  logical      :: lexcwf
  integer      :: lambda       !energy of the exciton
  real(SP)     :: window       !energy window in o/o (1 -> 100)
  integer      :: ncell(3)     !number of repetitions of the cell
  real(SP)     :: r_hole(3)    !position of the hole 
  character(1) :: sort_mode    !to decide if sorting by enegies or intensities
!
!EL-PH
!===============================================================
  logical :: lelph
  integer :: elph_steps
  real(SP):: elph_gamma_broad,elph_eli_broad,elph_Ef,elph_Emax
  character(schlen) :: elph_dbs_path
!
! BZ RIM
!===============================================================
  integer  :: BZ_RIM_nkpts,RIM_G_shells
  logical  :: lbzrim
  character(lchlen) :: BZ_RIM_path

 end module SPP
