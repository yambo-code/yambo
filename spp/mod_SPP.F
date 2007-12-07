!
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
 !
 use pars,    ONLY:SP,lchlen,schlen,BOHR
 implicit none
 !
 ! K analysis
 !
 logical  :: lkanalyze
 real(SP) :: Kgw_1(3)
 real(SP) :: qlong(3,3)
 character(schlen) :: coo_in
 character(schlen) :: coo_out
 !
 ! Exc WF
 !
 integer, parameter :: max_lambda_deg=100
 !
 logical      ::lexcwf
 integer      ::lambda       !energy of the exciton
 integer      ::n_lambda_deg 
 integer      ::lambda_deg(max_lambda_deg)
 integer      ::ncell(3)     !number of repetitions of the cell
 real(SP)     ::max_energy   !max energy to be shown in ordering
 real(SP)     ::r_hole(3)    !position of the hole 
 character(1) ::sort_mode    !to decide if sorting by energies or intensities
 character(1) ::action_mode  !to decide if sorting or plotting
 character(1) ::p_format     !Format of the plot
 character(3) ::p_dir        !Direction of the plot 
 character(3) ::R_normalize 
 ! 
 integer               ::nr(3)
 integer               ::nr_tot
 real(SP), allocatable ::exc_wf_sq(:)
 real(SP), allocatable ::wf_2d(:,:)
 !
#if defined SPP_PJ_PH
 !
 !EL-PH
 !
 logical :: lelph
 integer :: elph_steps
 real(SP):: elph_gamma_broad
 real(SP):: ph_broad
 real(SP):: elph_Ef
 real(SP):: elph_E_max
 character(schlen) :: elph_dbs_path
 character(schlen) :: ph_freqs_file
 !
#endif
#if defined SPP_PJ_RAS
 !
 !RAS
 !
 logical           :: lras, lsurf
 character(schlen) :: xdata, ydata
 character(len=5)  :: datatype
 real(SP)          :: sshift
 real(SP)          :: d_cellin
 !
 ! Localization
 !
 logical           :: lloc
 real(SP)          :: lowerlim, upperlim
 integer           :: normdir, ngloc
 !
 ! Transition Analysis
 !
 logical           :: ltrans
 real(SP)          :: Ecv_min, Ecv_max
 integer           :: idir
 !
#endif
 !
 ! BZ RIM
 !
 integer  :: BZ_RIM_nkpts
 logical  :: lbzrim
 character(lchlen) :: BZ_RIM_path
 !
end module SPP
