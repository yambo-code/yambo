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
module YPP
 !
 use pars,    ONLY:SP,lchlen,schlen
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
 ! PLOT
 !
 logical      ::l_plot
 logical      ::l_exc_wf
 logical      ::l_sp_wf
 logical      ::l_density
 logical      ::l_mag
 logical      ::l_merge
 logical      ::l_norm_to_one
 logical      ::use_gnuplot
 logical      ::use_xcrysden
 character(1) ::p_format     
 character(3) ::p_dir        
 character(lchlen) ::output_fname 
 integer               ::nr(3)
 integer               ::nr_tot
 integer               ::plot_dim
 real(SP), allocatable ::v2plot(:)
 real(SP), allocatable ::v2plot2D(:,:)
 !
 ! Exc Properties
 !
 logical      ::l_exc_props
 logical      ::l_sort
 logical      ::l_amplitude
 logical      ::l_spin
 logical      ::l_free_hole
 integer      ::lambda
 integer      ::ncell(3)     
 real(SP)     ::r_hole(3)    
 real(SP)     ::deg_energy   
 real(SP)     ::WF_multiplier   
 character(schlen) ::state_ctl 
 !
#if defined _YPP_ELPH
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
#if defined _YPP_RAS
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
 ! GPL_EXCLUDE_START
 !
 ! BZ RIM
 !
 integer  :: BZ_RIM_nkpts
 logical  :: lbzrim
 character(lchlen) :: BZ_RIM_path
 !
 ! GPL_EXCLUDE_END
 !
 interface
   !
   subroutine exc_wf_sort_and_report(Xk,E,BS_R,BS_E,BS_E_degs,A_weight,S_z,S_sq)
     use pars,          ONLY:SP
     use BS,            ONLY:BS_K_dim
     use R_lattice,     ONLY:bz_samp
     use electrons,     ONLY:levels
     implicit none
     complex(SP)        :: BS_R(BS_K_dim),BS_E(BS_K_dim)
     type(bz_samp)      :: Xk
     type(levels)       :: E
     real(SP),optional  :: A_weight(BS_K_dim)
     real(SP),optional  :: S_z(BS_K_dim)
     real(SP),optional  :: S_sq(BS_K_dim)
     integer, optional  :: BS_E_degs(BS_K_dim)
   end subroutine
   !
 end interface
 !
end module YPP
