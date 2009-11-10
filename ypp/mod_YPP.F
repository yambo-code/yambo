!
! Copyright (C) 2000-2009 A. Marini and the YAMBO team 
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
 use pars,    ONLY:SP
 use pars,    ONLY:lchlen
 use pars,    ONLY:schlen
 implicit none
 !
 ! BZ grids 
 !
 logical  :: l_bz_grids
 logical  :: l_k_grid
 logical  :: l_q_grid
 logical  :: l_long_grid
 logical  :: l_high_sym_pts
 integer  :: n_path_pts
 real(SP) :: Kgw_1(3)
 real(SP) :: qlong(3,3)
 real(SP) :: alat_used_for_output
 character(schlen) :: coo_in
 character(schlen) :: coo_out
 character(schlen) :: PtsPath
 ! 
 ! DOS
 !
 logical      ::l_dos ! electrons and phonons
 real         ::dos_broadening ! electrons
 integer      ::dos_bands      ! electrons
 !
 ! REAL SPACE PLOT
 !
 logical      ::l_plot
 logical      ::l_wavefunction
 logical      ::l_density
 logical      ::l_mag
 logical      ::l_sp_wf
 logical      ::l_exc_wf
 logical      ::l_norm_to_one
 logical      ::use_gnuplot
 logical      ::use_xcrysden
 character(1) ::p_format     
 character(1) ::mag_dir     
 character(3) ::p_dir        
 character(lchlen) ::output_fname 
 character(schlen) ::plot_title 
 integer               ::nr(3)
 integer               ::nr_tot
 integer               ::plot_dim
 real(SP), allocatable ::v2plot(:)
 real(SP), allocatable ::v2plot2D(:,:)
 !
 ! Exc/SP Properties
 !
 logical      ::l_excitons
 logical      ::l_electrons
 logical      ::l_sort
 logical      ::l_amplitude
 logical      ::l_spin
 logical      ::l_free_hole
 logical      ::l_angular_momentum
 integer      ::lambda
 integer      ::n_lambda
 integer      ::BS_H_dim
 integer      ::ncell(3)     
 real(SP)     ::r_hole(3)    
 real(SP)     ::deg_energy   
 real(SP)     ::WF_multiplier   
 character(schlen) ::state_ctl 
 !
 !EL-PH
 !
 logical :: l_gkkp
 logical :: l_phonons
 logical :: l_eliashberg
 logical :: l_atomic_amplitude
 integer :: elph_steps
 real(SP):: elph_gamma_broad
 real(SP):: ph_broad
 real(SP):: elph_Ef
 real(SP):: elph_E_max
 character(schlen) :: elph_dbs_path
 character(schlen) :: ph_freqs_file
 !
 !RAS
 !
 logical           :: lras, lsurf, lreels, lcelleps
 character(schlen) :: xdata, ydata, zdata
 character(len=5)  :: datatype
 real(SP)          :: sshift
 real(SP)          :: d_cellin
 !
 ! REELS
 !
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
 ! GPL_EXCLUDE_START RAS_IGNORE
 !
 ! BZ RIM
 !
 integer  :: BZ_RIM_nkpts
 logical  :: l_bzrim
 real(SP) :: Gamma_radius
 character(lchlen) :: BZ_RIM_path
 !
#if defined _YPP_RT
 !
 ! Reduce Symmetries
 !
 logical     :: l_rsymm
 logical     :: l_rtoptics
 real(SP)    :: E_field(2,3)
 real(SP)    :: B_field,B_theta,B_psi
 integer     :: ref_efield
 !
 type Xw_rt
   real(SP) :: time_range(2)       ! time windows for the transform
   integer  :: energy_steps        ! number of steps in energy mesh
   real(SP) :: energy_range(2)     ! energy range 
   character(schlen) :: damp       ! Damp type (none, lorentian or gaussian)
   real(SP) :: damp_factor         ! factor used in damping
   real(SP) :: noise_ratio         ! remove noise in current less than < max/noise_ration
 end type Xw_rt
 !
 type(Xw_rt) :: X_rt 
 !
#endif
 !
 ! GPL_EXCLUDE_END RAS_IGNORE
 !
 interface
   !
   subroutine excitons_sort_and_report(BS_H_dim,Xk,E,BS_R,BS_E,BS_E_degs,A_weight,S_z,S_sq)
     use pars,          ONLY:SP
     use R_lattice,     ONLY:bz_samp
     use electrons,     ONLY:levels
     implicit none
     integer            :: BS_H_dim
     complex(SP)        :: BS_R(BS_H_dim),BS_E(BS_H_dim)
     type(bz_samp)      :: Xk
     type(levels)       :: E
     real(SP),optional  :: A_weight(BS_H_dim)
     real(SP),optional  :: S_z(BS_H_dim)
     real(SP),optional  :: S_sq(BS_H_dim)
     integer, optional  :: BS_E_degs(BS_H_dim)
   end subroutine
   !
   subroutine  elph_general_gFsq(k,en,Xk,Xen,q,lambda_s,BS_E_degs)
     use R_lattice,     ONLY:bz_samp
     use electrons,     ONLY:levels
     implicit none
     type(bz_samp)      :: k,q,Xk
     type(levels)       :: en,Xen
     integer, optional  :: lambda_s(:)
     integer, optional  :: BS_E_degs(:)
   end subroutine
   !
 end interface
 !
 contains
   !
   subroutine K_transform(k,in_mode)
     !
     use units,        ONLY:pi
     use vec_operate,  ONLY:c2a
     !
     real(SP)     :: k(3)
     character(*) :: in_mode
     !
     select case (in_mode)
       case ('cc')
         if (trim(coo_out)=="iku")  call c2a(v_in=k,mode="kc2i")
         if (trim(coo_out)=="rlu")  call c2a(v_in=k,mode="kc2a")
         if (trim(coo_out)=="alat") then
           k=k*alat_used_for_output/2./pi
         endif
       case ('iku')
         if (trim(coo_out)=="cc")   call c2a(v_in=k,mode="ki2c")
         if (trim(coo_out)=="rlu")  call c2a(v_in=k,mode="ki2a")
         if (trim(coo_out)=="alat") then
           call c2a(v_in=k,mode="ki2c")
           k=k*alat_used_for_output/2./pi
         endif
       case ('rlu')
         if (trim(coo_out)=="cc")   call c2a(v_in=k,mode="ka2c")
         if (trim(coo_out)=="iku")  call c2a(v_in=k,mode="ka2i")
         if (trim(coo_out)=="alat") then
           call c2a(v_in=k,mode="ka2c")
           k=k*alat_used_for_output/2./pi
         endif
     end select
     !
   end subroutine
   !
end module YPP
