!
!        Copyright (C) 2000-2019 the YAMBO team
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
module YPP
 !
 use pars,    ONLY:SP,DP,lchlen,schlen
 use units,   ONLY:HA2EV
 use QP_m,    ONLY:QP_t
 !
 implicit none
 !
 ! BZ grids 
 !
 logical  :: l_bz_grids
 logical  :: l_k_grid
 logical  :: l_q_grid
 logical  :: l_shifted_grid
 logical  :: l_random_grid
 logical  :: l_high_sym_pts
 integer  :: n_path_pts
 integer  :: BZ_random_nkpts
 real(SP) :: alat_used_for_output
 character(schlen) :: coo_in
 character(schlen) :: coo_out
 character(schlen) :: PtsPath
 character(23), parameter :: output_string="o exc den mag wf curr V"
 !
 ! USER-defined points
 !
 integer  :: n_u_k_pts,n_u_q_pts,n_u_interp_pts
 !
 real(SP) :: U_k_pts(10,3)
 real(SP) :: U_q_pts(10,3)
 real(SP) :: U_q_shifts(3,3)
 real(SP) :: U_interp_pts(10,3)
 ! 
 ! DOS
 !
 logical      ::l_dos ! electrons and phonons
 real(SP)     ::dos_broadening ! electrons
 real(SP)     ::dos_E_range(2) ! Energy Range
 integer      ::dos_bands(2)   ! Bands Range
 integer      ::dos_E_steps    ! Energy steps
 !
 ! BANDS
 !
 integer               ::BANDS_steps
 integer               ::interp_grid(3)  ! Interpolation Grid
 real(SP)              ::Nel_fac
 !
 ! SPIN and SOC
 integer      ::WF_ref(2)
 !
 ! REAL SPACE PLOT
 !
 logical      ::l_plot
 logical      ::l_wavefunction
 logical      ::l_density
 logical      ::l_bands
 logical      ::l_mag
 logical      ::l_current
 logical      ::l_sp_wf
 logical      ::l_exc_wf
 logical      ::l_norm_to_one
 logical      ::use_gnuplot
 logical      ::use_xcrysden
 logical      ::use_cube
 logical      ::plot_is_1D(3)
 logical      ::plot_is_2D(3)
 logical      ::plot_is_3D
 character(1) ::p_format     
 character(1) ::mag_dir     
 character(3) ::p_dir        
 character(lchlen) ::output_fname 
 character(schlen) ::plot_title 
 character(schlen) ::V_value 
 integer               ::nr(3)
 integer               ::nr_tot
 integer               ::plot_dim
 real(SP), allocatable ::v2plot(:)
 real(SP), allocatable ::v2plot2D(:,:)
 real(SP)              ::current_dir(3)=(/1.,1.,1./)
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
 logical      ::l_position
 integer      ::ncell(3)     
 real(SP)     ::r_hole(3)    
 real(SP)     ::deg_energy   
 real(SP)     ::min_weight   
 real(SP)     ::WF_multiplier   
 character(schlen) ::state_ctl 
 !
 ! Wannier interface
 !
 logical           :: l_wannier
 logical           :: l_write_A
 logical           :: l_write_M
 logical           :: l_write_U
 character(schlen) :: seed_name
 character(schlen) :: what_to_write
 !
 ! BZ Double Grid
 !
 integer, parameter :: MAX_DbGd_DBs=20
 integer  :: n_DbGd_DBs
 integer  :: BZ_DbGd_nkpts_to_use
 logical  :: l_map_kpts
 logical  :: l_map_WFs
 real(SP) :: Gamma_radius
 character(lchlen) :: DbGd_DB1_paths(MAX_DbGd_DBs,1)
 !
 ! Perturbative SOC
 !
 integer           :: SOC_bands_to_map(2)
 integer           :: SOC_bands_buffer
 character(lchlen) :: perturbative_SOC_path
 real(SP)          :: SOC_split
 !
 ! QP databases
 !
 logical     :: l_QP_DBs_create_and_modify
 logical     :: l_QP_DBs_manipulate
 integer     :: n_QP_dbs 
 character(lchlen),  allocatable :: QP_db_name(:)
 character(schlen),  allocatable :: QP_db_what(:)
 character(schlen),  allocatable :: QP_db_operation(:)
 real(SP),           allocatable :: QP_db_prefactor(:)
 type(QP_t),         allocatable :: QP_db(:)
 !
 ! Surface spectroscopy
 !
 logical           :: lras, lsurf, lreels, lcelleps
 character(schlen) :: xdata, ydata, zdata
 character(len=5)  :: datatype
 real(SP)          :: sshift
 real(SP)          :: d_cellin
 !
 ! Localization
 !
 logical           :: lloc
 real(SP)          :: lowerlim, upperlim
 integer           :: normdir, ngloc
 integer           :: loc_bands(2)
 integer           :: loc_kpts(2)
 !
 ! Transition Analysis
 !
 logical           :: ltrans
 real(SP)          :: Ecv_min, Ecv_max
 real(SP)          :: qdir(3)
 integer           :: idir
 !
 ! SC
 !
 logical  :: l_mean_potential
 !
 ! Wannier bands
 !
 logical           :: l_bxsf
 character(lchlen) :: w90_fname
 integer           :: wannier_bands(2)   ! Bands Range
 integer           :: ord_dgrid_ipol     ! order of interpolation using double grid
 !
 ! Reduce Symmetries
 !
 logical     :: l_fix_syms
 real(SP)    :: E_field(2,3)
 real(SP)    :: B_field,B_theta,B_psi
 !
 ! Convert wave--functions
 !
 logical     :: l_fix_save
 !
 interface
   !
   subroutine excitons_sort_and_report(E,BS_R,BS_E,BS_E_SOC_corr,S_z,S_sq)
     use pars,          ONLY:SP
     use BS,            ONLY:BS_H_dim
     use electrons,     ONLY:levels
     implicit none
     type(levels)       :: E
     complex(SP)        :: BS_R(BS_H_dim),BS_E(BS_H_dim)
     real(SP),optional,pointer :: BS_E_SOC_corr(:,:)
     real(SP),optional  :: S_z(BS_H_dim)
     real(SP),optional  :: S_sq(BS_H_dim)
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
