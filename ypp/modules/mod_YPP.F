!
!        Copyright (C) 2000-2016 the YAMBO team
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
 ! EL-PH
 !
 logical :: l_gkkp
 logical :: l_gkkp_expand
 logical :: l_phonons
 logical :: l_eliashberg
 logical :: l_atomic_amplitude
 integer :: elph_steps
 real(SP):: elph_gamma_broad
 real(SP):: ph_broad
 real(SP):: elph_Ef
 real(SP):: elph_E_max
 real(SP):: elph_residual_treshold = 0.
 character(schlen) :: elph_dbs_path
 character(schlen) :: ph_freqs_file
 character(schlen) :: ph_modes_file
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
 character(lchlen) :: perturbative_SOC_path
 real(SP)          :: SOC_split
 !
 ! QP databases
 !
 logical     :: l_QP_DBs_create_and_modify
 logical     :: l_QP_DBs_merge
 integer     :: n_QP_dbs 
 character(lchlen),  allocatable :: QP_db_name(:)
 character(schlen),  allocatable :: QP_db_what(:)
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
 ! RT, merge QP DBs
 !
 real(SP) :: Z_input
 !
 ! SC
 !
 logical  :: l_mean_potential
 !
 ! RT
 !
 logical  :: l_RealTime
 !
 ! (RT) BANDS
 !
 logical  :: l_RT_occ_Bands
 logical  :: l_RT_life_Bands
 !
 ! Wannier bands
 !
 logical           :: l_bxsf
 character(lchlen) :: w90_fname
 integer           :: wannier_bands(2)   ! Bands Range
 integer           :: ord_dgrid_ipol     ! order of interpolation using double grid
 !
 ! (RT) Occupations
 !
 logical     :: l_RT_occ_time
 logical     :: l_RT_occ_energy
 logical     :: l_RT_occ_2D_dos
 !
 ! Reduce Symmetries
 !
 logical     :: l_fix_syms
 real(SP)    :: E_field(2,3)
 real(SP)    :: B_field,B_theta,B_psi
 !
 ! (RT) Retarded Green's functions
 !
 integer  :: Ret_GF_bands(2)      ! Bands Range
 real(SP) :: Gr_E_step
 real(SP) :: GF_T_step
 !
 ! (RT) optics
 !
 logical  :: Use_FFT
 logical  :: l_skip_pol_and_curr_IO
 logical  :: l_force_min_damp
 logical  :: l_RT_X
 logical  :: l_RT_abs
 logical  :: l_RT_X_inversion
 integer  :: i_t_start,i_t_end ! time window
 integer  :: RED_steps ! reduced number of steps
 integer  :: N_probes
 integer  :: N_pumps
 integer  :: N_probe_frequencies
 integer  :: X_order
 integer  :: J_steps
 real(SP) :: t_jump
 real(SP) :: W_RT_induced_step
 real(SP) :: J_time_step
 character(schlen) :: Probe_Keyword
 character(schlen) :: Pump_Keyword
 integer, parameter:: max_n_of_paths=10000
 character(schlen) :: Pump_path
 character(schlen) :: Probe_path(max_n_of_paths)
 !
 complex(DP),    allocatable :: RT_J_t(:,:)
 complex(DP),    allocatable :: RT_P_t(:,:)
 !
 type Xw_rt
   real(SP) :: time_range(2)       ! time windows for the transform
   integer  :: energy_steps        ! number of steps in energy mesh
   real(SP) :: energy_range(2)     ! energy range 
   character(schlen) :: damp       ! Damp type (none, lorentian or gaussian)
   real(SP) :: damp_factor         ! factor used in damping
 end type Xw_rt
 !
 type(Xw_rt) :: X_rt 
 !
 interface
   !
   subroutine excitons_sort_and_report(Xk,E,BS_R,BS_E,S_z,S_sq)
     use pars,          ONLY:SP
     use BS,            ONLY:BS_H_dim
     use R_lattice,     ONLY:bz_samp
     use electrons,     ONLY:levels
     implicit none
     type(bz_samp)      :: Xk
     type(levels)       :: E
     complex(SP)        :: BS_R(BS_H_dim),BS_E(BS_H_dim)
     real(SP),optional  :: S_z(BS_H_dim)
     real(SP),optional  :: S_sq(BS_H_dim)
   end subroutine
   !
   subroutine  ELPH_general_gFsq(k,en,Xk,Xen,q,BS_E_degs,lambda_s,n_lambda)
     use R_lattice,     ONLY:bz_samp
     use electrons,     ONLY:levels
     implicit none
     type(bz_samp)      :: k,q,Xk
     type(levels)       :: en,Xen
     integer, optional  :: BS_E_degs(:)
     integer, optional  :: lambda_s(:)
     integer, optional  :: n_lambda
   end subroutine
   !
   subroutine  RT_IO_J_and_P(IO_error,first_sec_only,G_lesser_only,COM_,mesg)
     use electrons,     ONLY:levels
     implicit none
     logical, optional  :: IO_error,first_sec_only,G_lesser_only
     integer, optional  :: COM_
     character(*), optional  :: mesg
   end subroutine
   !
   subroutine RT_IO_carriers(Time,imposed_COM)
     use pars,       ONLY:SP
     real(SP), optional, intent(inout) :: Time
     integer , optional, intent(in)    :: imposed_COM
   end subroutine RT_IO_carriers
   !
   subroutine electrons_bands_interpolate(Xen,Xk,ID_interp,USER_k,INTERP_k,CIRCUIT_k,BANDS_range,bands_output)
     use pars,       ONLY:SP
     use electrons,  ONLY:levels
     use R_lattice,  ONLY:bz_samp
     type(bz_samp), intent(in)  :: Xk
     type(levels),  intent(in)  :: Xen
     integer      , intent(in)  :: BANDS_range(2)
     integer      , intent(out) :: ID_interp
     type(bz_samp), intent(out) :: USER_K,CIRCUIT_K,INTERP_K
     real(SP)     , pointer     :: bands_output(:,:,:,:)
   end subroutine electrons_bands_interpolate
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
#if defined _YPP_RT
   !
   subroutine RT_IO_time_range_setup(N_times)
     !
     use units,          ONLY:FS2AUT
     use com,            ONLY:msg
     use real_time,      ONLY:RT_step,NE_time
     use rt_ctl,         ONLY:RT_IO_JPO_steps,OCC_deltaT,OCC_T_range
     !
     integer :: N_times
     !
     if (OCC_deltaT<RT_IO_JPO_steps*RT_step) then
       ! 
       OCC_deltaT=RT_IO_JPO_steps*RT_step
       ! 
     else
       !
       OCC_deltaT=nint(OCC_deltaT/RT_IO_JPO_steps/RT_step)*RT_IO_JPO_steps*RT_step
       !
     endif
     !
     N_times=nint( (OCC_T_range(2)-OCC_T_range(1))/OCC_deltaT )+1
     !
     if (N_times<0) return
     !
     if (NE_time<OCC_T_range(2)) then
       OCC_T_range(2)=NE_time
       if(OCC_deltaT>NE_time) OCC_deltaT=NE_time
       N_times=nint(OCC_T_range(2)/OCC_deltaT)+1
     endif
     !
     call msg('s','Time range [fs]:',OCC_T_range/FS2AUT)
     call msg('s','Time step  [fs]:',OCC_deltaT/FS2AUT)
     call msg('s','Time steps     :',N_times)
     !
   end subroutine
   !
#endif
   !
end module YPP
