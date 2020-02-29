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
module YPPm
 !
 use pars,      ONLY:SP,DP,lchlen,schlen
 use R_lattice, ONLY:bz_samp
 use units,     ONLY:HA2EV
 use QP_m,      ONLY:QP_t
 use electrons, ONLY:levels
 use X_m,       ONLY:X_t
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
 real(SP), allocatable :: U_k_pts(:,:)
 real(SP), allocatable :: U_q_pts(:,:)
 real(SP), allocatable :: U_interp_pts(:,:)
 real(SP)              :: U_q_shifts(3,3)
 ! 
 ! DOS/BANDS/SPECIAL-K/INTERP grids
 !
 integer, parameter        :: n_SPECIAL_k_MAX=20
 real(SP),     allocatable :: CIRCUIT_k_norm(:)
 character(2), allocatable :: CIRCUIT_k_label(:)
 character(2) :: SPECIAL_k_label(n_SPECIAL_k_MAX)=" "
 type(bz_samp):: SPECIAL_k
 type(bz_samp):: USER_k
 type(bz_samp):: CIRCUIT_k
 logical      :: CIRCUIT_made_of_SPECIAL_k
 ! 
 ! DOS
 !
 logical      ::l_dos ! electrons and phonons
 real(SP)     ::DOS_broadening ! electrons
 real(SP)     ::DOS_E_range(2) ! Energy Range
 integer      ::DOS_bands(2)   ! Bands Range
 integer      ::DOS_E_steps    ! Energy steps
 real(SP)     ::DOS_E_step     ! Energy step
 !
 ! PDOS
 !
 logical      ::l_pdos
 integer      ::PDOS_atoms(2)
 integer      ::PDOS_kinds(2)
 integer      ::PDOS_wfcs(2)
 integer      ::PDOS_l(2)
 real(SP)     ::PDOS_j(2)
 integer      ::PDOS_m(2)
 !
 ! BANDS
 !
 integer               ::BANDS_bands(2)
 integer               ::BANDS_steps
 character(lchlen)     ::CIRCUIT_E_DB_path  = "none"
 character(schlen)     ::BANDS_path  = " "
 type(levels)          ::CIRCUIT_E_db
 integer               ::INTERP_grid(3)  ! Interpolation Grid
 real(SP)              ::Nel_fac
 logical               ::BANDS_print_internal_energies
 logical               ::BANDS_from_db
 !
 ! SPIN and SOC
 !
 integer      ::WF_ref(2)
 !
 ! REAL SPACE PLOT
 !
 logical      ::l_plot
 logical      ::l_wavefunction
 logical      ::l_density
 logical      ::l_bands
 logical      ::l_mag
 logical      ::l_interp
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
 integer      ::BSiq    
 real(SP)     ::r_hole(3)    
 real(SP)     ::deg_energy   
 real(SP)     ::EXCITON_E_treshold
 real(SP)     ::EXCITON_Res_treshold
 real(SP)     ::EXCITON_weight_treshold   
 real(SP)     ::WF_multiplier   
 character(schlen)    ::state_ctl 
 integer              :: EXCITONS_n_user_states=0
 integer, allocatable :: EXCITONS_user_indexes(:)
 type EXCITON_spin
   integer               :: N
   real(SP), allocatable :: N_SM(:,:,:)
   real(SP), allocatable :: S_sq(:)
   real(SP), allocatable :: S_z(:)
 end type EXCITON_spin
 type EXCITON_kin
   integer           :: ik_bz = 0
   integer           :: ik_ibz = 0
   integer           :: ik_sym = 0
   integer           :: iv = 0
   integer           :: ic = 0
   integer           :: spin = 0
 end type EXCITON_kin
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
 character(lchlen),  allocatable :: QP_db_input_control(:,:)
 character(lchlen),  allocatable :: QP_db_name(:)
 character(schlen),  allocatable :: QP_db_what(:)
 character(schlen),  allocatable :: QP_db_operation(:)
 real(SP),           allocatable :: QP_db_prefactor(:)
 real(SP),           allocatable :: QP_user_corrections(:,:)
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
 integer     :: ng_cut
 !
 ! Convert wave--functions
 !
 logical     :: l_fix_save
 !
 ! BSE data
 !
 complex(SP), allocatable ::BS_R_left(:)
 complex(SP), allocatable ::BS_R_right(:)
 complex(SP), allocatable ::BS_E(:)
 real(SP),    pointer     ::BS_E_SOC_corr(:,:) => null()
 real(SP),    pointer     ::BS_R_PL(:,:)       => null()
 complex(SP), pointer     ::BS_R_kerr(:)       => null()
 type(X_t)         ::Xbsk 
 !
 interface
   !
   subroutine k_grid_printer(nk,k,coo_in,form,wk,coo_out)
     use pars,          ONLY:SP
     integer               ::nk
     real(SP)              ::k(nk,3)
     character(*)          ::coo_in
     character(*)          ::form
     real(SP),    optional ::wk(nk)
     character(*),optional ::coo_out
     !
   end subroutine
   !
   subroutine excitons_degeneracy_average(N,BS_E_degs,R,C,VERB)
     use pars,          ONLY:SP
     use BS_solvers,    ONLY:BSS_n_eig
     implicit none
     integer               :: N,BS_E_degs(BSS_n_eig)
     real(SP),   optional  :: R(N)
     complex(SP),optional  :: C(N)
     logical,    optional  :: VERB
   end subroutine
   !
   subroutine electrons_dos_elemental(k,E,bands,el_dos,dos_E,WF_fac,PDOS_fac,USE_the_DbGd,QUIET,FORCE_occ)
     use pars,           ONLY:SP
     use electrons,      ONLY:levels
     use R_lattice,      ONLY:bz_samp 
     use electrons,      ONLY:n_spin
     implicit none
     type(bz_samp), intent(in) :: k
     type(levels),  intent(in) :: E
     integer,       intent(in) :: bands(2)
     real(SP),      intent(out):: el_dos(:,:),dos_E(:)
     real(SP), pointer,  intent(in) :: WF_fac(:,:,:)
     real(SP), pointer,  intent(in) :: PDOS_fac(:,:,:)
     logical,  optional, intent(in) :: USE_the_DbGd,QUIET,FORCE_occ
   end subroutine electrons_dos_elemental
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
end module YPPm
