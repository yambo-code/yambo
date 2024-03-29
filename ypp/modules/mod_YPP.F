!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module YPPm
 !
 use pars,      ONLY:SP,DP,lchlen,schlen
 use R_lattice, ONLY:bz_samp
 use units,     ONLY:HA2EV
 use QP_m,      ONLY:QP_t
 use electrons, ONLY:levels
 use X_m,       ONLY:X_t
 use DIPOLES,   ONLY:DIPOLE_t
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
 logical  :: l_regular_grid
 logical  :: l_high_sym_pts
 integer  :: N_path_pts
 integer  :: BZ_random_nkpts
 integer  :: K_grid(3)
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
 integer, parameter        :: n_SPECIAL_k_MAX=150
 real(SP),          allocatable :: CIRCUIT_k_norm(:)
 character(schlen), allocatable :: CIRCUIT_k_label(:)
 character(schlen) :: SPECIAL_k_label(n_SPECIAL_k_MAX)=" "
 type(bz_samp):: USER_k
 type(bz_samp):: SPECIAL_k
 type(bz_samp):: CIRCUIT_k
 logical      :: CIRCUIT_made_of_SPECIAL_k
 !
 ! Projection mode
 !
 type(bz_samp)        :: SLICE_k
 type(levels)         :: SLICE_E
 character(schlen)    :: PROJECT_mode="none" ! ATOM,LINE,PLANE
 character(schlen)    :: PROJECT_path="none" 
 integer              :: PROJECT_steps=-1
 integer              :: PROJECT_N_par=0
 integer              :: PROJECT_N_perp=1
 real(SP)             :: PROJECT_threshold=5.
 real(SP)             :: PROJECT_plane_X(3)
 real(SP)             :: PROJECT_plane_Y(3)
 integer, allocatable :: PROJECT_k_map(:)
 logical              :: l_PROJECT_atom
 logical              :: l_PROJECT_line
 logical              :: l_PROJECT_plane
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
 character(schlen)     ::INTERP_input_mode='BOLTZ'
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
 logical      ::l_add_EQ_occ,l_separate_eh,l_skip_occ
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
 ! Dipoles
 !
 type(DIPOLE_t)    ::DIPs
 character(schlen) ::DIPs_kind
 logical           ::l_dipoles
 integer           ::DIPs_C_bands(2)
 integer           ::DIPs_V_bands(2)
 real(SP)          ::DIPs_E_range(2)
 real(SP)          ::DIPs_direction(3)=(/1._SP,0._SP,0._SP/)
 real(SP), allocatable :: DIPs_mask(:,:,:)
 !
 ! Exc/SP Properties
 !
 logical      ::l_excitons
 logical      ::l_electrons
 logical      ::l_sort
 logical      ::l_amplitude
 logical      ::l_spin
 logical      ::l_free_hole
 logical      ::l_average_eh
 logical      ::l_angular_momentum
 logical      ::l_position
 integer      ::ncell(3) 
 integer      ::BSiq    
 logical      ::BSiq_via_command_line=.FALSE.
 real(SP)     ::r_hole(3)    
 real(SP)     ::deg_energy   
 real(SP)     ::EXCITON_E_treshold
 real(SP)     ::EXCITON_Res_treshold
 real(SP)     ::EXCITON_weight_treshold   
 real(SP)     ::WF_multiplier   
 character(schlen)    :: AVE_exc
 character(schlen)    :: state_ctl 
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
 logical            :: l_map_kpts
 integer, parameter :: MAX_FineGd_DBs=20
 integer            :: n_FineGd_DBs
 integer            :: BZ_FineGd_nkpts_to_use
 character(lchlen)  :: FineGd_DB1_paths(MAX_FineGd_DBs,1)
 !
 ! Perturbative SOC
 !
 logical           :: l_SOC_map
 integer           :: SOC_bands_to_map(2)
 integer           :: SOC_bands_buffer
 character(lchlen) :: perturbative_SOC_path
 real(SP)          :: SOC_split
 !
 ! QP databases
 !
 logical     :: l_QP_DBs_create_and_modify
 logical     :: l_QP_DBs_manipulate
 logical     :: l_QP_DB_expand
 integer     :: n_QP_dbs 
 character(lchlen)    ::QP_DB_to_expand  = "none"
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
 ! Reduce Symmetries
 !
 logical     :: l_fix_syms
 real(SP)    :: E_field(2,3)
 real(SP)    :: B_field,B_theta,B_psi
 !
 ! BSE data
 !
 complex(SP), pointer     ::BS_R_left(:)
 complex(SP), pointer     ::BS_R_right(:)
 complex(SP), allocatable ::BS_E(:)
 real(SP),    pointer     ::BS_E_SOC_corr(:,:) => null()
 real(SP),    pointer     ::BS_R_PL(:,:)       => null()
 complex(SP), pointer     ::BS_R_kerr(:)       => null()
 type(X_t)                ::Xbsk 
 !
 ! NL
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
 end interface
 !
 contains
   !
   subroutine K_transform(k,in_mode)
     !
     use stderr,       ONLY:STRING_same
     use units,        ONLY:pi
     use vec_operate,  ONLY:c2a
     !
     real(SP)     :: k(3)
     character(*) :: in_mode
     !
     select case (in_mode)
       case ('cc')
         if (STRING_same(trim(coo_out),"iku"))  call c2a(v_in=k,mode="kc2i")
         if (STRING_same(trim(coo_out),"rlu"))  call c2a(v_in=k,mode="kc2a")
         if (STRING_same(trim(coo_out),"alat")) then
           k=k*alat_used_for_output/2./pi
         endif
       case ('iku')
         if (STRING_same(trim(coo_out),"cc"))   call c2a(v_in=k,mode="ki2c")
         if (STRING_same(trim(coo_out),"rlu"))  call c2a(v_in=k,mode="ki2a")
         if (STRING_same(trim(coo_out),"alat")) then
           call c2a(v_in=k,mode="ki2c")
           k=k*alat_used_for_output/2./pi
         endif
       case ('rlu')
         if (STRING_same(trim(coo_out),"cc"))   call c2a(v_in=k,mode="ka2c")
         if (STRING_same(trim(coo_out),"iku"))  call c2a(v_in=k,mode="ka2i")
         if (STRING_same(trim(coo_out),"alat")) then
           call c2a(v_in=k,mode="ka2c")
           k=k*alat_used_for_output/2./pi
         endif
     end select
     !
   end subroutine
   !
end module YPPm
