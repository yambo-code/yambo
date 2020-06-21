!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG DS
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
module real_time
 !
 use drivers,    ONLY:l_elel_scatt,l_elph_scatt,l_elphoton_scatt
 use pars,       ONLY:DP,SP,schlen,lchlen,LP
 use units,      ONLY:FS2AUT
 use global_XC,  ONLY:QP_SE_NONE
 !
#include<memory.h>
 !
 ! CORE LOGICALS
 !
 ! NE dynamics
 !
 logical           :: l_NE_dynamics
 logical           :: l_NE_with_fields
 logical           :: l_Dynamics_is_over
 logical           :: l_Update_Time_Step  = .FALSE.
 logical           :: l_RT_is_WFfree
 !
 character(schlen) :: RT_potential
 !
 ! Possible Kinds, functionals and external perturbations
 !
 integer           :: RT_kind
 integer           :: RT_xc_functional
 integer           :: RT_perturbation
 !
 ! Evaluation logicals
 !
 logical          :: eval_energy
 logical          :: eval_entropy
 logical          :: eval_HARTREE
 logical          :: eval_DFT
 !
 ! To include the induced field in the coupling and current
 !
 logical          :: l_RT_induced_field = .FALSE.
 logical          :: l_P_integrating_J
 logical          :: l_velocity_gauge_corr
 logical          :: l_velocity_gauge_diam
 !
 ! TMP SOLUTION FOR TR ABS
 !
 logical  :: l_RT_abs_YAMBO = .false.
 !
 ! ... core step length and Time-points array...
 !
 real(SP)              :: RT_step             ! Elemental step used to define dimensions and time-points
 real(SP)              :: RT_dyn_step = 0._SP ! On-the-fly time-step
 real(SP)              :: RT_MAX_step =-1._SP*FS2AUT ! MAX value of the dynamical RT step value
 !
 ! ... NE dynamics ...
 !
 real(SP) :: NE_time    =0._SP  ! <- incremented
 real(SP) :: NE_tot_time=0._SP  ! Total (fixed)
 !
 integer  :: NE_time_step_update_last_point      ! Last time-point of the dT update
 real(SP) :: NE_time_step_update_jump            ! Time BETWEEN two time-steps dynamically updated (0. means RT_step)
 real(SP) :: NE_time_step_update_jump_SAVE
 real(SP) :: NE_initial_time_step_update         ! Time above which the time-step is dynamically updated (0. means no update)
 real(SP) :: NE_step_update_treshold             ! Time-step update treshold
 !
 integer  :: NE_i_time=1 ! = (NE_time+1)/RT_step
 integer  :: NE_steps    ! Still relative to RT_step
 !
 ! Here I control the way the time evolution is sampled for the e-p and e-e
 ! channels. I will see later if this can be somehow extended/duplicated
 ! for the static correlations
 !
 logical  :: l_RT_lifetimes_extrapolation = .FALSE.! If .TRUE. the lifetimes not calculated are extrapolated. Otherwise are kept constant
 real(SP) :: RT_life_extrap_times(2)               ! Lifetimes are evaluated at distance RT_life_extrap_times(2) and in groups long RT_life_extrap_times(1)
 real(SP) :: RT_life_extrap_times_SAVE(2)
 !
 logical  :: l_RT_relax_time_approx = .FALSE.
 real(SP) :: RT_RTA_temp(2)
 real(SP) :: RT_RTA_chem(2)
 real(SP), allocatable :: RT_RTA_E_occ_infty(:,:)
 real(SP), allocatable :: RT_RTA_H_occ_infty(:,:)
 !
 ! Lifetimes Extrapolation Time status
 !
 integer(LP), allocatable :: RT_TIME_status(:)
 !
 ! Kind of e-e interactions (for the CORR and SCATT parts)
 !
 integer           :: RT_EE_CORR_kind=QP_SE_NONE
 integer           :: RT_EE_SCATT_kind=QP_SE_NONE
 !
 ! ... Memory ...
 !
 real(SP) :: NE_MEM_treshold
 integer  :: Life_MEM_steps
 integer  :: G_MEM_steps                ! By default only G_lesser@t and @t+1
 integer  :: i_MEM_prev                 ! Updated in RT_driver
 integer  :: i_MEM_now                  ! Updated in RT_driver
 integer  :: i_MEM_old                  ! Updated in RT_driver
 integer, allocatable :: MEM_pointer(:) ! Time memory pointers
 !
 ! ... Window on the k-points/bands allowed to follow the dynamics ...
 !
 real(SP)             :: RT_eh_en(2)
 real(SP)             :: RT_scatt_tresh
 !
 ! Macroscopic induced current ...
 !--------------------------------
 !
 complex(SP) :: RT_ind_J(3)
 complex(SP) :: RT_ind_J_prev(3)
 !
 ! ... polarization ...
 !----------------------
 complex(SP) :: RT_P(3)
 complex(SP) :: RT_P_prev(3)
 !
 ! ... and magnetization
 !----------------------
 complex(SP) :: RT_spin_magn(3,3)
 complex(SP) :: RT_orbt_magn(3,2)
 !
 ! Diagonal (QP) and off-diagonal (Phase) relaxation time
 !
 real(SP)    :: Phase_LifeTime
 real(SP)    :: RAD_LifeTime
 real(SP)    :: RAD_magnification=1._SP
 real(SP)    :: RT_deph_deg_thresh
 logical     :: RT_deph_cv_only
 logical     :: RT_is_statically_dephased=.FALSE.
 logical     :: RT_is_dynamically_dephased=.FALSE.

 !
 ! Table pointing to the position of the uniform grid points in the
 ! G_lesser structure
 !
 integer,     allocatable :: RT_ibz_coo(:,:)
 integer,     allocatable :: RT_ibz_dupl(:)
 integer,     allocatable :: RT_all2ibz(:)
 integer,     allocatable :: RT_Nk_ibz(:)
 !
 ! K-points: In case I want to use the G_lesser to define the occupations
 ! of a post-run I need to keep here the number of k-points
 !
 integer                 :: RT_nk
 integer                 :: RT_DbGd_nk
 integer                 :: RT_bands(2)
 integer                 :: RT_nbands
 !
 ! Equilibrium V_xc and V_hartree in real space
 ! Presently REF_V_hartree_sc is not used
 !
 real(SP),    allocatable :: REF_V_xc_sc(:,:)
 real(SP),    allocatable :: REF_V_hartree_sc(:,:)
 !
 ! Update Energies, eval Lifetimes rotate DM (density matrix)
 !-----------------------------------------------------------
 logical                  :: l_eval_Lifetimes
 logical                  :: l_RT_update_Energies
 logical                  :: l_RT_rotate_DM
 logical                  :: l_update_SOC
 !
 ! Relaxation and Dissipation
 !
 !----------------------------
 logical                  :: l_RT_uses_E_FineGd
 logical                  :: l_RT_eq_is_zero_temp
 logical                  :: l_RT_include_eq_scatt
 logical                  :: l_RT_impose_for_metals
 logical,     allocatable :: DbGd_EE_scatt_table(:,:)  
 integer,     allocatable :: DbGd_EE_Nk_table(:,:)  
 real(SP)                 :: DbGd_EE_percent 
 !
 ! ELPH kernel...
 !----------------
 !
 logical     :: l_RT_CCA_Kernel
 !
 ! ... Q weights calculated using RIM in order to evaluate the ELPH kernel
 !
 real(SP),    allocatable :: q_weight_RT(:,:,:)
 ! ... THETA matrix
 complex(SP), allocatable :: THETA_matrix(:,:,:,:)
 ! ... Q-parallelization  and GKKP
 real(SP),    allocatable :: elph_gkkp_expanded_sq(:,:,:,:,:)
 ! ... indexes ...
 integer,     allocatable :: THETA_matrix_index(:,:,:)
 integer,     allocatable :: Q_io_map(:)
 integer,     allocatable :: K_io_map(:)
 !
 ! Conservations
 !----------------------
 logical                  :: l_RT_impose_N_dN = .TRUE.
 logical                  :: l_RT_impose_E    = .TRUE.
 !
 ! Energies
 !---------------------
 type RT_DbGd
   real(SP), allocatable :: E(:,:,:)
   real(SP), allocatable :: W(:,:,:)
   real(SP), allocatable :: k(:,:)
 end type
 !
 type(RT_DbGd),save  :: RT_E_and_k
 ! 
 ! Locks, reference & Logicals
 !-----------------------------
 logical                  :: l_RT_relaxation_update_reference
 logical                  :: l_RT_constant_lifetimes
 logical                  :: l_RT_extrapolate_lifetimes
 logical                  :: l_RT_eval_lifetimes
 logical                  :: l_initial_equilibrium
 !
 real(SP),    allocatable :: RT_E_occupations(:,:)
 real(SP),    allocatable :: RT_H_occupations(:,:)
 real(SP),    allocatable :: RT_delta_occupations(:,:)       ! Running values. Updated at each time step.
 real(SP),    allocatable :: RT_life_delta_occupations(:,:)  ! Used in the Lifetimes definition. Defined in the Lifetimes procedure only.
 real(SP),    allocatable :: RT_E_occupations_reference(:,:)
 real(SP),    allocatable :: RT_H_occupations_reference(:,:)
 !
 logical,     allocatable :: RT_occupations_locked(:,:)
 !
 ! EL-PH RT life-times
 !-----------------------------------------
 real(SP),    allocatable :: E_P_Hole_lifetime(:,:)
 real(SP),    allocatable :: E_P_Electron_lifetime(:,:)
 real(SP),    allocatable :: E_P_El_abs_lifetime(:,:)
 real(SP),    allocatable :: E_P_Ho_abs_lifetime(:,:)
 !
 ! Coulomb RT life-times
 !-----------------------
 real(SP),    allocatable :: E_E_Hole_lifetime(:,:)
 real(SP),    allocatable :: E_E_Electron_lifetime(:,:)
 !
 ! EL-PHOT RT life-times
 !-----------------------
 real(SP),    allocatable :: E_PHOT_Hole_lifetime(:,:)
 real(SP),    allocatable :: E_PHOT_Electron_lifetime(:,:)
 !
 ! Saved RT life-times
 !-----------------------
 real(SP),    allocatable :: SAVED_times(:)
 real(SP),    allocatable :: REF_lifetime(:,:,:)
 real(SP),    allocatable :: SAVED_lifetime(:,:,:,:)
 !
 ! Ouput Lifetimes, ratio fit and temperatures
 !---------------------------------------------
 logical                  :: l_lftm_fit_stable(2)
 logical                  :: l_lftm_fit_temperatures
 integer                  :: Nfitted_lifetimes(2)
 real(SP)                 :: RT_mean_lifetimes(8) ! 1-2 (e-e), 3-6 (e-p), 7-8 (e-photon)
 real(SP)                 :: RT_Tfit_lifetimes(4)
 real(SP)                 :: RT_Efit_lifetimes(4)
 !
 real(SP)                 :: EE_prefactor(2)      ! 1=valence, 2=conduction
 real(SP)                 :: RAD_prefactor(2)
 real(SP)                 :: EP_prefactor(2)
 real(SP)                 :: EP_abs_prefactor(2)
 !
 ! Relaxation: Dephasing Matrix
 !------------------------------
 !
 real(SP)                 :: RT_ave_dephasing
 real(SP),    allocatable :: RT_INPUT_Deph_limits(:,:)
 real(SP),    allocatable :: RT_Deph_Matrix(:,:,:)
 !
 ! GFs (retarded)
 !----------------
 !
 integer           :: two_alpha
 real(SP)          :: Gr_QP_width
 character(schlen) :: Gr_kind
 !
 !
 ! GFs
 !-----
 !
 complex(SP), allocatable :: G_lesser(:,:,:,:)          ! Band, Band, K, Time
 complex(SP), allocatable :: dG_lesser(:,:,:,:)
 complex(SP), allocatable :: G_lesser_reference(:,:,:)  ! Band, Band, K
 complex(SP), allocatable :: I1_matrix(:,:)
 !
 ! COLLISIONS
 !------------
 complex(SP), allocatable :: Ho_plus_Sigma(:,:,:,:)
 complex(SP), allocatable :: H_pseudo_eq(:,:,:,:)
 complex(SP), allocatable :: RT_Vnl_xc(:,:,:,:)
 !
 ! Density
 !---------
 !
 real(SP),    allocatable :: rho_reference(:)    ! reference density from all bands
 real(SP),    allocatable :: magn_reference(:,:) ! reference magnetization from all bands
 !
 ! Integrators
 !
 logical  :: l_RT_EULER
 logical  :: l_RT_EXP
 logical  :: l_RT_INV
 !
 logical  :: l_RT_RWA
 logical  :: l_RT_ACC
 logical  :: l_RT_PERT
 logical  :: l_RT_DIAG
 !
 logical  :: l_RT_SIMPLE
 logical  :: l_RT_RK2
 logical  :: l_RT_RK4
 logical  :: l_RT_HEUN
 !
 integer  :: Integrator_step
 integer  :: Integrator_nsteps
 integer  :: Integrator_exp_order
 !
 real(SP) :: a_tableau(4)
 real(SP) :: b_tableau(4)
 real(SP) :: c_tableau(4)
 real(SP) :: RWA_table(5)
 !
 ! Integrate exactly the part of the dynamics due to H_o
 ! as it is done in Comp. Phys. Comm. 123(1999)
 ! in this case I choosed H_o = H_lda ( rho_0 )
 !
 character(schlen)            :: Integrator_name
 !
 ! Hartree term
 !
 complex(SP), allocatable :: XC0(:,:,:,:)
 complex(SP), allocatable :: HARTREE0(:,:,:,:)
 !
 type RT_t
   !
   ! Sizes and definitions
   !
   integer  :: nk
   integer  :: nb(2)
   integer  :: nstates
   character(schlen)  :: description
   integer,    allocatable:: table(:,:)      ! ib,ik_RT,ik,i_spin
   real(SP)   ,allocatable:: kpt(:,:)
   real(SP)   ,allocatable:: k_weight(:)
   !
   ! Energies
   !
   real(SP)   ,allocatable:: E_bare(:)       ! equilibrium energy, i.e. the QP energy
   real(SP)   ,allocatable:: delta_E(:)      ! change in E (BGR)
   !
   ! Occupations
   !
   real(SP)   ,allocatable:: f_bare(:)       ! equilibrium occupations
   real(SP)   ,allocatable:: delta_f(:)      ! change in the occupations
   !
   ! Lifetimes
   !
   real(SP)   ,allocatable:: GAMMA_bare(:,:) ! "initial equilibrium" lifetimes. In the code the "initial equilibrium"
                                                   ! e-e h, e-e e, e-p h, e-p e, e-p h(abs), e-p e(abs)
   real(SP)   ,allocatable:: NEQ_Lifetime(:) ! time dependent lifetime defined as for Silicon
   real(SP)   ,allocatable:: delta_GAMMA(:)  ! change with respect to the "initial equilibrium" lifetimes
   real(SP)   ,allocatable:: dGAMMA_E_P_Hole(:)
   real(SP)   ,allocatable:: dGAMMA_E_P_Electron(:)
   real(SP)   ,allocatable:: dGAMMA_E_P_Ho_abs(:)
   real(SP)   ,allocatable:: dGAMMA_E_P_El_abs(:)
   real(SP)   ,allocatable:: dGAMMA_E_E_Hole(:)
   real(SP)   ,allocatable:: dGAMMA_E_E_Electron(:)
   real(SP)   ,allocatable:: dGAMMA_E_PHOT_Hole(:)
   real(SP)   ,allocatable:: dGAMMA_E_PHOT_Electron(:)
   !
 end type RT_t
 !
 type(RT_t),save  :: RT_carriers
 !
 contains
   !
   subroutine RT_obj_alloc(RT)
     type(RT_t)::RT
     !
     if (allocated(RT%kpt)) return
     !
     YAMBO_ALLOC(RT%kpt,(RT%nk,3))
     YAMBO_ALLOC(RT%k_weight,(RT%nk))
     YAMBO_ALLOC(RT%table,(RT%nstates,4))
     YAMBO_ALLOC(RT%E_bare,(RT%nstates))
     YAMBO_ALLOC(RT%f_bare,(RT%nstates))
     RT%table         =0
     RT%E_bare        =0._SP
     RT%f_bare        =0._SP
     if (l_elel_scatt) then
       YAMBO_ALLOC(RT%dGAMMA_E_E_Hole,(RT%nstates))
       YAMBO_ALLOC(RT%dGAMMA_E_E_Electron,(RT%nstates))
       RT%dGAMMA_E_E_Hole    =0._SP
       RT%dGAMMA_E_E_Electron=0._SP
     endif
     if (l_elph_scatt) then
       YAMBO_ALLOC(RT%dGAMMA_E_P_Hole,(RT%nstates))
       YAMBO_ALLOC(RT%dGAMMA_E_P_Electron,(RT%nstates))
       YAMBO_ALLOC(RT%dGAMMA_E_P_Ho_abs,(RT%nstates))
       YAMBO_ALLOC(RT%dGAMMA_E_P_El_abs,(RT%nstates))
       RT%dGAMMA_E_P_Hole    =0._SP
       RT%dGAMMA_E_P_Electron=0._SP
       RT%dGAMMA_E_P_Ho_abs  =0._SP
       RT%dGAMMA_E_P_El_abs  =0._SP
     endif
     if (l_elphoton_scatt) then
       YAMBO_ALLOC(RT%dGAMMA_E_PHOT_Hole,(RT%nstates))
       YAMBO_ALLOC(RT%dGAMMA_E_PHOT_Electron,(RT%nstates))
       RT%dGAMMA_E_PHOT_Hole    =0._SP
       RT%dGAMMA_E_PHOT_Electron=0._SP
     endif
     if (l_elel_scatt.or.l_elph_scatt.or.l_elphoton_scatt) then
       YAMBO_ALLOC(RT%GAMMA_bare,(RT%nstates,8))
       YAMBO_ALLOC(RT%delta_GAMMA,(RT%nstates))
       YAMBO_ALLOC(RT%NEQ_Lifetime,(RT%nstates))
       RT%GAMMA_bare  =0._SP
       RT%delta_GAMMA =0._SP
       RT%NEQ_Lifetime=0._SP
     endif
     YAMBO_ALLOC(RT%delta_E,(RT%nstates))
     YAMBO_ALLOC(RT%delta_f,(RT%nstates))
     RT%delta_f=0._SP
     RT%delta_E=0._SP
   end subroutine
   !
   subroutine RT_obj_free(RT)
     type(RT_t)::RT
     RT%nk=0
     RT%nb=0
     RT%nstates=0
     YAMBO_FREE(RT%dGAMMA_E_E_Hole)
     YAMBO_FREE(RT%dGAMMA_E_E_Electron)
     YAMBO_FREE(RT%dGAMMA_E_P_Hole)
     YAMBO_FREE(RT%dGAMMA_E_P_Electron)
     YAMBO_FREE(RT%dGAMMA_E_P_Ho_abs)
     YAMBO_FREE(RT%dGAMMA_E_P_El_abs)
     YAMBO_FREE(RT%dGAMMA_E_PHOT_Hole)
     YAMBO_FREE(RT%dGAMMA_E_PHOT_Electron)
     YAMBO_FREE(RT%E_bare)
     YAMBO_FREE(RT%f_bare)
     YAMBO_FREE(RT%GAMMA_bare)
     YAMBO_FREE(RT%delta_E)
     YAMBO_FREE(RT%delta_f)
     YAMBO_FREE(RT%delta_GAMMA)
     YAMBO_FREE(RT%NEQ_Lifetime)
     YAMBO_FREE(RT%kpt)
     YAMBO_FREE(RT%k_weight)
     YAMBO_FREE(RT%table)
   end subroutine
   !
   subroutine RT_alloc(en,what)
     !
     use pars,           ONLY:SP,LP,schlen,cZERO
     use parallel_m,     ONLY:PAR_G_k_range
     use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_use_Hxc_collisions
     use hamiltonian,    ONLY:H_alloc,H_QP_table_setup
     use FFT_m,          ONLY:fft_size
     use ELPH,           ONLY:ph_modes
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use xc_functionals, ONLY:E_xc
     use R_lattice,      ONLY:nqbz,nXkibz
     !
     implicit none
     !
     type(levels), intent(in) :: en
     character(*), intent(in) :: what
     !
     integer :: i_mem,nk(2)
     logical :: l_carriers,l_all,l_G_lesser
     !
     l_carriers = trim(what)=="carriers"
     l_G_lesser = trim(what)=="Glesser"
     l_all      = trim(what)=="all"
     !
     call H_alloc(en,.not.l_RT_is_WFfree,l_RT_rotate_DM)
     !
     call H_QP_table_setup(en)
     !
     nk=PAR_G_k_range
     RT_nbands=RT_bands(2)-RT_bands(1)+1
     !
     YAMBO_ALLOC(RT_E_and_k%E,(RT_bands(1):RT_bands(2),RT_nk,n_sp_pol))
     YAMBO_ALLOC(RT_E_and_k%W,(RT_bands(1):RT_bands(2),RT_nk,n_sp_pol))
     YAMBO_ALLOC(RT_E_and_k%k,(3,RT_nk))
     !
     if(l_RT_relax_time_approx) then
       YAMBO_ALLOC(RT_RTA_E_occ_infty,(RT_bands(1):RT_bands(2),RT_nk))
       YAMBO_ALLOC(RT_RTA_H_occ_infty,(RT_bands(1):RT_bands(2),RT_nk))
     endif
     !
     if (l_elphoton_scatt) then
       YAMBO_ALLOC(E_PHOT_Hole_lifetime,(RT_bands(1):RT_bands(2),RT_nk))
       YAMBO_ALLOC(E_PHOT_Electron_lifetime,(RT_bands(1):RT_bands(2),RT_nk))
       E_PHOT_Hole_lifetime=0._SP
       E_PHOT_Electron_lifetime=0._SP
     endif
     !
     if (l_elel_scatt) then
       YAMBO_ALLOC(E_E_Hole_lifetime,(RT_bands(1):RT_bands(2),RT_nk))
       YAMBO_ALLOC(E_E_Electron_lifetime,(RT_bands(1):RT_bands(2),RT_nk))
       E_E_Hole_lifetime=0._SP
       E_E_Electron_lifetime=0._SP
     endif
     !
     if (l_RT_CCA_Kernel.and.l_elph_scatt) then
       YAMBO_ALLOC(E_P_Hole_lifetime,(RT_bands(1):RT_bands(2),RT_nk))
       YAMBO_ALLOC(E_P_Electron_lifetime,(RT_bands(1):RT_bands(2),RT_nk))
       YAMBO_ALLOC(E_P_El_abs_lifetime,(RT_bands(1):RT_bands(2),RT_nk))
       YAMBO_ALLOC(E_P_Ho_abs_lifetime,(RT_bands(1):RT_bands(2),RT_nk))
       E_P_Hole_lifetime=0._SP
       E_P_Electron_lifetime=0._SP
       E_P_El_abs_lifetime=0._SP
       E_P_Ho_abs_lifetime=0._SP
     endif
     !
     if (l_elel_scatt.or.(l_RT_CCA_Kernel.and.l_elph_scatt).or.l_elphoton_scatt) then
       YAMBO_ALLOC(REF_lifetime,(RT_bands(1):RT_bands(2),RT_nk,8))
       REF_lifetime=0._SP
       if (Life_MEM_steps>0) then
         YAMBO_ALLOC(SAVED_lifetime,(RT_bands(1):RT_bands(2),RT_nk,8,Life_MEM_steps))!e-eh,e-ee,e-ph,e-pe,e-ph(abs),e-pe(abs),e-gh,e-g)
         YAMBO_ALLOC(SAVED_times,(Life_MEM_steps))
         SAVED_lifetime=0._SP
         SAVED_times=0._SP
       endif
     endif
     !
     if(l_all.or.l_carriers) then
       !
       ! Occ's ...
       YAMBO_ALLOC(RT_H_occupations,(RT_bands(1):RT_bands(2),RT_nk))
       YAMBO_ALLOC(RT_delta_occupations,(RT_bands(1):RT_bands(2),RT_nk))
       YAMBO_ALLOC(RT_life_delta_occupations,(RT_bands(1):RT_bands(2),RT_nk))
       YAMBO_ALLOC(RT_E_occupations_reference,(RT_bands(1):RT_bands(2),RT_nk))
       RT_E_occupations_reference=0._SP
       YAMBO_ALLOC(RT_H_occupations_reference,(RT_bands(1):RT_bands(2),RT_nk))
       RT_H_occupations_reference=0._SP
       YAMBO_ALLOC(RT_occupations_locked,(RT_bands(1):RT_bands(2),RT_nk))
       RT_occupations_locked=.false.
       YAMBO_ALLOC(RT_E_occupations,(RT_bands(1):RT_bands(2),RT_nk))
       !
     endif
     !
     if(l_all .or. l_G_lesser) then
       !
       YAMBO_ALLOC(Ho_plus_Sigma,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nk(1):nk(2),n_sp_pol))
       !
       if (l_velocity_gauge_corr) then
         YAMBO_ALLOC(H_pseudo_eq,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,n_sp_pol))
       endif
       !
       if(l_use_Hxc_collisions) then
         YAMBO_ALLOC(RT_Vnl_xc,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nXkibz,n_sp_pol))
         RT_Vnl_xc=(0._SP,0._SP)
       endif
       !
       YAMBO_ALLOC(HARTREE0,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nXkibz,n_sp_pol))
       HARTREE0=cZERO
       !
       YAMBO_ALLOC(XC0,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nXkibz,n_sp_pol))
       XC0=cZERO
       !
       ! G's ...
       YAMBO_ALLOC(G_lesser_reference,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk))
       YAMBO_ALLOC(dG_lesser,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nk(1):nk(2),G_MEM_steps))
       YAMBO_ALLOC(G_lesser,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nk(1):nk(2),G_MEM_steps))
       G_lesser_reference=cZERO
       dG_lesser         =cZERO
       G_lesser          =cZERO
       !
       YAMBO_ALLOC(MEM_pointer,(G_MEM_steps))
       MEM_pointer(1)=1
       MEM_pointer(2)=0
       if(G_MEM_steps==3) MEM_pointer(3)=-1
       !
     endif
     !
     if (fft_size==0) return
     !
     YAMBO_ALLOC(E_xc,(fft_size))
     !
     YAMBO_ALLOC(REF_V_xc_sc,(fft_size,n_spin))
     YAMBO_ALLOC(REF_V_hartree_sc,(fft_size,n_spin))
     !
     if( .not.l_RT_is_WFfree ) then
       !
       YAMBO_ALLOC(rho_reference,(fft_size))
       !
       if(n_spin>1) then
         YAMBO_ALLOC(magn_reference,(fft_size,3))
       endif
       !
     endif
     !
   end subroutine RT_alloc
   !
   subroutine RT_free(what)
     !
     use DIPOLES,        ONLY:DIP_alloc
     use hamiltonian,    ONLY:H_free
     use xc_functionals, ONLY:E_xc
     !
     implicit none
     !
     character(*), intent(in) :: what
     !
     logical :: l_carriers,l_all,l_G_lesser
     !
     l_carriers = trim(what)=="carriers"
     l_G_lesser = trim(what)=="Glesser"
     l_all      = trim(what)=="all"
     !
     call H_free()
     !
     call DIP_alloc('DIP_iR')
     call DIP_alloc('DIP_P')
     call DIP_alloc('DIP_v')
     call DIP_alloc('DIP_spin')
     call DIP_alloc('DIP_orbital')
     call DIP_alloc('P_square')
     call DIP_alloc('DIP_P_spinor')
     !
     YAMBO_FREE(RT_ibz_coo)
     YAMBO_FREE(RT_all2ibz)
     YAMBO_FREE(RT_Nk_ibz)
     YAMBO_FREE(DbGd_EE_scatt_table)
     YAMBO_FREE(E_xc)
     YAMBO_FREE(RT_TIME_status)
     YAMBO_FREE(E_PHOT_Hole_lifetime)
     YAMBO_FREE(E_PHOT_Electron_lifetime)
     YAMBO_FREE(E_E_Hole_lifetime)
     YAMBO_FREE(E_E_Electron_lifetime)
     YAMBO_FREE(E_P_Hole_lifetime)
     YAMBO_FREE(E_P_Electron_lifetime)
     YAMBO_FREE(E_P_El_abs_lifetime)
     YAMBO_FREE(E_P_Ho_abs_lifetime)
     YAMBO_FREE(REF_lifetime)
     YAMBO_FREE(SAVED_lifetime)
     YAMBO_FREE(SAVED_times)
     YAMBO_FREE(REF_V_xc_sc)
     !
     if(l_all.or.l_carriers) then
       !
       YAMBO_FREE(RT_E_occupations)
       YAMBO_FREE(RT_H_occupations)
       YAMBO_FREE(RT_delta_occupations)
       YAMBO_FREE(RT_life_delta_occupations)
       YAMBO_FREE(RT_E_occupations_reference)
       YAMBO_FREE(RT_H_occupations_reference)
       YAMBO_FREE(RT_occupations_locked)
       !
     endif
     !
     YAMBO_FREE(REF_V_hartree_sc)
     YAMBO_FREE(RT_E_and_k%E)
     YAMBO_FREE(RT_E_and_k%W)
     YAMBO_FREE(RT_E_and_k%k)
     !
     if(l_all .or. l_G_lesser) then
       !
       YAMBO_FREE(Ho_plus_Sigma)
       YAMBO_FREE(H_pseudo_eq)
       YAMBO_FREE(RT_Vnl_xc)
       !
       YAMBO_FREE(HARTREE0)
       YAMBO_FREE(XC0)
       !
       YAMBO_FREE(G_lesser)
       YAMBO_FREE(G_lesser_reference)
       YAMBO_FREE(dG_lesser)
       YAMBO_FREE(MEM_pointer)
       !
     endif
     !
     YAMBO_FREE(q_weight_RT)
     YAMBO_FREE(THETA_matrix)
     YAMBO_FREE(THETA_matrix_index)
     YAMBO_FREE(elph_gkkp_expanded_sq)
     YAMBO_FREE(I1_matrix)
     YAMBO_FREE(rho_reference)
     YAMBO_FREE(magn_reference)
     YAMBO_FREE(RT_Deph_Matrix)
     YAMBO_FREE(Q_io_map)
     YAMBO_FREE(K_io_map)
     !
   end subroutine RT_free
   !
   logical function FIRST_step( )
     FIRST_step=integrator_step==1
   end function
   !
   subroutine ADD_to_saved_values(VAL,ARRAY,STEPS)
     integer  :: STEPS
     real(SP) :: VAL,ARRAY(STEPS)
     integer  :: i1
     do i1=1,STEPS-1
       ARRAY(i1)=ARRAY(i1+1)
     enddo
     ARRAY(STEPS)=VAL
   end subroutine
   !
   subroutine RT_init_occupation_reference( )
     use electrons,     ONLY:spin_occ
     integer               :: ib
     do ib=RT_bands(1),RT_bands(2)
       RT_E_occupations_reference(ib,:)=         aimag(G_lesser_reference(ib,ib,:))
       RT_H_occupations_reference(ib,:)=spin_occ-aimag(G_lesser_reference(ib,ib,:))
     enddo
   end subroutine
   !
end module
