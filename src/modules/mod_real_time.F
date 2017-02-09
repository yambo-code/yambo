! 
!        Copyright (C) 2000-2017 the YAMBO team
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
 use pars,       ONLY:DP,SP,schlen,lchlen
 use units,      ONLY:FS2AUT
 use global_XC,  ONLY:QP_SE_NONE
 use electrons,  ONLY:levels
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
 logical          :: eval_HARTREE
 logical          :: eval_DFT
 !
 ! To include the induced field in the coupling and current
 !
 logical           :: l_RT_induced_field = .FALSE.
 !
 ! ... core step length and Time-points array...
 !
 real(SP)              :: RT_step             ! Elemental step used to define dimensions and time-points
 real(SP)              :: RT_dyn_step = 0._SP ! On-the-fly time-step
 real(SP)              :: RT_MAX_step = 0._SP ! MAX value of the dynamical RT step value
 ! 
 ! ... NE dynamics ...
 !
 real(SP) :: NE_time    =0._SP  ! <- incremented
 real(SP) :: NE_tot_time=0._SP  ! Total (fixed)
 integer  :: NE_time_step_update_last_point = 1     ! Last time-point of the dT update
 real(SP) :: NE_initial_time_step_update    = 0._SP ! Time above which the time-step is dynamically updated (0. means no update)
 real(SP) :: NE_time_step_update_jump       = 0._SP ! Time BETWEEN two time-steps dynamically updated (0. means RT_step)
 real(SP) :: NE_step_update_treshold        = 2._SP ! Time-step update treshold
 real(SP) :: NE_time_step_update_jump_SAVE  = 0._SP
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
 ! Lifetimes Extrapolation Time status 
 ! 
 integer(2), allocatable :: RT_TIME_status(:)
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
 integer  :: G_MEM_steps     = 2 ! By default only G_lesser@t and @t+1
 integer  :: i_MEM_collision = 1
 integer  :: i_MEM_now       = 1
 integer  :: i_MEM_next      = 0 ! Defined in RT_driver 
 integer  :: i_MEM_previous  = 0 ! Defined in RT_driver
 integer  :: MAX_dG_dT_indexes(3)
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
 complex(SP) :: RT_magn(3)
 !
 ! Diagonal (QP) and off-diagonal (Phase) relaxation time
 !
 real(SP)    :: Phase_LifeTime
 real(SP)    :: RAD_LifeTime
 real(SP)    :: RAD_magnification=1._SP
 logical     :: RT_is_statically_dephased=.FALSE.
 logical     :: RT_is_dynamically_dephased=.FALSE.
 !
 ! Table pointing to the position of the uniform grid points in the
 ! G_lesser structure
 !
 integer,     allocatable :: RT_ibz_coo(:,:)
 integer,     allocatable :: RT_all2ibz(:)  
 integer,     allocatable :: RT_Nk_ibz(:)  
 !
 ! K-points: In case I want to use the G_lesser to define the occupations
 ! of a post-run I need to keep here the number of k-points 
 !
 integer                 :: RT_nk
 integer                 :: RT_bands(2)
 integer                 :: RT_nbands
 !
 ! BGR
 !----------------------------
 logical                  :: l_BGR=.false.
 real(SP),    allocatable :: BGR_hartree(:,:,:)  
 real(SP),    allocatable :: BGR_V_xc(:,:,:)  
 real(SP),    allocatable :: BGR_Self_Energy(:,:,:)  
 real(SP),    allocatable :: REF_V_xc_sc(:,:)  
 real(SP),    allocatable :: REF_V_hartree_sc(:,:)  
 real(SP),    allocatable :: REF_Vnl_xc(:)  
 real(SP)                 :: BGR(5) ! (BGR at the minimum dir/ind gap, H contrib., xc contrib., SE contrib.)
 type(levels),save        :: E_reference
 !
 ! Relaxation and Dissipation 
 !----------------------------
 logical                  :: l_RT_uses_E_DbGd
 logical,     allocatable :: DbGd_EE_scatt_table(:,:)  
 integer,     allocatable :: DbGd_EE_Nk_table(:,:)  
 integer,     allocatable :: DbGd_ibz_coo(:,:)
 integer,     allocatable :: DbGd_bz_coo(:,:)  
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
 complex(SP), allocatable :: elph_gkkp_expanded(:,:,:,:)
 ! ... indexes ...
 integer,     allocatable :: THETA_matrix_index(:,:,:)
 integer,     allocatable :: Q_io_map(:)
 integer,     allocatable :: K_io_map(:)
 !
 ! Locks and reference
 !----------------------
 logical                  :: l_RT_impose_N_dN    = .TRUE.
 logical                  :: l_RT_impose_E    = .TRUE.
 !
 ! Locks and reference
 !----------------------
 logical                  :: l_use_updated_reference
 logical                  :: l_initial_equilibrium
 real(SP),    allocatable :: RT_E_occupations(:,:) 
 real(SP),    allocatable :: RT_H_occupations(:,:) 
 real(SP),    allocatable :: RT_delta0_occupations(:,:) 
 real(SP),    allocatable :: RT_delta_occupations(:,:) 
 real(SP),    allocatable :: RT_E_occupations_reference(:,:) 
 real(SP),    allocatable :: RT_H_occupations_reference(:,:)
 logical,     allocatable :: RT_occupations_locked(:,:) 
 logical,     allocatable :: RT_lifetimes_locked(:,:) 
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
 ! Occupations fit
 !-----------------
 real(SP)                 :: RT_Tfit_occupations(2)
 real(SP)                 :: RT_Efit_occupations(2)
 !
 ! Thermodynamics
 !-----------------
 real(SP)                 :: RT_T_thermodynamics(3)
 real(SP)                 :: RT_E_thermodynamics(3)
 real(SP)                 :: RT_S_thermodynamics(3)
 !
 ! Relaxation: Dephasing Matrix
 !------------------------------
 !
 real(SP)                 :: RT_ave_dephasing
 real(SP),    allocatable :: RT_Deph_Matrix(:,:,:)
 !
 ! GFs (retarded)
 !----------------
 !
 integer           :: two_alpha
 integer           :: N_Gr_descs
 real(SP)          :: Gr_QP_width
 character(schlen) :: Gr_kind = "QP"
 character(lchlen) :: Gr_description(5) 
 !
 ! GFs
 !-----
 !
 complex(SP), allocatable :: G_lesser(:,:,:,:)          ! Band, Band, K, Time
 complex(SP), allocatable :: G_lesser_reference(:,:,:)
 complex(SP), allocatable :: dG_lesser(:,:,:,:)
 !
 ! COLLISIONS
 !------------
 complex(SP), allocatable :: Ho_plus_Sigma(:,:,:,:)
 complex(SP), allocatable :: RT_Vnl_xc(:,:,:,:)
 complex(SP), allocatable :: I_relax_static(:,:,:)
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
 logical  :: l_RT_RK2
 logical  :: l_RT_HEUN
 logical  :: l_RT_EXP
 logical  :: l_RT_RWA
 !
 integer  :: integrator_step
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
 ! TD-ABS and RT-BSE [yambo]
 !
 character(schlen)       :: RT_ctl_db(4)
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
   real(SP)   ,allocatable:: dE_Hartree(:)   
   real(SP)   ,allocatable:: dE_V_xc(:)        
   real(SP)   ,allocatable:: dE_Self_Energy(:) 
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
     Y_ALLOC(RT%kpt(RT%nk,3))
     Y_MEM_ALLOC(RT%kpt)
     Y_ALLOC(RT%k_weight(RT%nk))
     Y_MEM_ALLOC(RT%k_weight)
     Y_ALLOC(RT%table(RT%nstates,4))
     Y_MEM_ALLOC(RT%table)
     Y_ALLOC(RT%E_bare(RT%nstates))
     Y_MEM_ALLOC(RT%E_bare)
     Y_ALLOC(RT%f_bare(RT%nstates))
     Y_MEM_ALLOC(RT%f_bare)
     Y_ALLOC(RT%dE_Hartree(RT%nstates))
     Y_MEM_ALLOC(RT%dE_Hartree)
     Y_ALLOC(RT%dE_V_xc(RT%nstates))
     Y_MEM_ALLOC(RT%dE_V_xc)
     Y_ALLOC(RT%dE_Self_Energy(RT%nstates))
     Y_MEM_ALLOC(RT%dE_Self_Energy)
     RT%table         =0
     RT%E_bare        =0._SP
     RT%f_bare        =0._SP
     RT%dE_Hartree    =0._SP
     RT%dE_V_xc       =0._SP
     RT%dE_Self_Energy=0._SP
     if (l_elel_scatt) then
       Y_ALLOC(RT%dGAMMA_E_E_Hole(RT%nstates))
       Y_MEM_ALLOC(RT%dGAMMA_E_E_Hole)
       Y_ALLOC(RT%dGAMMA_E_E_Electron(RT%nstates))
       Y_MEM_ALLOC(RT%dGAMMA_E_E_Electron)
       RT%dGAMMA_E_E_Hole    =0._SP
       RT%dGAMMA_E_E_Electron=0._SP
     endif
     if (l_elph_scatt) then
       Y_ALLOC(RT%dGAMMA_E_P_Hole(RT%nstates))
       Y_MEM_ALLOC(RT%dGAMMA_E_P_Hole)
       Y_ALLOC(RT%dGAMMA_E_P_Electron(RT%nstates))
       Y_MEM_ALLOC(RT%dGAMMA_E_P_Electron)
       Y_ALLOC(RT%dGAMMA_E_P_Ho_abs(RT%nstates))
       Y_MEM_ALLOC(RT%dGAMMA_E_P_Ho_abs)
       Y_ALLOC(RT%dGAMMA_E_P_El_abs(RT%nstates))
       Y_MEM_ALLOC(RT%dGAMMA_E_P_El_abs)
       RT%dGAMMA_E_P_Hole    =0._SP
       RT%dGAMMA_E_P_Electron=0._SP
       RT%dGAMMA_E_P_Ho_abs  =0._SP
       RT%dGAMMA_E_P_El_abs  =0._SP
     endif
     if (l_elphoton_scatt) then
       Y_ALLOC(RT%dGAMMA_E_PHOT_Hole(RT%nstates))
       Y_MEM_ALLOC(RT%dGAMMA_E_PHOT_Hole)
       Y_ALLOC(RT%dGAMMA_E_PHOT_Electron(RT%nstates))
       Y_MEM_ALLOC(RT%dGAMMA_E_PHOT_Electron)
       RT%dGAMMA_E_PHOT_Hole    =0._SP
       RT%dGAMMA_E_PHOT_Electron=0._SP
     endif
     if (l_elel_scatt.or.l_elph_scatt.or.l_elphoton_scatt) then
       Y_ALLOC(RT%GAMMA_bare(RT%nstates,8))
       Y_MEM_ALLOC(RT%GAMMA_bare)
       Y_ALLOC(RT%delta_GAMMA(RT%nstates))
       Y_MEM_ALLOC(RT%delta_GAMMA)
       Y_ALLOC(RT%NEQ_Lifetime(RT%nstates))
       Y_MEM_ALLOC(RT%NEQ_Lifetime)
       RT%GAMMA_bare  =0._SP
       RT%delta_GAMMA =0._SP
       RT%NEQ_Lifetime=0._SP
     endif
     Y_ALLOC(RT%delta_E(RT%nstates))
     Y_MEM_ALLOC(RT%delta_E)
     Y_ALLOC(RT%delta_f(RT%nstates))
     Y_MEM_ALLOC(RT%delta_f)
     RT%delta_f=0._SP
     RT%delta_E=0._SP
   end subroutine
   !
   subroutine RT_obj_free(RT)
     type(RT_t)::RT
     RT%nk=0
     RT%nb=0
     RT%nstates=0
     Y_FREE(RT%dE_Hartree)
     Y_FREE(RT%dE_V_xc)
     Y_FREE(RT%dE_Self_Energy)
     Y_FREE(RT%dGAMMA_E_E_Hole)
     Y_FREE(RT%dGAMMA_E_E_Electron)
     Y_FREE(RT%dGAMMA_E_P_Hole)
     Y_FREE(RT%dGAMMA_E_P_Electron)
     Y_FREE(RT%dGAMMA_E_P_Ho_abs)
     Y_FREE(RT%dGAMMA_E_P_El_abs)
     Y_FREE(RT%dGAMMA_E_PHOT_Hole)
     Y_FREE(RT%dGAMMA_E_PHOT_Electron)
     Y_FREE(RT%E_bare)
     Y_FREE(RT%f_bare)
     Y_FREE(RT%GAMMA_bare)
     Y_FREE(RT%delta_E)
     Y_FREE(RT%delta_f)
     Y_FREE(RT%delta_GAMMA)
     Y_FREE(RT%NEQ_Lifetime)
     Y_FREE(RT%kpt)
     Y_FREE(RT%k_weight)
     Y_FREE(RT%table)
   end subroutine
   !
   subroutine RT_alloc(en)
     !
     use pars,           ONLY:SP,LP,schlen,cZERO
     use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_use_Hxc_collisions
     use memory_m,       ONLY:mem_est
     use hamiltonian,    ONLY:H_alloc,H_QP_table_setup,E_bare
     use FFT_m,          ONLY:fft_size
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use xc_functionals, ONLY:E_xc
     use QP_m,           ONLY:QP_nk,QP_n_states,QP_Vnl_xc
     use R_lattice,      ONLY:nqbz,nXkibz
     !
     type(levels), intent(in) :: en
     !
     call H_alloc(en)
     !
     call H_QP_table_setup(en)
     !
     RT_nbands=RT_bands(2)-RT_bands(1)+1
     !
     Y_ALLOC(E_xc(fft_size))
     Y_MEM_ALLOC(E_xc)
     !
     Y_ALLOC(G_lesser(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,G_MEM_steps))
     Y_MEM_ALLOC(G_lesser)
     !
     Y_ALLOC(RT_H_occupations(RT_bands(1):RT_bands(2),RT_nk))
     Y_MEM_ALLOC(RT_H_occupations)
     !
     Y_ALLOC(RT_delta0_occupations(RT_bands(1):RT_bands(2),RT_nk))
     Y_MEM_ALLOC(RT_delta0_occupations)
     !
     Y_ALLOC(RT_delta_occupations(RT_bands(1):RT_bands(2),RT_nk))
     Y_MEM_ALLOC(RT_delta_occupations)
     !
     Y_ALLOC(RT_E_occupations_reference(RT_bands(1):RT_bands(2),RT_nk))
     Y_MEM_ALLOC(RT_E_occupations_reference)
     RT_E_occupations_reference=0._SP
     !
     Y_ALLOC(RT_H_occupations_reference(RT_bands(1):RT_bands(2),RT_nk))
     Y_MEM_ALLOC(RT_H_occupations_reference)
     RT_H_occupations_reference=0._SP
     !
     Y_ALLOC(RT_occupations_locked(RT_bands(1):RT_bands(2),RT_nk))
     Y_MEM_ALLOC(RT_occupations_locked)
     RT_occupations_locked=.false.
     !
     Y_ALLOC(RT_lifetimes_locked(RT_bands(1):RT_bands(2),RT_nk))
     Y_MEM_ALLOC(RT_lifetimes_locked)
     RT_lifetimes_locked=.false.
     !
     Y_ALLOC(RT_E_occupations(RT_bands(1):RT_bands(2),RT_nk))
     Y_MEM_ALLOC(RT_E_occupations)
     !
     if (l_elphoton_scatt) then
       Y_ALLOC(E_PHOT_Hole_lifetime(RT_bands(1):RT_bands(2),RT_nk))
       Y_MEM_ALLOC(E_PHOT_Hole_lifetime)
       Y_ALLOC(E_PHOT_Electron_lifetime(RT_bands(1):RT_bands(2),RT_nk))
       Y_MEM_ALLOC(E_PHOT_Electron_lifetime)
       E_PHOT_Hole_lifetime=0._SP
       E_PHOT_Electron_lifetime=0._SP
     endif
     !
     if (l_elel_scatt) then
       Y_ALLOC(E_E_Hole_lifetime(RT_bands(1):RT_bands(2),RT_nk))
       Y_MEM_ALLOC(E_E_Hole_lifetime)
       Y_ALLOC(E_E_Electron_lifetime(RT_bands(1):RT_bands(2),RT_nk))
       Y_MEM_ALLOC(E_E_Electron_lifetime)
       E_E_Hole_lifetime=0._SP
       E_E_Electron_lifetime=0._SP
     endif
     !
     if (l_RT_CCA_Kernel.and.l_elph_scatt) then
       Y_ALLOC(E_P_Hole_lifetime(RT_bands(1):RT_bands(2),RT_nk))
       Y_MEM_ALLOC(E_P_Hole_lifetime)
       Y_ALLOC(E_P_Electron_lifetime(RT_bands(1):RT_bands(2),RT_nk))
       Y_MEM_ALLOC(E_P_Electron_lifetime)
       Y_ALLOC(E_P_El_abs_lifetime(RT_bands(1):RT_bands(2),RT_nk))
       Y_MEM_ALLOC(E_P_El_abs_lifetime)
       Y_ALLOC(E_P_Ho_abs_lifetime(RT_bands(1):RT_bands(2),RT_nk))
       Y_MEM_ALLOC(E_P_Ho_abs_lifetime)
       E_P_Hole_lifetime=0._SP
       E_P_Electron_lifetime=0._SP
       E_P_El_abs_lifetime=0._SP
       E_P_Ho_abs_lifetime=0._SP
     endif
     !
     if (l_elel_scatt.or.l_RT_CCA_Kernel.or.l_elph_scatt.or.l_elphoton_scatt) then
       Y_ALLOC(REF_lifetime(RT_bands(1):RT_bands(2),RT_nk,8))
       Y_MEM_ALLOC(REF_lifetime)
       Y_ALLOCATE(SAVED_lifetime(RT_bands(1):RT_bands(2),RT_nk,8,Life_MEM_steps))!e-eh,e-ee,e-ph,e-pe,e-ph(abs),e-pe(abs),e-gh,e-g)
       Y_MEM_ALLOC(SAVED_lifetime)
       Y_ALLOC(SAVED_times(Life_MEM_steps))
       Y_MEM_ALLOC(SAVED_times)
       REF_lifetime=0._SP
       SAVED_lifetime=0._SP
       SAVED_times=0._SP
     endif
     !
     Y_ALLOC(Ho_plus_Sigma(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,n_sp_pol))
     Y_MEM_ALLOC(Ho_plus_Sigma)
     !
     if(l_use_Hxc_collisions) then
       Y_ALLOC(RT_Vnl_xc(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,n_sp_pol))
       Y_MEM_ALLOC(RT_Vnl_xc)
       RT_Vnl_xc=(0._SP,0._SP)
     endif
     !
     if(any ((/l_elel_scatt,l_elph_scatt,l_elphoton_scatt,RT_is_statically_dephased/)) ) then
       Y_ALLOC(I_relax_static(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk))
       Y_MEM_ALLOC(I_relax_static)
     endif
     !
     Y_ALLOC(HARTREE0(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nXkibz,n_sp_pol))
     Y_MEM_ALLOC(HARTREE0)
     HARTREE0=cZERO
     !
     Y_ALLOC(XC0(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nXkibz,n_sp_pol))
     Y_MEM_ALLOC(XC0)
     XC0=cZERO
     !
     Y_ALLOC(SC_R(1:RT_bands(2),1:RT_bands(2),nXkibz,n_sp_pol))
     Y_MEM_ALLOC(SC_R)
     !
     Y_ALLOC(G_lesser_reference(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk))
     Y_MEM_ALLOC(G_lesser_reference)
     !
     Y_ALLOC(dG_lesser(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,G_MEM_steps))
     Y_MEM_ALLOC(dG_lesser)
     !
     Y_ALLOC(MEM_pointer(G_MEM_steps))
     Y_MEM_ALLOC(MEM_pointer)
     MEM_pointer    = 0
     MEM_pointer(1) = 1
     !
     Y_ALLOC(REF_V_xc_sc(fft_size,n_spin))
     Y_MEM_ALLOC(REF_V_xc_sc)
     Y_ALLOC(REF_V_hartree_sc(fft_size,n_spin))
     Y_MEM_ALLOC(REF_V_hartree_sc)
     Y_ALLOC(REF_Vnl_xc(QP_n_states))
     Y_MEM_ALLOC(REF_Vnl_xc)
     !
     Y_ALLOC(BGR_hartree(RT_bands(1):RT_bands(2),nXkibz,n_sp_pol))
     Y_MEM_ALLOC(BGR_hartree)
     Y_ALLOC(BGR_V_xc(RT_bands(1):RT_bands(2),nXkibz,n_sp_pol))
     Y_MEM_ALLOC(BGR_V_xc)
     Y_ALLOC(BGR_Self_Energy(RT_bands(1):RT_bands(2),nXkibz,n_sp_pol))
     Y_MEM_ALLOC(BGR_Self_Energy)
     !
     Y_ALLOC(rho_reference(fft_size))
     Y_MEM_ALLOC(rho_reference)
     !
     if(n_spin>1) then
       Y_ALLOC(magn_reference(fft_size,3))
       Y_MEM_ALLOC(magn_reference)
     endif
     !
     ! Reference energies
     !
     Y_ALLOC(E_bare(1:RT_bands(2),nXkibz,n_sp_pol))
     Y_MEM_ALLOC(E_bare)
     E_bare=en%E(:RT_bands(2),:,:)
     !
   end subroutine RT_alloc
   !   
   subroutine RT_free
     !
     use hamiltonian,    ONLY:H_free,E_bare
     use memory_m,       ONLY:mem_est
     use xc_functionals, ONLY:E_xc
     !
     call H_free()
     !
     Y_FREE(DbGd_ibz_coo)
     Y_FREE(DbGd_bz_coo)
     Y_FREE(RT_ibz_coo)
     Y_FREE(RT_all2ibz)
     Y_FREE(RT_Nk_ibz)
     Y_FREE(DbGd_EE_scatt_table)
     Y_FREE(E_xc)
     Y_FREE(RT_Vnl_xc)
     Y_FREE(Ho_plus_Sigma)
     Y_FREE(RT_TIME_status)
     Y_FREE(I_relax_static)
     Y_FREE(HARTREE0)
     Y_FREE(XC0)
     Y_FREE(SC_R)
     Y_FREE(E_PHOT_Hole_lifetime)
     Y_FREE(E_PHOT_Electron_lifetime)
     Y_FREE(E_E_Hole_lifetime)
     Y_FREE(E_E_Electron_lifetime)
     Y_FREE(E_P_Hole_lifetime)
     Y_FREE(E_P_Electron_lifetime)
     Y_FREE(E_P_El_abs_lifetime)
     Y_FREE(E_P_Ho_abs_lifetime)
     Y_FREE(REF_lifetime)
     Y_FREE(SAVED_lifetime)
     Y_FREE(SAVED_times)
     Y_FREE(REF_Vnl_xc)
     Y_FREE(REF_V_xc_sc)
     Y_FREE(REF_V_hartree_sc)
     Y_FREE(BGR_hartree)
     Y_FREE(BGR_V_xc)
     Y_FREE(BGR_Self_Energy)
     Y_FREE(RT_H_occupations)
     Y_FREE(RT_delta0_occupations)
     Y_FREE(RT_delta_occupations)
     Y_FREE(RT_E_occupations_reference)
     Y_FREE(RT_H_occupations_reference)
     Y_FREE(RT_occupations_locked)
     Y_FREE(RT_lifetimes_locked)
     Y_FREE(RT_E_occupations)
     Y_FREE(G_lesser)
     Y_FREE(MEM_pointer)
     Y_FREE(q_weight_RT)
     Y_FREE(THETA_matrix)
     Y_FREE(THETA_matrix_index)
     Y_FREE(elph_gkkp_expanded)
     Y_FREE(G_lesser_reference)
     Y_FREE(dG_lesser)
     Y_FREE(rho_reference)
     Y_FREE(magn_reference)
     Y_FREE(E_bare)
     Y_FREE(RT_Deph_Matrix)
     Y_FREE(Q_io_map)
     Y_FREE(K_io_map)
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
