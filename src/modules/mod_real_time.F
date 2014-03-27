! 
! Copyright (C) 2000-2013 A. Marini, M. Gruening and the YAMBO team 
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
module real_time
 !
 use SC,         ONLY:G_lesser,SC_R
 use pars,       ONLY:DP,SP,schlen,lchlen
 use units,      ONLY:FS2AUT
 use global_XC,  ONLY:SE_NONE
 !
 implicit none
 !
 ! CORE LOGICALS
 !
 ! NE dynamics
 !
 logical           :: l_NE_dynamics  
 logical           :: l_NE_dynamics_started
 logical           :: l_NE_with_fields
 logical           :: l_RT_probe
 logical           :: l_RT_pump_and_probe
 logical           :: l_Dynamics_is_over  
 !
 ! Inital XC (SC or default) are requested
 !
 logical           :: l_RT_needs_XC0
 !
 ! To exclude the Diamagnetic term 
 !
 logical           :: l_RT_diamagnetic
 !
 ! To include the induced field in the coupling and current
 !
 logical           :: l_RT_induced_field
 ! 
 ! Diagonalization (QP analysis)
 !
 logical           :: l_RT_diagonalize
 integer           :: RT_diagonalization_steps
 !
 ! RPA and independent particles
 !
 logical           :: l_NE_simulate_RPA
 logical           :: l_NE_simulate_IP
 logical           :: l_NE_simulate_Irredux
 logical           :: l_NE_simulate_BSE
 logical           :: l_NE_simulate_TDDFT
 !
 ! Frozen parts of self-energy
 !
 logical           :: Hfrozen
 logical           :: XCfrozen 
 !
 ! Time steps and step length...
 !------------------------------
 ! 
 !   Thermal_steps   NE_steps
 ! --------------- 0 ---------------------------- (end)
 !                 ^   ^
 !                 |   |
 !                 Thermal_steps+1
 !                     |__ ef1/2_i_time
 ! 
 ! NE_time is negative during the Thermal steps. When NE_time is positive
 ! l_NE_dynamics_started=.TRUE.
 !
 ! ... core step length ...
 !
 real(SP) :: RT_step
 ! 
 ! ... thermalization ...
 !
 integer  :: Thermal_steps
 ! 
 ! ... Switch On Time (adiabatic switch on) ...
 !
 real(SP) :: SwitchOnTime
 !
 ! ... NE dynamics ...
 !
 real(SP) :: NE_time ! <- incremented
 real(SP) :: NE_tot_time ! Total (fixed)
 integer  :: NE_i_time ! = (NE_time-1)/RT_step
 integer  :: RT_dynamics_jumps 
 integer  :: NE_steps
 !
 ! Here I control the way the time evolution is sampled for the e-p and e-e
 ! channels. I will see later if this can be somehow extended/duplicated
 ! for the static correlations
 !
 character(schlen) :: RT_dynamics_KIND = "EXP" ! EXP or FIELD
 real(SP) :: RT_dynamics_steps(2)  =-1.*FS2AUT ! (1) is step between groups of 3, (2) is step in the group of 3.
 real(SP) :: RT_dynamics_decay_time=-1.*FS2AUT ! For pulsed-like distribution it dumps the distribution of points
                                               ! in the long-time regime
 integer(2), allocatable :: RT_TIME_status(:)
 !
 ! Kind of e-e interactions (for the CORR and SCATT parts)
 !
 integer           :: RT_EE_CORR_kind=SE_NONE
 integer           :: RT_EE_SCATT_kind=SE_NONE
 !
 ! ... Memory ...
 !
 real(SP) :: NE_MEM_treshold
 integer  :: G_MEM_steps
 integer  :: Life_MEM_steps
 integer  :: I_MEM_steps    = 0
 !
 ! ... Window on the k-points/bands allowed to follow the dynamics ...
 !
 real(SP)             :: RT_eh_energy_max
 !
 ! Macroscopic induced current ... 
 !--------------------------------
 !
 complex(SP) :: RT_ind_J(3)
 !
 ! ... and polarization ...
 !
 ! the calculated one ...
 !
 complex(SP) :: RT_P(3)
 !
 ! Diagonal (QP) and off-diagonal (Phase) relaxation time
 !
 real(SP)    :: Phase_LifeTime
 real(SP)    :: RAD_LifeTime
 logical     :: RT_is_dephased
 !
 ! Relaxation and Dissipation 
 !----------------------------
 logical                  :: l_RT_uses_E_RIM
 integer,     allocatable :: RIM_ibz_coo(:,:)
 integer,     allocatable :: RIM_bz_coo(:,:)  
 integer,     allocatable :: RIM_closest_E(:,:)  
 !
 ! ELPH kernel...
 !----------------
 !
 logical     :: l_RT_CCA_Kernel
 logical     :: l_Debye_TBose_update
 real(SP)    :: Eh_pumped_Nel
 real(SP)    :: Eh_pumped_pair_energy(2)
 real(SP)    :: Eh_pumped_pair_width
 !
 ! ... Q weights calculated using RIM in order to evaluate the ELPH kernel
 !
 real(SP),    allocatable :: Q_weight(:)
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
 logical                  :: l_RT_skip_impose = .FALSE.
 logical                  :: l_use_delta_occupations
 real(SP),    allocatable :: RT_E_occupations(:,:) 
 real(SP),    allocatable :: RT_H_occupations(:,:) 
 real(SP),    allocatable :: RT_delta_occupations(:,:) 
 real(SP),    allocatable :: RT_occupations_reference(:,:) 
 logical,     allocatable :: RT_occupations_locked(:,:) 
 logical,     allocatable :: RT_lifetimes_locked(:,:) 
 !
 ! EL-PH RD life-times
 !-----------------------------------------
 real(SP),    allocatable :: E_P_Hole_lifetime(:,:)
 real(SP),    allocatable :: E_P_Electron_lifetime(:,:)
 real(SP),    allocatable :: E_P_El_abs_lifetime(:,:)
 real(SP),    allocatable :: E_P_Ho_abs_lifetime(:,:)
 !
 ! Coulomb RD life-times
 !-----------------------
 real(SP),    allocatable :: E_E_Hole_lifetime(:,:)
 real(SP),    allocatable :: E_E_Electron_lifetime(:,:)
 !
 ! Saved RD life-times
 !-----------------------
 real(SP),    allocatable :: SAVED_times(:)
 real(SP),    allocatable :: REF_e_lifetime(:,:)
 real(SP),    allocatable :: REF_h_lifetime(:,:)
 real(SP),    allocatable :: SAVED_lifetime(:,:,:,:)
 !
 ! Ouput Lifetimes, ratio fit and temperatures
 !---------------------------------------------
 logical                  :: l_lftm_fit_stable(2)
 integer                  :: Nfitted_lifetimes(2)
 real(SP)                 :: RT_mean_lifetimes(6)
 real(SP)                 :: RT_Tfit_lifetimes(4)
 real(SP)                 :: RT_Efit_lifetimes(4)
 real(SP)                 :: Imp_factors(5)
 !
 ! Occupations fit
 !-----------------
 real(SP)                 :: RT_Tfit_occupations(2)
 real(SP)                 :: RT_Efit_occupations(2)
 !
 ! Relaxation: Dephasing Matrix
 !------------------------------
 !
 real(SP)    :: RT_ave_dephasing
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
 complex(SP), allocatable :: G_lesser_reference(:,:,:)
 !
 ! COLLISIONS
 !------------
 complex(SP), allocatable :: Ho_plus_Sigma(:,:,:,:)
 complex(SP), allocatable :: I_coh_SAVE(:,:,:,:)
 complex(SP), allocatable :: Sigma_relax(:,:,:)
 !
 ! Density
 !---------
 !
 real(SP),    allocatable :: rho_reference(:)    ! reference density from SC_bands(1):SC_bands(2)
 real(SP),    allocatable :: full_rho(:)         ! density of all bands from 1:SC_bands(2)
 !
 ! Integrators
 !
 logical  :: l_RT_EULER
 logical  :: l_RT_RK2
 logical  :: l_RT_EXP
 logical  :: l_RT_INTERPOLATE
 logical  :: l_RT_RWA
 !
 ! Integrate exactly the part of the dynamics due to H_o 
 ! as it is done in Comp. Phys. Comm. 123(1999)
 ! in this case I choosed H_o = H_lda ( rho_0 )
 !
 character(schlen)            :: Integrator_name
 !
 ! Hartree term
 !
 complex(SP), allocatable :: XC0_and_HARTREE0(:,:,:,:)
 !
 ! Occupation natural orbitals
 !
 real(SP),    allocatable :: Occ_natural_orb(:,:) 
 !
 ! Databases input/output
 !
 integer,  parameter      :: J_and_P_cache_size=18
 !
 integer                  :: ID(7)
 integer                  :: it_start           = 1   ! restart point
 integer                  :: last_it_evaluated  = 0   ! When RT_dynamics_jumps are non zero this is 
                                                      ! the last evaluated point
 integer                  :: RT_IO_JPO_steps    = -1
 integer                  :: RT_IO_output_steps = -1
 integer                  :: RT_IO_G_steps      = -1
 integer                  :: RT_IO_steps_DB     = -1
 real(SP)                 :: RT_IO_time(3)      = -1.*FS2AUT ! Three values for J,P,OCC - OUPUT - GF's
 real(SP), allocatable    :: J_and_P_cache(:,:)
 !
 ! Interface
 !
 interface
   !
   subroutine RT_output(what,VALUEs,TIME,E)
     use pars,       ONLY:SP
     character(*),       intent(in) :: what
     real(SP), optional, intent(in) :: VALUEs(:) 
     real(SP), optional, intent(in) :: TIME
     real(SP), optional, intent(in) :: E
   end subroutine
   !
   subroutine RT_average_operator(VALUE,O_REAL,O_QP,O_MATRIX,G,k,en,TRANS,nK_mem)
     use pars,       ONLY:SP
     use SC,         ONLY:SC_bands
     use R_lattice,  ONLY:bz_samp 
     use QP_m,       ONLY:QP_nk,QP_table,QP_n_states
     use electrons,  ONLY:n_sp_pol,levels
     use FFT_m,      ONLY:fft_size
     type(bz_samp), intent(in) :: k
     complex(SP),   intent(in) :: G(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
     type(levels),  intent(in) :: en
     integer,       intent(in) :: nK_mem
     complex(SP),  intent(out) :: VALUE
     complex(SP),     optional, intent(in) :: O_MATRIX(SC_bands(2),SC_bands(2),nK_mem,n_sp_pol)
     complex(SP),     optional, intent(in) :: O_QP(QP_n_states)
     real(SP),        optional, intent(in) :: O_REAL(fft_size,n_sp_pol)
     logical,         optional, intent(in) :: TRANS
   end subroutine RT_average_operator
   !
   subroutine RT_update(G_input,SC_R,it,E,k,X,up_Gref,up_DIP,an_QP)
     use pars,       ONLY:SP
     use SC,         ONLY:SC_bands
     use R_lattice,  ONLY:bz_samp
     use QP_m,       ONLY:QP_nk
     use electrons,  ONLY:levels,n_sp_pol
     use X_m,        ONLY:X_t
     type(X_t),      intent(inout)        :: X
     complex(SP),    intent(inout)        :: G_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
     complex(SP),    intent(in)           :: SC_R(SC_bands(2),SC_bands(2),QP_nk,n_sp_pol)
     type(bz_samp),  intent(in)           :: k
     type(levels),   intent(in)           :: E
     integer,        intent(in)           :: it
     logical,        optional, intent(in) :: up_Gref,up_DIP,an_QP
   end subroutine RT_update
   !
   subroutine OCCUPATIONS_IO_and_interpolate(E,what,k,Time,imposed_COM)
     use pars,       ONLY:SP
     use electrons,  ONLY:levels
     use R_lattice,  ONLY:bz_samp
     type(levels),       intent(in)    :: E
     character(1)                      :: what ! G,X,K
     type(bz_samp),optional,intent(in) :: k
     real(SP), optional, intent(inout) :: Time
     integer , optional, intent(in)    :: imposed_COM
   end subroutine OCCUPATIONS_IO_and_interpolate
   !
   subroutine RT_Integrator(G_new,G_old,Avec,Avec_old,T_step,Xen,k,q,Integrator,Time)
   end subroutine RT_Integrator
   !
 end interface
 !
 contains
   !
   subroutine RT_alloc(en)
     !
     use pars,           ONLY:SP,LP,schlen,cZERO
     use drivers,        ONLY:l_sc_hf,l_HF_and_locXC,l_sc_contains_lda,l_elph_scatt,l_elel_scatt,&
&                             l_sc_cohsex         
     use memory_m,       ONLY:mem_est
     use SC,             ONLY:SC_bands,V_hartree_sc,local_V,SC_alloc,E_bare,rho_n, &
&                             SC_xc_functional,SC_xc_functional,RT_nk
     use FFT_m,          ONLY:fft_size
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use xc_functionals, ONLY:E_xc
     use QP_m,           ONLY:QP_nk,QP_n_states,QP_Vnl_xc
     use R_lattice,      ONLY:nqbz,nkibz
     !
     type(levels), intent(in) :: en
     !
     call SC_alloc(en)
     !
     if (l_HF_and_locXC.or.l_sc_hf.or.l_sc_cohsex) then
       allocate(QP_Vnl_xc(QP_n_states))
       call mem_est("QP_Vnl_xc",(/QP_n_states/))
       QP_Vnl_xc=cZERO
     endif
     !
     if(l_NE_dynamics) then
       allocate(J_and_P_cache(J_and_P_cache_size,RT_IO_JPO_steps))
       call mem_est('Current_Cache',(/size(J_and_P_cache)/),(/SP/))
     endif
     !
     allocate(E_xc(fft_size))
     call mem_est('Exc',(/fft_size/),(/SP/))
     !
     allocate(G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk,G_MEM_steps))
     call mem_est("G_lesser",(/size(G_lesser)/),(/2*SP/))
     !
     allocate(RT_H_occupations(SC_bands(1):SC_bands(2),RT_nk))
     call mem_est("RT_H_occupations",(/size(RT_H_occupations)/),(/SP/))
     !
     allocate(RT_delta_occupations(SC_bands(1):SC_bands(2),RT_nk))
     call mem_est("RT_delta_occupations",(/size(RT_delta_occupations)/),(/SP/))
     !
     allocate(RT_occupations_reference(SC_bands(1):SC_bands(2),RT_nk))
     call mem_est("RT_occupations_reference",(/size(RT_occupations_reference)/),(/SP/))
     RT_occupations_reference=0._SP
     !
     allocate(RT_occupations_locked(SC_bands(1):SC_bands(2),RT_nk))
     call mem_est("RT_occupations_locked",(/size(RT_occupations_locked)/),(/LP/))
     RT_occupations_locked=.false.
     !
     allocate(RT_lifetimes_locked(SC_bands(1):SC_bands(2),RT_nk))
     call mem_est("RT_lifetimes_locked",(/size(RT_lifetimes_locked)/),(/LP/))
     RT_lifetimes_locked=.false.
     !
     allocate(RT_E_occupations(SC_bands(1):SC_bands(2),RT_nk))
     call mem_est("RT_E_occupations",(/size(RT_E_occupations)/),(/SP/))
     !
     if (l_elel_scatt) then
       allocate(E_E_Hole_lifetime(SC_bands(1):SC_bands(2),RT_nk))
       allocate(E_E_Electron_lifetime(SC_bands(1):SC_bands(2),RT_nk))
       call mem_est("RT_E_E_lifetimes",(/2*size(E_E_Hole_lifetime)/),(/SP/))
       E_E_Hole_lifetime=0._SP
       E_E_Electron_lifetime=0._SP
     endif
     !
     if (l_RT_CCA_Kernel.and.l_elph_scatt) then
       allocate(E_P_Hole_lifetime(SC_bands(1):SC_bands(2),RT_nk))
       allocate(E_P_Electron_lifetime(SC_bands(1):SC_bands(2),RT_nk))
       allocate(E_P_El_abs_lifetime(SC_bands(1):SC_bands(2),RT_nk))
       allocate(E_P_Ho_abs_lifetime(SC_bands(1):SC_bands(2),RT_nk))
       call mem_est("RT_E_P_lifetimes",(/4*size(E_P_Hole_lifetime)/),(/SP/))
       E_P_Hole_lifetime=0._SP
       E_P_Electron_lifetime=0._SP
       E_P_El_abs_lifetime=0._SP
       E_P_Ho_abs_lifetime=0._SP
     endif
     !
     if (l_elel_scatt.or.l_RT_CCA_Kernel.or.l_elph_scatt) then
       allocate(REF_e_lifetime(SC_bands(1):SC_bands(2),RT_nk))
       allocate(REF_h_lifetime(SC_bands(1):SC_bands(2),RT_nk))
       call mem_est("REF_lifetime",(/2*size(REF_e_lifetime)/),(/SP/))
       allocate(SAVED_lifetime(SC_bands(1):SC_bands(2),RT_nk,6,Life_MEM_steps)) ! e-e h, e-e e, e-p h, e-p e, e-p h(abs), e-p e(abs)
       call mem_est("SAVED_lifetime",(/size(SAVED_lifetime)/),(/SP/))
       allocate(SAVED_times(Life_MEM_steps)) 
     endif
     !
     allocate(Ho_plus_Sigma(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk,n_sp_pol))
     call mem_est("Ho_plus_Sigma",(/size(Ho_plus_Sigma)/),(/2*SP/))
     !
     if (l_RT_INTERPOLATE) then
       allocate(I_coh_SAVE(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk,I_MEM_steps))
       call mem_est("I_coh_SAVE",(/size(I_coh_SAVE)/),(/2*SP/))
     endif
     !
     if(RT_is_dephased.or.l_elph_scatt.or.l_elel_scatt) then
       allocate(Sigma_relax(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk))
       call mem_est("Sigma_relax",(/size(Sigma_relax)/),(/2*SP/))
     endif
     !
     if(l_RT_needs_XC0) then
       allocate(XC0_and_HARTREE0(1:SC_bands(2),1:SC_bands(2),QP_nk,n_sp_pol))
       call mem_est("XC0_and_HARTREE0",(/size(XC0_and_HARTREE0)/),(/2*SP/))
       XC0_and_HARTREE0=cZERO
     endif
     !
     allocate(SC_R(1:SC_bands(2),1:SC_bands(2),QP_nk,n_sp_pol))
     call mem_est("SC_R",(/size(SC_R)/),(/2*SP/))
     !
     allocate(G_lesser_reference(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk))
     call mem_est("Gf_l_ref",(/size(G_lesser_reference)/),(/2*SP/))
     !
     allocate(rho_reference(fft_size))
     call mem_est("rho_reference",(/fft_size/),(/SP/))
     !
     if(l_sc_contains_lda.and.SC_xc_functional/=0.and.SC_bands(1)>1) then
       allocate(full_rho(fft_size))
       call mem_est("full_rho",(/fft_size/),(/SP/))
     endif
     !
     ! Reference energies
     !
     allocate(E_bare(1:SC_bands(2),QP_nk,n_sp_pol))
     call mem_est("E_bare",(/size(E_bare)/))
     E_bare=en%E(:SC_bands(2),:,:)
     !
     if(l_RT_diagonalize) then
       allocate(Occ_natural_orb(1:SC_bands(2),QP_nk))
       call mem_est("Occ_natural",(/size(Occ_natural_orb)/))
     endif
     !
   end subroutine RT_alloc
   
   subroutine RT_free
     !
     use drivers,        ONLY:l_sc_hf,l_HF_and_locXC
     use SC,             ONLY:V_hartree_sc,Ho,SC_free,E_bare,rho_n,RT_all2ibz,&
&                             RT_ibz_coo,RT_Nk_ibz
     use memory_m,       ONLY:mem_est
     use QP_m,           ONLY:QP_Vnl_xc
     use xc_functionals, ONLY:E_xc
     !
     call SC_free()
     !
     if (l_HF_and_locXC) then
       !
       deallocate(QP_Vnl_xc)
       call mem_est("QP_Vnl_xc")
       !
     endif
     !
     deallocate(RIM_ibz_coo,RIM_bz_coo,RT_ibz_coo,RT_all2ibz,RT_Nk_ibz)
     !
     deallocate(E_xc)
     call mem_est('Exc')
     !
     deallocate(Ho_plus_Sigma)
     call mem_est("Ho_plus_Sigma")
     !
     if (l_RT_INTERPOLATE) then
       deallocate(I_coh_SAVE)
       call mem_est("I_coh_SAVE")
     endif
     !
     if(allocated(RT_TIME_status)) then
       deallocate(RT_TIME_status)
       call mem_est("RT_TIME_status")
     endif
     !
     if(allocated(Sigma_relax)) then
       deallocate(Sigma_relax)
       call mem_est("Sigma_relax")
     endif
     !  
     if(allocated(XC0_and_HARTREE0)) then
       deallocate(XC0_and_HARTREE0)
       call mem_est("XC0_and_HARTREE0")
     endif
     !
     if(allocated(SC_R)) then
       deallocate(SC_R)
       call mem_est("SC_R")
     endif
     !
     if(allocated(E_E_Hole_lifetime)) then
       deallocate(E_E_Hole_lifetime,E_E_Electron_lifetime)
       call mem_est("RT_E_E_lifetimes")
     endif
     !
     if(allocated(E_P_Hole_lifetime)) then
       deallocate(E_P_Hole_lifetime,E_P_Electron_lifetime,E_P_El_abs_lifetime,E_P_Ho_abs_lifetime)
       call mem_est("RT_E_P_lifetimes")
     endif
     !
     if(allocated(REF_e_lifetime)) then
       deallocate(REF_e_lifetime,REF_h_lifetime)
       call mem_est("REF_lifetimes")
     endif
     !
     if(allocated(SAVED_lifetime)) then
       deallocate(SAVED_lifetime,SAVED_times)
       call mem_est("SAVED_lifetimes")
     endif
     !
     deallocate(RT_H_occupations)
     call mem_est("RT_H_occupations")
     !
     deallocate(RT_delta_occupations)
     call mem_est("RT_delta_occupations")
     !
     deallocate(RT_occupations_reference)
     call mem_est("RT_occupations_reference")
     !
     deallocate(RT_occupations_locked)
     call mem_est("RT_occupations_locked")
     !
     deallocate(RT_lifetimes_locked)
     call mem_est("RT_lifetimes_locked")
     !
     deallocate(RT_E_occupations)
     call mem_est("RT_E_occupations")
     !
     deallocate(G_lesser)
     call mem_est("G_lesser")
     !
     if (allocated(q_weight)) deallocate(q_weight)
     !
     if (allocated(THETA_matrix)) then
       deallocate(THETA_matrix)
       call mem_est("THETA_matrix")
       deallocate(THETA_matrix_index)
     endif
     !
     if (allocated(elph_gkkp_expanded)) then
       deallocate(elph_gkkp_expanded)
       call mem_est("GKKP_expanded")
     endif
     !
     deallocate(G_lesser_reference)
     call mem_est("Gf_l_ref")
     !
     if(allocated(rho_reference)) then
       deallocate(rho_reference)
       call mem_est("rho_reference")
     endif
     !
     if(allocated(full_rho)) then
       deallocate(full_rho)
       call mem_est("full_rho")
     endif
     !
     deallocate(E_bare)
     call mem_est("E_bare")
     !
     if(l_RT_diagonalize) then
       deallocate(Occ_natural_orb)
       call mem_est("Occ_natural")
     endif
     !
     if(l_NE_dynamics) then
       deallocate(J_and_P_cache)
       call mem_est('Current_Cache')
     endif
     !
     if(allocated(RT_Deph_Matrix)) then
       deallocate(RT_Deph_Matrix)
       call mem_est("DephMatrix")
     endif
     !
     if(allocated(Q_io_map)) then
       deallocate(Q_io_map,K_io_map)
     endif
     !
   end subroutine RT_free
   !
   logical function RT_do_IO(what)
     character(*) :: what
     if (what=="J".or.what=="P".or.what=="OCCs") RT_do_IO=mod(NE_i_time-1,RT_IO_JPO_steps)==0
     if (what=="OUT") RT_do_IO=mod(NE_i_time-1,RT_IO_output_steps)==0
     if (what=="G")   RT_do_IO=mod(NE_i_time-1,RT_IO_G_steps)==0
   end function
   !
   logical function FIRST_step(it,TIME)
     integer   :: it
     real(SP)  :: TIME
     FIRST_step=abs(TIME-(it-1)*RT_step)<1.E-6
   end function
   !
   integer function B_mat_index(ib1,ib2)
     use SC, ONLY:SC_bands
     integer :: ib1,ib2
     !
     B_mat_index=(ib1-SC_bands(1))*(SC_bands(2)-SC_bands(1)+1)+ib2-SC_bands(1)+1
     !
   end function
   !
   integer function MEM_index(it,Nt)
     integer :: it,Nt
     MEM_index=mod(it,Nt)
     if (MEM_index==0) MEM_index=Nt
     if (it        <0) MEM_index=1
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
end module
