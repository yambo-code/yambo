! 
! Copyright (C) 2000-2014 A. Marini, M. Gruening and the YAMBO team 
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
 use pars,       ONLY:DP,SP,schlen
 use parallel_m, ONLY:PP_indexes
 !
 implicit none
 !
 ! CORE LOGICALS
 !
 ! NE dynamics
 !
 logical           :: l_NE_with_fields  
 logical           :: l_RT_probe  
 logical           :: l_RT_pump_and_probe  
 logical           :: l_Dynamics_is_over  
 !
 character(schlen) :: Dynamics_Kind ! (C)arriers/(P)olarization
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
 ! NE_dynamics=.TRUE.
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
 logical  :: NE_dynamics
 real(SP) :: NE_time ! <- incremented
 real(SP) :: NE_tot_time ! Total (fixed)
 integer  :: NE_i_time ! = (NE_time-1)/RT_step
 integer  :: NE_Hxc_jumps
 integer  :: NE_steps
 !
 ! ... Memory ...
 !
 real(SP) :: NE_MEM_treshold
 integer  :: NE_MEM_steps
 !
 ! ... Window on the k-points/bands allowed to follow the dynamics ...
 !
 real(SP)             :: RT_eh_energy_max
 type(PP_indexes),SAVE:: RT_k_px
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
 logical     :: RT_is_dumped
 !
 ! I/O
 !----------------
 !
 logical     :: l_RT_skip_GF_IO
 logical     :: l_RT_skip_OCC_IO
 logical     :: l_RT_skip_JandP_IO
 !
 ! ELPH kernel...
 !----------------
 !
 logical     :: l_ELPH_CCA_Kernel
 real(SP)    :: Eh_pumped_pair_energy(2)
 real(SP)    :: Eh_pumped_pair_width
 !
 ! ... Q weights calculated using RIM in order to evaluate the ELPH kernel
 !
 real(SP),    allocatable :: Q_weight(:)
 ! ... THETA matrix
 complex(SP), allocatable :: THETA_matrix(:,:,:,:)
 ! ... Q-parallelization  and GKKP
 type(PP_indexes),   SAVE :: RT_q_px
 complex(SP), allocatable :: elph_gkkp_expanded(:,:,:,:)
 ! ... indexes ...
 integer,     allocatable :: THETA_matrix_index(:,:,:)
 integer,     allocatable :: GKKP_matrix_index(:)
 integer,     allocatable :: Q_io_map(:)
 integer,     allocatable :: K_io_map(:)
 !
 ! Hole/Electron Time-dependent life-times
 !
 real(SP),    allocatable :: E_P_Hole_lifetime(:,:)
 real(SP),    allocatable :: E_P_Electron_lifetime(:,:)
 real(SP),    allocatable :: E_P_Phonon_lifetime(:,:)
 !
 ! Coulomb scattering kernel...
 !----------------------------
 type(PP_indexes),   SAVE :: RT_qp_px
 real(SP),    allocatable :: E_E_Hole_lifetime(:,:)
 real(SP),    allocatable :: E_E_Electron_lifetime(:,:)
 !
 ! ... ouput max/min lifetimes
 !-----------------------------
 real(SP)                 :: RT_mean_lifetimes(5)
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
 logical           :: l_G_retarded_DB
 logical           :: l_G_DB_has_el_ph
 integer           :: Gr_mat_dim
 integer           :: Gr_T_steps
 integer           :: Gr_E_steps
 integer           :: Gr_bands(2)
 real(SP)          :: Gr_QP_width
 real(SP)          :: Gr_T_step
 real(SP)          :: Gr_T_window
 real(SP)          :: Gr_E_window
 character(schlen) :: Gr_kind
 integer,     allocatable :: Gr_indexes(:,:,:,:)
 real(SP),    allocatable :: Gr_ee_matrix(:,:)
 real(SP),    allocatable :: Gr_ep_matrix(:,:,:,:)
 real(SP),    allocatable :: Gr_peak_pos(:,:)
 real(SP),    allocatable :: Gr_E_ranges(:,:,:)
 complex(SP), allocatable :: Gr(:,:,:)
 !
 ! GFs
 !-----
 !
 complex(SP), allocatable :: G_lesser_reference(:,:,:)
 complex(SP), allocatable :: Ho_plus_Sigma(:,:,:,:)
 complex(SP), allocatable :: Sigma_relax(:,:,:)
 !
 real(SP),    allocatable :: rho_reference(:)    ! reference density from SC_lw_band:SC_bands(2)
 real(SP),    allocatable :: full_rho(:)         ! density of all bands from 1:SC_bands(2)
 !
 ! Integrator Variables
 !
 character(schlen), parameter :: EULER    = "EULER"
 character(schlen), parameter :: EULEREXP = "EULEREXP"
 character(schlen), parameter :: RK2      = "RK2"
 character(schlen), parameter :: RK2EXP   = "RK2EXP"
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
 integer                  :: it_start ! restart point
 integer                  :: RT_IO_steps 
 integer                  :: RT_IO_steps_DB 
 real(SP)                 :: RT_IO_time 
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
   subroutine RT_average_operator(VALUE,O_REAL,O_QP,O_MATRIX,G,k,en,TRANS)
     use pars,       ONLY:SP
     use SC,         ONLY:SC_bands
     use R_lattice,  ONLY:bz_samp 
     use QP_m,       ONLY:QP_nk,QP_table,QP_n_states
     use electrons,  ONLY:n_sp_pol,levels
     use FFT_m,      ONLY:fft_size
     type(bz_samp), intent(in) :: k
     complex(SP),   intent(in) :: G(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
     type(levels),  intent(in) :: en
     complex(SP),  intent(out) :: VALUE
     complex(SP),     optional, intent(in) :: O_MATRIX(SC_bands(2),SC_bands(2),QP_nk,n_sp_pol)
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
     use pars,           ONLY:SP,schlen
     use drivers,        ONLY:l_sc_hf,l_HF_and_locXC,l_sc_contains_lda,l_ph_corr,l_carrier_dynamics,&
&                             l_sc_cohsex         
     use memory_m,       ONLY:mem_est
     use SC,             ONLY:SC_bands,V_hartree_sc,local_V,SC_alloc,E_bare,rho_n, &
&                             SC_xc_functional,SC_xc_functional,RT_occupations
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
       !
       allocate(QP_Vnl_xc(QP_n_states))
       call mem_est("QP_Vnl_xc",(/QP_n_states/))
       !
     endif
     !
     if(l_NE_with_fields) then
       allocate(J_and_P_cache(J_and_P_cache_size,RT_IO_steps))
       call mem_est('Current_Cache',(/size(J_and_P_cache)/),(/SP/))
     endif
     !
     allocate(E_xc(fft_size))
     call mem_est('Exc',(/fft_size/),(/SP/))
     !
     allocate(G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk,NE_MEM_steps))
     call mem_est("G_lesser",(/size(G_lesser)/),(/2*SP/))
     !
     allocate(RT_occupations(SC_bands(1):SC_bands(2),QP_nk))
     call mem_est("RT_occupations",(/size(RT_occupations)/),(/SP/))
     !
     if (l_carrier_dynamics) then
       allocate(E_E_Hole_lifetime(SC_bands(1):SC_bands(2),QP_nk))
       allocate(E_E_Electron_lifetime(SC_bands(1):SC_bands(2),QP_nk))
       call mem_est("RT_E_E_lifetimes",(/2*size(E_E_Hole_lifetime)/),(/SP/))
       E_E_Hole_lifetime=0._SP
       E_E_Electron_lifetime=0._SP
     endif
     !
     if (l_ELPH_CCA_Kernel.and.l_ph_corr) then
       allocate(E_P_Hole_lifetime(SC_bands(1):SC_bands(2),QP_nk))
       allocate(E_P_Electron_lifetime(SC_bands(1):SC_bands(2),QP_nk))
       allocate(E_P_Phonon_lifetime(SC_bands(1):SC_bands(2),QP_nk))
       call mem_est("RT_E_P_lifetimes",(/3*size(E_P_Hole_lifetime)/),(/SP/))
     endif
    !
     allocate(Ho_plus_Sigma(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk,n_sp_pol))
     call mem_est("Ho_plus_Sigma",(/size(Ho_plus_Sigma)/),(/2*SP/))
     !
     if(RT_is_dumped.or.l_ph_corr) then
       allocate(Sigma_relax(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk))
       call mem_est("Sigma_relax",(/size(Sigma_relax)/),(/2*SP/))
     endif
     !
     if(l_RT_needs_XC0) then
       allocate(XC0_and_HARTREE0(1:SC_bands(2),1:SC_bands(2),QP_nk,n_sp_pol))
       call mem_est("XC0_and_HARTREE0",(/size(XC0_and_HARTREE0)/),(/2*SP/))
     endif
     !
     allocate(SC_R(1:SC_bands(2),1:SC_bands(2),QP_nk,n_sp_pol))
     call mem_est("SC_R",(/size(SC_R)/),(/2*SP/))
     !
     allocate(G_lesser_reference(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk))
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
     ! Retarded Green Functions
     !
     if (l_G_retarded_DB) then
       allocate(Gr(nkibz,Gr_bands(2),Gr_T_steps))
       call mem_est("G_retarded",(/size(Gr)/),(/2*SP/))
       allocate(Gr_peak_pos(nkibz,Gr_bands(2)))
       call mem_est("G_peak_pos",(/size(Gr_peak_pos)/))
       allocate(Gr_E_ranges(nkibz,Gr_bands(2),2))
       call mem_est("G_E_ranges",(/size(Gr_E_ranges)/))
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
     use SC,             ONLY:V_hartree_sc,Ho,SC_free,E_bare,rho_n,RT_k,RT_occupations
     use memory_m,       ONLY:mem_est
     use QP_m,           ONLY:QP_Vnl_xc
     use xc_functionals, ONLY:E_xc
     use parallel_m,     ONLY:PP_indexes_reset
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
     deallocate(E_xc)
     call mem_est('Exc')
     !
     deallocate(Ho_plus_Sigma)
     call mem_est("Ho_plus_Sigma")
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
       deallocate(E_P_Hole_lifetime,E_P_Electron_lifetime,E_P_Phonon_lifetime)
       call mem_est("RT_E_P_lifetimes")
     endif
     !
     deallocate(RT_occupations)
     call mem_est("RT_occupations")
     !
     deallocate(G_lesser)
     call mem_est("G_lesser")
     !
     if (allocated(q_weight)) deallocate(q_weight)
     !
     if (allocated(THETA_matrix)) then
       deallocate(THETA_matrix)
       call mem_est("THETA_matrix")
       deallocate(THETA_matrix_index,GKKP_matrix_index)
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
     deallocate(RT_k)
     !
     if(l_RT_diagonalize) then
       deallocate(Occ_natural_orb)
       call mem_est("Occ_natural")
     endif
     !
     if(l_NE_with_fields) then
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
     ! Retarded Green Functions
     !
     if (allocated(Gr)) then
       deallocate(Gr)
       call mem_est("G_retarded")
     endif
     if (allocated(Gr_indexes)) then
       deallocate(Gr_indexes)
       deallocate(Gr_ee_matrix)
       call mem_est("Gr_indexes G_matrix")
     endif
     !
     call PP_indexes_reset(RT_k_px) 
     call PP_indexes_reset(RT_q_px) 
     call PP_indexes_reset(RT_qp_px) 
     !
   end subroutine RT_free
   !
   logical function Gr_ordering_condition(i_n,i_k,i_m,i_p)
     !
     integer i_k,i_n,i_p,i_m
     !
     Gr_ordering_condition=.FALSE.
     !
     if (i_k<i_p) then
       Gr_ordering_condition=.TRUE.
     else if (i_k==i_p.and.i_n<=i_m) then
       Gr_ordering_condition=.TRUE.
     endif
     !
   end function
   !
   integer function MEM_index(it)
     !
     integer :: it
     MEM_index=mod(it,NE_MEM_steps)
     if (MEM_index==0) MEM_index=NE_MEM_steps
     if (it        <0) MEM_index=1
     !
   end function
   !
end module
