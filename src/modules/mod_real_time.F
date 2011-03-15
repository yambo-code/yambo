!
! Copyright (C) 2000-2010 A. Marini, M. Gruening and the YAMBO team 
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
 use SC,        ONLY:G_lesser,SC_R
 use pars,      ONLY:DP,SP,schlen
 !
 implicit none
 !
#if defined _TWO_LEVELS
 !
 ! Three-levels flags
 !
 logical  :: l_3levels
 real(SP) :: tau_offdiag(3)  ! dephasing terms (tau_12,tau_13,tau_23)
 real(SP) :: tau_diag(3)     ! diagonal terms  (tau_11,tau_22,tau_33)
 real(SP) :: dip(3)          ! dipoles (D_12, D_13, D_23) In principle they can be complex
 real(SP) :: W1
 !
 complex(SP) :: Rabi_freq_pump
 complex(SP) :: Rabi_freq_probe
 complex(SP) :: Rabi_nn(2,2)
 complex(SP) :: P
 complex(SP) :: D
 real(SP)    :: W_pump
 real(SP)    :: delta
 real(SP)    :: Wo
#endif
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
 ! and the analytic obtained summing all 
 ! single-e/h-pair GFs 
 !
 logical     :: P_2levels_is_done
 complex(SP) :: RT_P_2levels(3)
 complex(SP), allocatable :: Poles_2levels_save(:,:,:,:)
 !
 ! Diagonal (QP) and off-diagonal (Phase) relaxation time
 !
 real(SP)    :: Phase_LifeTime
 real(SP)    :: QP_LifeTime
 logical     :: RT_is_dumped
 !
 ! GFs
 !
 complex(SP), allocatable :: G_lesser_previous(:,:,:)
 complex(SP), allocatable :: G_lesser_reference(:,:,:)
 complex(SP), allocatable :: I1_lesser(:,:,:)
 complex(SP), allocatable :: I2_lesser(:,:,:)
 !
 real(SP),    allocatable :: rho_reference(:)
 !
 ! Integrator Variables
 !
 character(schlen), parameter :: EULER    = "EULER"
 character(schlen), parameter :: RK2      = "RK2"
 character(schlen), parameter :: EXACT    = "EXACT"
 character(schlen), parameter :: RK2EXACT = "RK2EXACT"
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
 integer                  :: ID(5)                
 integer                  :: it_start ! restart point
 integer                  :: max_rt_steps_disk 
 integer,  allocatable    :: kpoints_map(:,:)
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
   subroutine RT_TwoL_Polarization(E,eval_P,k)
     use R_lattice,  ONLY:bz_samp
     use electrons,  ONLY:levels
     logical,       intent(in)           :: eval_P
     type(levels),  intent(in)           :: E
     type(bz_samp), optional, intent(in) :: k
   end subroutine
   !
   subroutine RT_average_operator(VALUE,O_REAL,O_QP,O_MATRIX,G,k,TRANS)
     use pars,       ONLY:SP
     use SC,         ONLY:SC_bands
     use R_lattice,  ONLY:bz_samp 
     use QP_m,       ONLY:QP_nk,QP_table,QP_n_states
     use electrons,  ONLY:n_sp_pol
     use FFT_m,      ONLY:fft_size
     type(bz_samp), intent(in) :: k
     complex(SP),   intent(in) :: G(SC_bands,SC_bands,QP_nk)
     complex(SP),  intent(out) :: VALUE
     complex(SP),     optional, intent(in) :: O_MATRIX(SC_bands,SC_bands,QP_nk,n_sp_pol)
     complex(SP),     optional, intent(in) :: O_QP(QP_n_states)
     real(SP),        optional, intent(in) :: O_REAL(fft_size,n_sp_pol)
     logical,         optional, intent(in) :: TRANS
   end subroutine RT_average_operator
   !
   subroutine RT_update(G_input,SC_R,it,Xen,k,Xk,X,up_Gref,up_DIP,an_QP)
     use pars,       ONLY:SP
     use SC,         ONLY:SC_bands
     use R_lattice,  ONLY:bz_samp
     use QP_m,       ONLY:QP_nk
     use electrons,  ONLY:levels,n_sp_pol
     use X_m,        ONLY:X_t
     !
     type(X_t),      intent(inout)        :: X
     complex(SP),    intent(inout)        :: G_input(SC_bands,SC_bands,QP_nk)
     complex(SP),    intent(in)           :: SC_R(SC_bands,SC_bands,QP_nk,n_sp_pol)
     type(bz_samp),  intent(in)           :: k,Xk
     type(levels),   intent(in)           :: Xen
     integer,        intent(in)           :: it
     logical,        optional, intent(in) :: up_Gref,up_DIP,an_QP
     !
   end subroutine RT_update
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
     use drivers,        ONLY:l_sc_hf,l_HF_and_locXC
     use memory_m,       ONLY:mem_est
     use SC,             ONLY:SC_bands,V_hartree_sc,local_V,SC_alloc,E_bare,rho_n
     use FFT_m,          ONLY:fft_size
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use xc_functionals, ONLY:E_xc
     use QP_m,           ONLY:QP_nk,QP_n_states,QP_Vnl_xc
     use collisions_IO,  ONLY:collisions_IO_alloc
     !
     type(levels), intent(in) :: en
     !
     call SC_alloc(en)
     !
     if (l_HF_and_locXC) then
       !
       allocate(QP_Vnl_xc(QP_n_states))
       call mem_est("QP_Vnl_xc",(/QP_n_states/))
       !
     endif
     !
     if(l_NE_with_fields) then
       allocate(J_and_P_cache(J_and_P_cache_size,max_rt_steps_disk))
       call mem_est('Current_Cache',(/size(J_and_P_cache)/),(/SP/))
     endif
     !
     allocate(E_xc(fft_size))
     call mem_est('Exc',(/fft_size/),(/SP/))
     !
     allocate(G_lesser(SC_bands,SC_bands,QP_nk))
     call mem_est("G_lesser",(/size(G_lesser)/),(/2*SP/))
     !
     if(l_RT_needs_XC0) then
       !
       allocate(XC0_and_HARTREE0(SC_bands,SC_bands,QP_nk,n_sp_pol))
       call mem_est("XC0_and_HARTREE0",(/size(XC0_and_HARTREE0)/),(/2*SP/))
       !
     endif
     !
     allocate(SC_R(SC_bands,SC_bands,QP_nk,n_sp_pol))
     call mem_est("SC_R",(/size(SC_R)/),(/2*SP/))
     !
     allocate(I1_lesser(SC_bands,SC_bands,QP_nk))
     call mem_est("I1_lesser",(/size(I1_lesser)/),(/2*SP/))
     !
     allocate(I2_lesser(SC_bands,SC_bands,QP_nk))
     call mem_est("I2_lesser",(/size(I2_lesser)/),(/2*SP/))
     !
     allocate(G_lesser_previous(SC_bands,SC_bands,QP_nk))
     call mem_est("Gf_l_old",(/size(G_lesser_previous)/),(/2*SP/))
     !
     allocate(G_lesser_reference(SC_bands,SC_bands,QP_nk))
     call mem_est("Gf_l_ref",(/size(G_lesser_reference)/),(/2*SP/))
     !
     allocate(rho_reference(fft_size))
     call mem_est("rho_reference",(/fft_size/),(/SP/))
     !
     ! Reference energies
     !
     allocate(E_bare(SC_bands,QP_nk,n_sp_pol))
     call mem_est("E_bare",(/size(E_bare)/))
     E_bare=en%E(:SC_bands,:,:)
     !
     if(l_RT_diagonalize) then
       allocate(Occ_natural_orb(SC_bands,QP_nk))
       call mem_est("Occ_natural",(/size(Occ_natural_orb)/))
     endif
     !
     call collisions_IO_alloc()
     !
   end subroutine RT_alloc
   
   subroutine RT_free
     !
     use drivers,        ONLY:l_sc_hf,l_HF_and_locXC
     use SC,             ONLY:V_hartree_sc,Ho,SC_free,E_bare,rho_n
     use memory_m,       ONLY:mem_est
     use QP_m,           ONLY:QP_Vnl_xc
     use xc_functionals, ONLY:E_xc
     use collisions_IO,  ONLY:collisions_IO_free
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
     if(allocated(XC0_and_HARTREE0)) then
       !      
       deallocate(XC0_and_HARTREE0)
       call mem_est("XC0_and_HARTREE0")
       !
     endif
     !
     if(allocated(SC_R)) then
       deallocate(SC_R)
       call mem_est("SC_R")
     endif
     !
     deallocate(G_lesser)
     call mem_est("G_lesser")
     !
     deallocate(I1_lesser)
     call mem_est("I1_lesser")
     !
     deallocate(I2_lesser)
     call mem_est("I2_lesser")
     !
     deallocate(G_lesser_previous)
     call mem_est("Gf_l_old")
     !
     deallocate(G_lesser_reference)
     call mem_est("Gf_l_ref")
     !
     if(allocated(rho_reference)) then
       deallocate(rho_reference)
       call mem_est("rho_reference")
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
     if(l_NE_with_fields) then
       deallocate(J_and_P_cache)
       call mem_est('Current_Cache')
     endif
     !
     if (allocated(Poles_2levels_save)) then
       deallocate(Poles_2levels_save)
       call mem_est("Poles_2levels_save")
     endif
     !
     call collisions_IO_free() 
     !
   end subroutine RT_free
   !
end module
