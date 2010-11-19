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
 use SC,        ONLY:G_lesser
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
 !
 ! Inital XC (SC or default) are requested
 !
 logical           :: l_RT_needs_XC0
 !
 ! To exclude the Diamagnetic term 
 !
 logical           :: l_RT_no_diamagnetic
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
 !
 ! Frozen parts of self-energy
 !
 logical           :: Hfrozen
 logical           :: XCfrozen 
 !
 ! Time steps and step length
 !
 real(SP) :: RT_step
 real(SP) :: Thermal_time
 real(SP) :: Ext_fields_initial_itime
 integer  :: NE_steps
 integer  :: Thermal_steps
 integer  :: RT_memory_steps
 !
 ! Macroscopic induced current ... 
 !
 complex(SP) :: RT_ind_J(3)
 !
 ! ... and polarization
 !
 complex(SP) :: RT_P(3)
 !
 ! External current to generate a given vector potential
 !
 complex(SP) :: j_current_ext(3)
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
 ! Integrator Variables
 !
 integer                      :: i_time   ! full time index
 integer                      :: ef1_time ! time index for extern field one
 integer                      :: ef2_time ! time index for extern field two
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
 complex(SP), allocatable :: H_t(:,:,:,:)
 !
 ! Occupation natural orbitals
 !
 real(SP),    allocatable :: Occ_natural_orb(:,:) 
 !
 ! Databases input/output
 !
 integer,  parameter      :: current_cache_size=24
 !
 integer                  :: ID(5)                
 integer                  :: it_start ! restart point
 integer                  :: max_rt_steps_disk 
 integer,  allocatable    :: kpoints_map(:,:)
 real(SP), allocatable    :: current_cache(:,:)
 !
 ! Interface
 !
 interface
   !
   subroutine RT_output(what,VALUEs,TIME_or_E)
     use pars,       ONLY:SP
     character(*),       intent(in) :: what
     real(SP), optional, intent(in) :: VALUEs(:) 
     real(SP), optional, intent(in) :: TIME_or_E
   end subroutine
   !
   subroutine RT_average_operator(VALUE,O_REAL,O_QP,O_MATRIX,G,k)
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
   end subroutine RT_average_operator
   !
   subroutine RT_Integrator(G_new,G_old,Avec,Avec_old,T_step,Xen,k,q,Integrator,RT_time)
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
     implicit none
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
       allocate(current_cache(current_cache_size,max_rt_steps_disk))
       call mem_est('Current_Cache',(/size(current_cache)/),(/SP/))
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
     if(l_RT_diagonalize.or.trim(Integrator_name)==EXACT.or.trim(Integrator_name)==RK2EXACT &
&      .or.(RT_is_dumped.and.Thermal_Steps/=0)) then  
       allocate(H_t(SC_bands,SC_bands,QP_nk,n_sp_pol))
       call mem_est("H_t",(/size(H_t)/),(/2*SP/))
     endif
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
     implicit none
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
     if(allocated(H_t)) then
       deallocate(H_t)
       call mem_est("H_t")
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
     deallocate(E_bare)
     call mem_est("E_bare")
     !
     if(l_RT_diagonalize) then
       deallocate(Occ_natural_orb)
       call mem_est("Occ_natural")
     endif
     !
     if(l_NE_with_fields) then
       deallocate(current_cache)
       call mem_est('Current_Cache')
     endif
     !
     call collisions_IO_free() 
     !
   end subroutine RT_free
   !
end module
