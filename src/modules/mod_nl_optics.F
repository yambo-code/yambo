! 
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG AC
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
module nl_optics
 !
 use pars,       ONLY:SP,schlen,lchlen
 use electrons,  ONLY:levels
 use real_time,  ONLY:NE_steps
 !
 implicit none
 !
 ! Long range correction field
 !
 real(SP)          :: NL_LRC_alpha
 !
 ! Time steps and step length defined in module real_time.F
 ! --------------------------------------------------------
 ! 
 ! Polarization 
 ! ------------
 ! Calculated from the string-averaged discretized geometric phase
 ! see ref. PRB 47, 1651 (1993), or from DIPOLES
 !
 complex(SP)    :: NL_initial_P(3) ! in principle it can be different from zero
 !
 ! Diagonal (QP) and off-diagonal (Phase) relaxation time
 !
 real(SP)    :: NL_damping
 !
 ! Initial Energies and Damping
 !------------
 complex(SP), allocatable :: I_relax(:,:,:,:,:)
 real(SP),    allocatable :: E_full(:,:,:)
 !
 ! For TDDFT, V_xc_0 stores the V_xc at equilibrium
 !
 real(SP), allocatable    :: V_xc_0(:,:)
 !
 ! For JGM E_xc_0 for the microscopic part of the functional
 !
 complex(SP) :: E_xc_0(3)
 !
 ! WFs and Densityt
 !
 complex(SP), allocatable :: V_bands(:,:,:,:)  ! time-dependent valence bands
 real(SP),    allocatable :: full_rho(:)               ! density of all bands from 1:SC_bands(2)
 complex(SP), allocatable :: dG(:,:,:) 
 !
 ! Integrator Variables
 !
 integer, parameter :: EULER    = 1
 integer, parameter :: EULEREXP = 2
 integer, parameter :: RK2      = 3
 integer, parameter :: RK4      = 4
 integer, parameter :: RK2EXP   = 5
 integer, parameter :: RK4EXP   = 6
 integer, parameter :: HEUN     = 7
 integer, parameter :: INVINT   = 8
 integer, parameter :: CRANK_NICOLSON = 9
 !
 integer                      :: Integrator
 !
 ! Correlation
 !
 character(schlen)            :: NL_correlation
 integer                      :: Correlation
 !
 integer, parameter :: IPA    = 1
 integer, parameter :: HARTREE= 2
 integer, parameter :: TDDFT  = 3
 integer, parameter :: LRC    = 4
 integer, parameter :: JGM    = 5
 integer, parameter :: LRCW   = 6
 integer, parameter :: HF     = 7
 integer, parameter :: SEX    = 8
 !
 logical :: eval_COLLISIONS
 logical :: eval_dG
 !
 ! Non-linear spectra parameters
 !
 real(SP) :: NL_er(2)
 real(SP) :: NL_estep
 integer  :: n_frequencies
 integer  :: n_freqs_todo
 !
 ! Databases input/output
 !
 integer                  :: ID(3)
 !
 ! Other
 !
 logical  :: l_use_DIPOLES   ! Use standard dipoles (valid only for linear response)
 complex(SP), allocatable :: DIP_bz(:,:,:,:,:)
 !
 ! IO variables
 !
 real(SP),     allocatable :: NL_P_t(:,:)
 complex(SP),  allocatable :: E_tot_t(:,:),E_ext_t(:,:), E_ks_t(:,:)
 complex(SP)               :: E_tot(3),E_ext(3),E_ks(3)
 logical,      allocatable :: freqs_done(:)
 integer                   :: n_NL_descriptions
 character(lchlen)         :: NL_description(100)
 !
 contains
   !
   subroutine NL_alloc(en)
     !
     use pars,           ONLY:SP
     use memory_m,       ONLY:mem_est
     use FFT_m,          ONLY:fft_size
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use SC,             ONLY:SC_bands,E_bare
     use hamiltonian,    ONLY:H_alloc
     use real_time,      ONLY:rho_reference,Ho_plus_Sigma,RT_Vnl_xc
     use R_lattice,      ONLY:nXkbz
     use QP_m,           ONLY:QP_nk
     use electric,       ONLY:ELECTRIC_alloc
     !
     implicit none
     !
     type(levels), intent(in) :: en
     !
     call H_alloc(en)
     !
     allocate(Ho_plus_Sigma(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk,n_sp_pol))
     call mem_est("Ho_plus_Sigma",(/size(Ho_plus_Sigma)/),(/2*SP/))
     !
     allocate(I_relax(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),en%nbf,QP_nk,n_sp_pol))
     call mem_est("I_relax",(/size(I_relax)/),(/2*SP/))
     !
     allocate(V_bands(1:SC_bands(2),en%nbf,QP_nk,n_sp_pol))
     call mem_est("V_bands",(/size(V_bands)/),(/2*SP/))
     !
     allocate(rho_reference(fft_size))
     call mem_est("rho_reference",(/fft_size/),(/SP/))
     !
     allocate(full_rho(fft_size))
     call mem_est("full_rho",(/fft_size/),(/SP/))
     !
     if(Correlation==TDDFT.or.Correlation==JGM) then
       allocate(V_xc_0(fft_size,n_spin))
       call mem_est("V_xc_0",(/fft_size,n_spin/),(/SP/))
     endif          
     !
     if(Correlation==SEX.or.Correlation==HF) then
       allocate(dG(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk))
       call mem_est("dG",(/size(dG)/),(/2*SP/))
       allocate(RT_Vnl_xc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk,n_sp_pol))
       call mem_est("RT_Vnl_xc",(/size(RT_Vnl_xc)/),(/2*SP/))
     endif          
     !
     ! Reference energies
     !
     allocate(E_bare(1:SC_bands(2),QP_nk,n_sp_pol))
     call mem_est("E_bare",(/size(E_bare)/))
     E_bare=en%E(:SC_bands(2),:,:)
     !
     allocate(E_full(1:SC_bands(2),QP_nk,n_sp_pol))
     call mem_est("E_full",(/size(E_full)/))
     !
     if(l_use_DIPOLES) then
       allocate(DIP_bz(3,SC_bands(2),SC_bands(2),nXkbz,n_sp_pol))
       call mem_est("DIP_bz",(/size(DIP_bz)/))
     else
       call  ELECTRIC_alloc(en)
     endif
     !
     allocate(NL_P_t(NE_steps,3))
     call mem_est("NL_P_t",(/size(NL_P_t)/))
     !
     allocate(E_tot_t(NE_steps,3),E_ext_t(NE_steps,3),E_ks_t(NE_steps,3))
     call mem_est("E_tot_t E_ext_t",(/size(E_tot_t),size(E_ext_t),size(E_ks_t)/))
     !
   end subroutine NL_alloc
   
   subroutine NL_free
     !
     use memory_m,       ONLY:mem_est
     use SC,             ONLY:E_bare
     use hamiltonian,    ONLY:H_free
     use real_time,      ONLY:rho_reference,Ho_plus_Sigma,RT_Vnl_xc
     use electric,       ONLY:ELECTRIC_free
     !
     implicit none
     !
     call H_free()
     !
     deallocate(Ho_plus_Sigma)
     call mem_est("Ho_plus_Sigma")
     !
     deallocate(I_relax)
     call mem_est("I_relax")
     !
     deallocate(V_bands)
     call mem_est("V_bands")
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
     if(allocated(E_full)) then
       deallocate(E_full)
       call mem_est("E_full")
     endif
     !
     if(allocated(V_xc_0)) then
       deallocate(V_xc_0)
       call mem_est("V_xc_0")
     endif
     !
     if(l_use_DIPOLES) then
       deallocate(DIP_bz)
       call mem_est("DIP_bz")
     else
       call ELECTRIC_free()
     endif
     !
     deallocate(NL_P_t)
     call mem_est("NL_P_t")
     !
     deallocate(E_tot_t,E_ext_t,E_ks_t)
     call mem_est("E_tot_t E_ext_t E_ks_t")
     !
     if(Correlation==SEX.or.Correlation==HF) then
       deallocate(dG)
       call mem_est("dG")
       deallocate(RT_Vnl_xc)
       call mem_est("RT_Vnl_xc")
     endif
     !
   end subroutine NL_free
   !
   real function Alpha_ED(en,omega)
   !
   real(SP) :: Egap,beta,omega
   type(levels) :: en
     !	
     Egap = sum(en%E(en%nbf+1,:,1)-en%E(en%nbf,:,1))/real(en%nk,SP)
     beta = NL_LRC_alpha/Egap**2
     Alpha_ED = NL_LRC_alpha + beta*omega**2
   end function
   !
end module
