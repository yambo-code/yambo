! 
!        Copyright (C) 2000-2019 the YAMBO team
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
#include<memory.h>
 !
 ! Long range correction field
 !
 real(SP)          :: NL_LRC_alpha
 !
 ! Time steps and step length defined in module real_time.F
 ! --------------------------------------------------------
 !
 integer           :: NL_ng      ! number of G-vectors in the Hartree/TDDFT
 ! 
 ! Polarization 
 ! ------------
 ! Calculated from the string-averaged discretized geometric phase
 ! see ref. PRB 47, 1651 (1993), or from DIPOLES
 !
 complex(SP)    :: NL_initial_P(3) ! in principle it can be different from zero
 !
 ! Current
 !
 complex(SP)    :: NL_J(6)
 complex(SP)    :: NL_initial_J(6)
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
 ! WFs and Density
 !
 ! Bands in terms of Kohn-Sham orbitals
 complex(SP), allocatable :: V_bands(:,:,:,:)  ! time-dependent valence bands
 ! 
 ! Time-dependent Valence bands in real-space
 complex(SP), allocatable :: VAL_BANDS(:,:,:,:)
 !
 real(SP),    allocatable :: full_rho(:)       ! density of all bands from 1:SC_bands(2)
 !
 ! Density matrix
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
 ! Verbosity
 !
 integer, parameter :: VERB_HIGH = 1
 integer, parameter :: VERB_LOW  = 2
 !
 integer :: NL_verbosity
 character(schlen)            :: NL_verb_name
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
 logical :: eval_OVERLAPS
 !
 ! Non-linear spectra parameters
 !
 real(SP) :: NL_er(2)
 real(SP) :: NL_estep
 integer  :: NL_bands(2)
 integer  :: NL_nbands
 integer  :: n_frequencies
 integer  :: n_freqs_todo
 !
 ! Wavefunction sampling
 !
 type NLsampleWF
   integer              :: n_order     ! Sampling NL order
   integer              :: n_size      ! Size of sampling
   integer              :: n_val       ! Number of valence
   integer              :: n_k         ! Number of kpoints
   logical              :: ison        ! WF sample logical switch 
   integer, allocatable :: indx_T(:)   ! indexes of time sampling
 end type NLsampleWF
 !
 integer  :: n_order
 type(NLsampleWF) :: WF_sample, WF_sample_ondisk 
 !
 ! Databases input/output
 !
 integer                  :: ID(3)
 !
 ! Other
 !
 logical  :: l_use_DIPOLES   ! Use standard dipoles (valid only for linear response)
 logical  :: l_eval_CURRENT  ! Evaluate current using the commutator v=[H,r] and the IP formulation
 !
 ! IO variables
 !
 real(SP),     allocatable :: NL_P_t(:,:)
 complex(SP),  allocatable :: E_tot_t(:,:),E_ext_t(:,:), E_ks_t(:,:),V_bands_t(:,:,:,:,:)
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
     use FFT_m,          ONLY:fft_size
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use hamiltonian,    ONLY:H_alloc
     use real_time,      ONLY:rho_reference,RT_Vnl_xc
     use R_lattice,      ONLY:nXkbz
     use QP_m,           ONLY:QP_nk
     use electric,       ONLY:ELECTRIC_alloc
     use drivers,        ONLY:l_use_Hxc_collisions
     !
     implicit none
     !
     type(levels), intent(in) :: en
     !
     call H_alloc(en,.true.,.false.)
     !
     !
     YAMBO_ALLOC(V_bands,(NL_bands(2),en%nbf,QP_nk,n_sp_pol))
     YAMBO_ALLOC(rho_reference,(fft_size))
     YAMBO_ALLOC(full_rho,(fft_size))
     !
     if(Correlation==TDDFT.or.Correlation==JGM) then
       YAMBO_ALLOC(V_xc_0,(fft_size,n_spin))
     endif          
     !
     if(l_use_Hxc_collisions) then
       YAMBO_ALLOC(dG,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk))
       YAMBO_ALLOC(RT_Vnl_xc,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),QP_nk,n_sp_pol))
     endif          
     !
     ! Reference energies
     !
     YAMBO_ALLOC(E_full,(1:NL_bands(2),QP_nk,n_sp_pol))
     !
     if(.not.l_use_DIPOLES) then
       call  ELECTRIC_alloc(en)
     endif
     !
     YAMBO_ALLOC(NL_P_t,(NE_steps,3))
     YAMBO_ALLOC(E_tot_t,(NE_steps,3))
     YAMBO_ALLOC(E_ext_t,(NE_steps,3))
     YAMBO_ALLOC(E_ks_t,(NE_steps,3))
     !
     call NL_ALLOC_WF_sample(WF_sample,NL_bands(2),n_sp_pol)
     !
   end subroutine NL_alloc
   !
   subroutine NL_alloc_k_distributed(en)
     !  
     use parallel_m,     ONLY:PAR_Xk_nibz,PAR_Xk_ibz_index
     use pars,           ONLY:SP
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use real_time,      ONLY:Ho_plus_Sigma
     use R_lattice,      ONLY:nXkbz
     use QP_m,           ONLY:QP_nk
     use FFT_m,          ONLY:fft_size
     !
     implicit none
     !
     type(levels),  intent(in) :: en
     !
     ! Valence bands in fft space
     !
     YAMBO_ALLOC(VAL_BANDS,(fft_size,en%nbf,PAR_Xk_nibz,n_sp_pol))
     YAMBO_ALLOC(I_relax,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),en%nbf,PAR_Xk_nibz,n_sp_pol))
     YAMBO_ALLOC(Ho_plus_Sigma,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),PAR_Xk_nibz,n_sp_pol))
     !
   end subroutine NL_alloc_k_distributed
   !
   subroutine NL_free
     !
     use hamiltonian,    ONLY:H_free
     use real_time,      ONLY:rho_reference,Ho_plus_Sigma,RT_Vnl_xc
     use electric,       ONLY:ELECTRIC_free
     use drivers,        ONLY:l_use_Hxc_collisions
     !
     implicit none
     !
     call H_free()
     !
     YAMBO_FREE(Ho_plus_Sigma)
     YAMBO_FREE(I_relax)
     YAMBO_FREE(V_bands)
     !
     YAMBO_FREE(rho_reference)
     YAMBO_FREE(full_rho)
     YAMBO_FREE(E_full)
     YAMBO_FREE(V_xc_0)
     YAMBO_FREE(VAL_BANDS)
     !
     if(.not.l_use_DIPOLES) then
       call ELECTRIC_free()
     endif
     !
     YAMBO_FREE(NL_P_t)
     YAMBO_FREE(E_tot_t)
     YAMBO_FREE(E_ext_t)
     YAMBO_FREE(E_ks_t)
     YAMBO_FREE(V_bands_t)
     !
     if(l_use_Hxc_collisions) then
       YAMBO_FREE(dG)
       YAMBO_FREE(RT_Vnl_xc)
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
   subroutine NL_set_WF_sample(W,n,Tstep,Trange,v,k)
     !
     type(NLsampleWF),intent(out) :: W
     integer         ,intent(in)  :: n 
     real(SP)        ,intent(in)  :: Tstep
     real(SP)        ,intent(in)  :: Trange(2)
     integer         ,optional    :: v,k 
     ! ws:
     integer :: i_t,i_t_start,i_deltaT     
     !
     W%ison = .false.
     if (n > 0) then
       W%ison = .true.
       W%n_order = min(n,7)
       W%n_size = 2*W%n_order + 1
       if (present(v)) W%n_val = v
       if (present(k)) W%n_k = k
       YAMBO_ALLOC(W%indx_T,(W%n_size))
       i_t_start = nint(Trange(1)/Tstep)+1
       i_deltaT=nint((Trange(2)-Trange(1))/Tstep)/W%n_size
       forall(i_t=0:W%n_size-1) W%indx_T(1+i_t) = i_t_start + i_t*i_deltaT 
     end if
     !
   end subroutine NL_set_WF_sample
   !
   subroutine NL_COPY_WF_sample(W1,W2)
     !
     type(NLsampleWF),intent(in) :: W1
     type(NLsampleWF),intent(out) :: W2
     !
     W2%ison = W1%ison
     if (W1%ison) then
       W2%n_order = W1%n_order
       W2%n_size  = W1%n_size
       W2%n_val   = W1%n_val 
       W2%n_k     = W1%n_k
       YAMBO_ALLOC(W2%indx_T,(W2%n_size))
       W2%indx_T(:) = W1%indx_T(:)
     end if
     !
   end subroutine NL_COPY_WF_sample
   !
   subroutine NL_INIT_WF_sample(W)
     !
     type(NLsampleWF),intent(inout) :: W
     !
     W%ison = .false.
     W%n_order = 0
     W%n_size = 0
     W%n_val  = 0
     W%n_k    = 0
     !
   end subroutine NL_INIT_WF_sample
   !
   subroutine NL_ALLOC_WF_sample(W,nb,ns)
     !
     type(NLsampleWF),intent(in) :: W
     integer,         intent(in) :: nb,ns
     !
     if (W%ison) then
       YAMBO_ALLOC(V_bands_t,(W%n_size,nb,W%n_val,W%n_k,ns))
     endif
   end subroutine NL_ALLOC_WF_sample
   !
end module
