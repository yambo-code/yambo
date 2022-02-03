! 
!        Copyright (C) 2000-2021 the YAMBO team
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
module fl_optics
 !
 use descriptors,     ONLY:IO_desc
 use pars,            ONLY:SP,schlen,lchlen
 use electrons,       ONLY:levels
 use nl_optics
 !
#include<memory.h>
!!
! !
!! Long range correction field
!!
!real(SP)          :: NL_LRC_alpha
!!
!! Time steps and step length defined in module real_time.F
!! --------------------------------------------------------
!!
!! Polarization
!! ------------
!! Calculated from the string-averaged discretized geometric phase
!! see ref. PRB 47, 1651 (1993), or from DIPOLES
!!
!complex(SP)    :: NL_P(6)  ! NL_P(1:3) Berry Pol, NL_P(4:6) Dip Pol
!complex(SP)    :: NL_initial_P(6) ! in principle it can be different from zero
!!
!!
!! Initial Energies and Damping
!!------------
!complex(SP), allocatable :: I_relax(:,:,:,:,:)
!real(SP),    allocatable :: E_full(:,:,:)
!!
!! For TDDFT, V_xc_0 stores the V_xc at equilibrium
!!
!real(SP), allocatable    :: V_xc_0(:,:)
!!
!! For JGM E_xc_0 for the microscopic part of the functional
!!
!complex(SP) :: E_xc_0(3)
!!
!! WFs and Density
!!
!! Bands in terms of Kohn-Sham orbitals
 complex(SP), allocatable :: FL_V_bands(:,:,:,:,:)  ! Floquet bands
 complex(SP), allocatable :: FL_S_plus(:,:,:,:,:,:)  ! Floquet overlaps
 complex(SP), allocatable :: FL_S_minus(:,:,:,:,:,:)  ! Floquet overlaps
 complex(SP), allocatable :: FL_Ho_plus_Sigma(:,:,:,:)  ! Floquet Hamiltonian
!!
!! Time-dependent Valence bands in real-space
!complex(SP), allocatable :: VAL_BANDS(:,:,:,:)
!!
!real(SP),    allocatable :: full_rho(:)       ! density of all bands from 1:SC_bands(2)
!!
!! Density matrix
!complex(SP), allocatable :: dG(:,:,:)
!!
!! Verbosity
!!
!integer, parameter :: VERB_HIGH = 1
!integer, parameter :: VERB_LOW  = 2
!!
!integer :: NL_verbosity
!character(schlen)            :: NL_verb_name
!!
!! Correlation
!!
!character(schlen)            :: NL_correlation
!integer                      :: Correlation
!!
!integer, parameter :: IPA    = 1
!integer, parameter :: HARTREE= 2
!integer, parameter :: TDDFT  = 3
!integer, parameter :: LRC    = 4
!integer, parameter :: JGM    = 5
!integer, parameter :: LRCW   = 6
!integer, parameter :: HF     = 7
!integer, parameter :: SEX    = 8
!!
!logical :: eval_COLLISIONS
!logical :: eval_dG
!!
!! Non-linear spectra parameters
!!
!real(SP) :: NL_er(2)
!real(SP) :: NL_estep
!integer  :: NL_bands(2)
!integer  :: NL_nbands
!integer  :: n_frequencies
!integer  :: n_freqs_todo
!!
!! Wavefunction sampling
!!
!type NLsampleWF
!  integer              :: n_order     ! Sampling NL order
!  integer              :: n_size      ! Size of sampling
!  integer              :: n_val       ! Number of valence
!  integer              :: n_k         ! Number of kpoints
!  logical              :: ison        ! WF sample logical switch
!  integer, allocatable :: indx_T(:)   ! indexes of time sampling
!end type NLsampleWF
!!
!integer  :: n_order
!type(NLsampleWF) :: WF_sample, WF_sample_ondisk
!!
!! Databases input/output
!!
!integer                  :: ID(3)
!!
!! Other
!!
!logical  :: l_use_DIPOLES   ! Use standard dipoles (valid only for linear response)
!logical  :: l_eval_CURRENT  ! Evaluate current using the commutator v=[H,r] and the IP formulation
!!
!! IO variables
!!
!real(SP),     allocatable :: NL_P_t(:,:)
!complex(SP),  allocatable :: E_tot_t(:,:),E_ext_t(:,:), E_ks_t(:,:),V_bands_t(:,:,:,:,:)
!complex(SP)               :: E_tot(3),E_ext(3),E_ks(3)
!logical,      allocatable :: freqs_done(:)
!type(IO_desc)             :: NL_desc
 !
 !aim137 new variables
 integer                   :: max_fl_mode=3 ! momentaneamente fijo
 integer                   :: max_extra_mode
 integer                   :: n_tot_fl_modes
 integer                   :: n_tot_extra_modes
 integer                   :: FL_space_dim,FL_states(2)
 !
 contains
   subroutine FL_alloc(en)
     !
     use pars,           ONLY:SP
     use FFT_m,          ONLY:fft_size
     use electrons,      ONLY:levels,n_spin,n_sp_pol
     use hamiltonian,    ONLY:FL_H_alloc
     use real_time,      ONLY:rho_reference,RT_Vnl_xc
     use R_lattice,      ONLY:nXkbz
     use QP_m,           ONLY:QP_nk
     use electric,       ONLY:ELECTRIC_alloc
     use drivers,        ONLY:l_use_Hxc_collisions
     use nl_optics,      ONLY:V_bands
     !
     implicit none
     !
     type(levels), intent(in) :: en
     !
     max_extra_mode = 2*max_fl_mode
     n_tot_fl_modes = 2*max_fl_mode+1
     n_tot_extra_modes = 2*max_extra_mode+1
     FL_space_dim = NL_bands(2) * n_tot_fl_modes
     FL_states(1) = (NL_bands(1)-1)*n_tot_fl_modes+1
     FL_states(2) = FL_space_dim
     !
     call FL_H_alloc(en,.true.,.false.,FL_states)
     !
     ! aim137 allocate V_bands because I still need it for FL_start_and_restart
     YAMBO_ALLOC(V_bands,(NL_bands(2),en%nbf,QP_nk,n_sp_pol))
     ! aim137 FL_bands in FKS space broken down in bands x fl modes
     YAMBO_ALLOC(FL_V_bands,(NL_bands(2),n_tot_fl_modes,en%nbf,QP_nk,n_sp_pol))
     !
     YAMBO_ALLOC(FL_Ho_plus_Sigma,(FL_states(1):FL_states(2),FL_states(1):FL_states(2),QP_nk,n_sp_pol))
     !
     if (fft_size>0) then
       YAMBO_ALLOC(rho_reference,(fft_size))
       YAMBO_ALLOC(full_rho,(fft_size))
     endif
     !
     ! FL overlaps
     !
     YAMBO_ALLOC(FL_S_plus,(en%nbf,en%nbf,n_tot_extra_modes,6,QP_nk,n_sp_pol))
     YAMBO_ALLOC(FL_S_minus,(en%nbf,en%nbf,n_tot_extra_modes,6,QP_nk,n_sp_pol))
     !
     ! Reference energies
     !
     YAMBO_ALLOC(E_full,(1:NL_bands(2),QP_nk,n_sp_pol))
     !
     if(.not.l_use_DIPOLES) then
       call  ELECTRIC_alloc(en)
     endif
     !
   !
   end subroutine FL_alloc
   !
   subroutine FL_alloc_k_distributed(en) !aim137 still need to modify this subroutine
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
     if(fft_size>0) then
       YAMBO_ALLOC(VAL_BANDS,(fft_size,en%nbf,PAR_Xk_nibz,n_sp_pol))
     endif
     YAMBO_ALLOC(I_relax,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),en%nbf,PAR_Xk_nibz,n_sp_pol))
     YAMBO_ALLOC(Ho_plus_Sigma,(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),PAR_Xk_nibz,n_sp_pol))
     !
   end subroutine FL_alloc_k_distributed
   !
   subroutine FL_free
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
     YAMBO_FREE(VAL_BANDS)
     !
     if(.not.l_use_DIPOLES) then
       call ELECTRIC_free()
     endif
     !
   end subroutine FL_free
 !
end module
