!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
module ELPH
 !
 use pars,       ONLY:SP,rZERO,schlen
 use electrons,  ONLY:levels,E_reset
 !
 implicit none
 !
 ! Dumensions
 integer   :: ph_modes=0
 integer   :: elph_nb=0
 integer   :: elph_nk_bz=0
 integer   :: elph_nQ=0
 integer   :: elph_nQ_used=0
 integer   :: elph_branches(2)=0
 !
 logical   :: elph_use_q_grid=.TRUE.
 logical   :: elph_grids_are_expanded=.FALSE.
 !
 ! QP props
 real(SP),    allocatable :: QP_OMS_Fan(:)
 real(SP),    allocatable :: QP_DW(:)
 !
 ! PHONON props 
 real(SP)                 :: PH_W_debye=rZERO
 real(SP),    allocatable :: PH_kpt_bz(:,:)
 real(SP),    allocatable :: PH_qpt(:,:)
 real(SP),    allocatable :: PH_freqs_sq(:,:)
 complex(SP), allocatable :: PH_pol_vector(:,:,:,:)
 real(SP),    allocatable :: PH_Q_modulus(:)
 real(SP),    allocatable :: PH_acoustic_speed(:,:)
 logical,     allocatable :: PH_acoustic_branch(:)
 !
 ! GKKP Matrix Elements and derived quantities
 type GKKP_me
   !
   character(schlen)        :: KIND="none" ! dV, dV_bare, dV_sq, DW, dV_bare_sq
   !
   ! dVr/c(Nmodes,Nb,Nb,Nk,Nq)
   real(SP),    allocatable :: dVr(:,:,:,:,:)
   complex(SP), allocatable :: dVc(:,:,:,:,:)
   !
   ! Phonon assisted energy/occupation levels
   type(levels),allocatable :: E_kpq(:)
   !
   integer                  :: Nq_mem=0
   integer                  :: Nk_mem=0
   integer                  :: Nb(2)=0
   integer                  :: Nmodes(2)=0
   !
 end type
 !
 type(GKKP_me), SAVE :: GKKP
 type(GKKP_me), SAVE :: GKKP_bare
 type(GKKP_me), SAVE :: GKKP_ep_sq
 type(GKKP_me), SAVE :: GKKP_pe_sq
 !
 integer,     allocatable :: Q_io_map(:)
 integer,     allocatable :: K_io_map(:)
 !
 ! El_h Hamiltonian
 !
 integer   :: elph_Ham_bands(2)
 integer   :: elph_Ham_ik
 real(SP)  :: RES_tresh
 ! 
 ! gFsq matrices
 !
 integer              :: gsqF_energy_steps
 real(SP),allocatable :: gsqF_fan(:,:,:,:)
 real(SP),allocatable :: gsqF_dw(:,:,:)
 real(SP),allocatable :: gsqF_ca_corr(:,:,:,:)
 real(SP),allocatable :: gsqF_life_bose(:,:,:,:)
 real(SP),allocatable :: gsqF_life_f(:,:,:,:)
 !
 ! el-ph Self-Energy
 !
 integer   :: QP_PH_n_G_bands
 logical   :: eval_G_using_KK ! Perform KK of the spectral function
 real(SP)  :: FAN_deltaE_treshold
 real(SP)  :: DW_deltaE_treshold
 real(SP)  :: MAX_gkkp=rZERO
 !
 contains
   !
   subroutine setup_k_plus_q_levels(iq,E)
     use electrons,  ONLY:spin_occ
     use D_lattice,  ONLY:Tel
     use R_lattice,  ONLY:nkbz
     use functions,  ONLY:Fermi_fnc
     real(SP)    ::E
     integer     ::iq,ib,ik
     do ib=GKKP%Nb(1),GKKP%Nb(2)
       do ik=1,nkbz
         GKKP%E_kpq(iq)%E(ib,ik,1)=GKKP%E_kpq(iq)%E(ib,ik,1)-E
         GKKP%E_kpq(iq)%f(ib,ik,1)=spin_occ*Fermi_fnc(GKKP%E_kpq(iq)%E(ib,ik,1),Tel)
       enddo 
     enddo
   end subroutine
   !
end module ELPH
