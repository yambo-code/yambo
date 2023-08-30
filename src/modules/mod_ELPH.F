!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module ELPH
 !
 use pars,           ONLY:SP,rZERO,schlen
 use electrons,      ONLY:levels,E_reset
 use descriptors,    ONLY:IO_desc
 !
#include<memory.h>
 !
 ! Dumensions
 integer   :: ph_modes=0
 integer   :: elph_nb=0
 integer   :: elph_nk_bz=0
 integer   :: elph_nQ=0
 integer   :: elph_nQ_used=0
 integer   :: elph_branches(2)=0
 !
 logical   :: elph_use_q_grid=.FALSE.
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
 ! Double grid
 !
 logical              :: use_PH_DbGd
 !
 type(levels)         :: PH_freqs
 !
 character(schlen)    :: EkplusQ_mode
 integer              :: ID_E_kpq=1
 integer              :: ID_E_kpq_obj=1
 real(SP)             :: E_kpq_sh_fact=20.0_SP
 real(SP)             :: Efermi_DbGd=0._SP
 !
 type E_kpq_fine_grid
   integer         :: nk
   integer         :: nq_around
   real(SP),   allocatable :: E_kpq(:,:,:,:)
   real(SP),   allocatable :: f_kpq(:,:,:,:)
 end type
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
   ! Phonon assisted double-grid energy/occupation levels (in the q-BZ)
   type(E_kpq_fine_grid),   allocatable :: E_dg_kpq(:)
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
 integer   :: elph_Ham_bands(2)
 integer   :: elph_Ham_ik
 real(SP)  :: RES_tresh
 !
 ! gFsq matrices
 integer              :: gsqF_energy_steps
 real(SP),allocatable :: gsqF_fan(:,:,:,:)
 real(SP),allocatable :: gsqF_dw(:,:,:)
 real(SP),allocatable :: gsqF_ca_corr(:,:,:,:)
 real(SP),allocatable :: gsqF_life_bose(:,:,:,:)
 real(SP),allocatable :: gsqF_life_f(:,:,:,:)
 !
 ! el-ph Self-Energy
 integer   :: QP_PH_n_G_bands(2)=0
 logical   :: eval_G_using_KK ! Perform KK of the spectral function
 real(SP)  :: FAN_deltaE_treshold
 real(SP)  :: DW_deltaE_treshold
 real(SP)  :: MAX_gkkp=rZERO
 !
 ! Descriptors
 type(IO_desc) :: ELPH_desc
 type(IO_desc) :: PHEL_desc
 !
 ! Databases
 logical :: l_GKKP_hosts_bare_dV=.FALSE.
 logical :: l_GKKP_DB_exists=.FALSE.
 logical :: l_GKKP_expanded_DB_exists=.FALSE.
 character(schlen) :: gkkp_db ! Database type for el-ph calculation (gkkp | expanded | genFroh )
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
   subroutine FineGd_E_kpq_components_reset(E)
     !      
     type(E_kpq_fine_grid):: E
     !
     E%nk     =0
     E%nq_around=0
     !
     YAMBO_FREE(E%E_kpq)
     YAMBO_FREE(E%f_kpq)
     !
   end subroutine
   !
   subroutine FineGd_E_kpq_alloc(E)
     !      
     use electrons,      ONLY:n_sp_pol
     !
     type(E_kpq_fine_grid):: E
     !
     YAMBO_ALLOC(E%E_kpq,(elph_nb,E%nk,n_sp_pol,E%nq_around))
     YAMBO_ALLOC(E%f_kpq,(elph_nb,E%nk,n_sp_pol,E%nq_around))
     !
   end subroutine
   !
end module ELPH
