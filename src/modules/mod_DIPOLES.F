!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
module DIPOLES
 !
 use pars,          ONLY:SP,lchlen,schlen
 use units,         ONLY:HA2EV
 use descriptors,   ONLY:IO_desc
 use gpu_m,         ONLY:have_gpu
 !
#include <dev_defs.h>
#include <y_memory.h>
 !
 ! Logicals
 !
 logical                  :: Vnl_commutator_warning      =.FALSE.
 !
 ! List of observables associated to the specific approach and/or to other routines
 !
 character(schlen) :: shifted_grids_obs=""
 character(schlen) :: covariant_obs    =""
 character(schlen) :: g_space_obs      =""
 character(schlen) :: x_space_obs      =""
 character(schlen) :: specific_obs     =""
 ! 
 ! other
 !
 integer                  :: num_shifted_grids
 logical                  :: eval_OVERLAPS
 ! 
 ! Descriptor
 !
 type(IO_desc)            :: DIP_desc
 !
 ! Allocs
 !
 complex(SP), allocatable :: DIP_iR(:,:,:,:,:)
 complex(SP), allocatable :: DIP_P(:,:,:,:,:)
 complex(SP), allocatable :: DIP_v(:,:,:,:,:)
 complex(SP), allocatable :: DIP_S(:,:,:,:,:)
 complex(SP), allocatable :: DIP_orbital(:,:,:,:,:,:)
 complex(SP), allocatable :: DIP_spin(:,:,:,:,:,:)
 complex(SP), allocatable :: DIP_P_spinor(:,:,:,:,:,:)
 complex(SP), allocatable :: P_square(:,:,:,:)
 !
 ! DIP Transverse workspace
 !
 complex(SP), allocatable          :: DIP_work_drho(:,:,:)
 complex(SP), allocatable DEV_ATTR :: DIP_work_drho_d(:,:,:)
 !
 ! type def
 !
 type DIPOLE_t
   integer    :: iq(2)
   integer    :: ib(2)     !> @var ib(1)  lowest valence, ib(2)     upper conduction
   integer    :: ib_lim(2) !> @var ib_lim(1) top valence, ib_lim(2) bottom conduction  
   integer    :: db_ib(2)
   integer    :: db_ib_lim(2)
   integer    :: ng
   real(SP)   :: ehe(2)
   real(SP)   :: q0(3)
   real(SP)   :: Energy_treshold
   real(SP)   :: Energy_treshold_vv
   logical    :: v_eff
   logical    :: Vnl_included
   logical    :: bands_ordered
   logical    :: force_v_g_space
   character(schlen) :: approach
   character(schlen) :: computed
   !
   ! TO DO: add PAR INDEX / COMM and everything needeed (in particular by DIPOLES_IO)
   !        inside dipole type. Like that I do not need to give extra arguments in input
   !        to DIPOLES_IO
   !
 end type
 !
 contains
   !
   subroutine DIPOLES_duplicate(Dip_in,Dip_out)
     type(DIPOLE_t), intent(in) :: Dip_in
     type(DIPOLE_t), intent(out):: Dip_out
     Dip_out%iq        = Dip_in%iq
     Dip_out%ib        = Dip_in%ib
     Dip_out%ib_lim    = Dip_in%ib_lim
     Dip_out%db_ib     = Dip_in%db_ib
     Dip_out%db_ib_lim = Dip_in%db_ib_lim
     Dip_out%ng        = Dip_in%ng
     Dip_out%ehe       = Dip_in%ehe
     Dip_out%q0        = Dip_in%q0
     Dip_out%Energy_treshold    = Dip_in%Energy_treshold
     Dip_out%Energy_treshold_vv = Dip_in%Energy_treshold_vv
     Dip_out%v_eff              = Dip_in%v_eff
     Dip_out%Vnl_included       = Dip_in%Vnl_included
     Dip_out%bands_ordered      = Dip_in%bands_ordered
     Dip_out%force_v_g_space    = Dip_in%force_v_g_space
     Dip_out%approach           = Dip_in%approach
     Dip_out%computed           = Dip_in%computed
   end subroutine DIPOLES_duplicate
   !
   subroutine DIPOLES_reset(Dip) 
     type(DIPOLE_t)::Dip
     Dip%iq=(/1,1/)
     Dip%ib=(/1,-1/)
     Dip%ib_lim=(/1,-1/)
     Dip%db_ib=(/1,-1/)
     Dip%db_ib_lim=(/1,-1/)
     Dip%ng=0
     Dip%ehe=(/-1._SP,-1._SP/)/HA2EV
     Dip%q0=(/1._SP,0._SP,0._SP/)
     Dip%Energy_treshold=1.E-5_SP/HA2EV
     Dip%Energy_treshold_vv=1.E+5_SP/HA2EV
     Dip%v_eff=.false.
     Dip%Vnl_included=.false.
     Dip%bands_ordered=.true.
     Dip%force_v_g_space=.false.
     Dip%approach="G-space v"
     Dip%computed=" "
   end subroutine DIPOLES_reset
   !
end module
