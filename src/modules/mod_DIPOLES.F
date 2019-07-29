!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
module DIPOLES
 !
 use pars,  ONLY:SP,lchlen,schlen
 use units, ONLY:HA2EV
 !
#include <memory.h>
 !
 ! Logicals
 !
 logical                  :: use_covariant_approach
 logical                  :: use_shifted_grids_approach
 logical                  :: use_g_space_approach
 logical                  :: use_real_space_approach
 logical                  :: Vnl_commutator_warning
 ! 
 ! other
 !
 integer                  :: num_shifted_grids
 logical                  :: eval_OVERLAPS
 !
 ! Allocs
 !
 complex(SP), allocatable :: DIP_projected(:,:,:,:)
 complex(SP), allocatable :: DIP_iR(:,:,:,:,:)
 complex(SP), allocatable :: DIP_P(:,:,:,:,:)
 complex(SP), allocatable :: DIP_v(:,:,:,:,:)
 complex(SP), allocatable :: DIP_S(:,:,:,:,:)
 complex(SP), allocatable :: DIP_orbital(:,:,:,:,:,:)
 complex(SP), allocatable :: DIP_spin(:,:,:,:,:)
 complex(SP), allocatable :: DIP_P_spinor(:,:,:,:,:,:)
 complex(SP), allocatable :: P_square(:,:,:,:)
 !
 type DIPOLE_t
   integer    :: iq(2)
   integer    :: ib(2)
   integer    :: ib_lim(2)
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
   subroutine DIP_alloc(what,d)
     !
     use electrons, ONLY:n_sp_pol,n_spinor
     use pars,      ONLY:cZERO
     !
     character(*)      :: what
     integer, optional :: d(:)
     !
     ! Work Space
     !
     integer :: err
     !
     if (present(d)) then
       select case(what)
       case('DIP_iR')
         if (allocated(DIP_iR)) return
         YAMBO_ALLOC(DIP_iR,(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol))
         DIP_iR=cZERO
       case('DIP_P')
         if (allocated(DIP_P)) return
         YAMBO_ALLOC(DIP_P,(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol))
         DIP_P=cZERO
       case('DIP_v')
         if (allocated(DIP_v)) return
         YAMBO_ALLOC(DIP_v,(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol))
         DIP_v=cZERO
       case('DIP_S')
         if (allocated(DIP_S)) return
         YAMBO_ALLOC(DIP_S,(d(1),d(2),d(3),d(4),n_sp_pol))
         DIP_S=cZERO
       case('DIP_orbital')
         if (allocated(DIP_orbital)) return
         YAMBO_ALLOC(DIP_orbital,(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol,2))
         DIP_orbital=cZERO
       case('DIP_spin')
         if (allocated(DIP_spin)) return
         YAMBO_ALLOC(DIP_spin,(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol))
         DIP_spin=cZERO
       case('DIP_P_spinor')
         if (allocated(DIP_P_spinor).or.n_spinor==1) return
         YAMBO_ALLOC(DIP_P_spinor,(d(1),n_spinor,n_spinor,d(2):d(3),d(4):d(5),d(6)))
         DIP_P_spinor=cZERO
       case('DIP_projected')
         if (allocated(DIP_projected)) return
         YAMBO_ALLOC(DIP_projected,(d(1):d(2),d(3):d(4),d(5),n_sp_pol))
         DIP_projected=cZERO
       case('P_square')
         if (allocated(P_square)) return
         YAMBO_ALLOC(P_square,(d(1):d(2),d(3):d(4),d(5),n_sp_pol))
         P_square=cZERO
       end select
     else
       select case(what)
       case('DIP_iR')
         if (.not.allocated(DIP_iR)) return
         YAMBO_FREE(DIP_iR)
       case('DIP_P')
         if (.not.allocated(DIP_P)) return
         YAMBO_FREE(DIP_P)
       case('DIP_v')
         if (.not.allocated(DIP_v)) return
         YAMBO_FREE(DIP_v)
       case('DIP_S')
         if (.not.allocated(DIP_S)) return
         YAMBO_FREE(DIP_S)
       case('DIP_orbital')
         if (.not.allocated(DIP_orbital)) return
         YAMBO_FREE(DIP_orbital)
       case('DIP_spin')
         if (.not.allocated(DIP_spin)) return
         YAMBO_FREE(DIP_spin)
       case('DIP_P_spinor')
         if (.not.allocated(DIP_P_spinor)) return
         YAMBO_FREE(DIP_P_spinor)
       case('DIP_projected')
         if (.not.allocated(DIP_projected)) return
         YAMBO_FREE(DIP_projected)
       case('P_square')
         if (.not.allocated(P_square)) return
         YAMBO_FREE(P_square)
       end select
     endif
   end subroutine
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
     Dip%computed="R P"
   end subroutine DIPOLES_reset
   !
end module

