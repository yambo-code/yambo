!
!        Copyright (C) 2000-2016 the YAMBO team
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
 !
 ! Logicals
 !
 logical                  :: use_covariant_approach
 logical                  :: use_shifted_grids_approach
 logical                  :: use_g_space_approach
 logical                  :: use_real_space_approach
 logical                  :: force_v_g_space_approach
 logical                  :: Vnl_commutator_warning
 ! 
 ! other
 !
 integer                  :: num_shifted_grids
 !
 ! Allocs
 !
 complex(SP), allocatable :: DIP_projected(:,:,:,:)
 complex(SP), allocatable :: DIP_iR(:,:,:,:,:)
 complex(SP), allocatable :: DIP_P(:,:,:,:,:)
 complex(SP), allocatable :: DIP_S(:,:,:,:,:)
 complex(SP), allocatable :: DIP_spin(:,:,:,:,:)
 complex(SP), allocatable :: P_square(:,:,:,:)
 !
 type DIPOLE_t
   integer    :: iq(2)
   integer    :: ib(2)
   integer    :: ib_lim(2)
   integer    :: db_ib(2)
   integer    :: db_ib_lim(2)
   integer    :: ngostnts
   real(SP)   :: ehe(2)
   real(SP)   :: q0(3)
   real(SP)   :: Energy_treshold
   real(SP)   :: Energy_treshold_vv
   logical    :: Vnl_included
   character(schlen) :: approach
 end type
 !
 contains
   !
   subroutine DIP_alloc(what,d)
     !
     use electrons, ONLY:n_sp_pol
     use memory_m,  ONLY:mem_est
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
         allocate(DIP_iR(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol),stat=err)
         call mem_est(what,(/size(dip_iR)/),errors=(/err/))
         DIP_iR=cZERO
       case('DIP_P')
         if (allocated(DIP_P)) return
         allocate(DIP_P(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol),stat=err)
         call mem_est(what,(/size(dip_P)/),errors=(/err/))
         DIP_P=cZERO
       case('DIP_S')
         if (allocated(DIP_S)) return
         allocate(DIP_S(d(1),d(2),d(3),d(4),n_sp_pol),stat=err)
         call mem_est(what,(/size(dip_S)/),errors=(/err/))
         DIP_S=cZERO
       case('DIP_spin')
         if (allocated(DIP_spin)) return
         allocate(DIP_spin(d(1),d(2):d(3),d(4):d(5),d(6),n_sp_pol),stat=err)
         call mem_est(what,(/size(dip_P)/),errors=(/err/))
         DIP_spin=cZERO
       case('DIP_projected')
         if (allocated(DIP_projected)) return
         allocate(DIP_projected(d(1):d(2),d(3):d(4),d(5),n_sp_pol),stat=err)
         call mem_est(what,(/size(DIP_projected)/),errors=(/err/))
         DIP_projected=cZERO
       case('P_square')
         if (allocated(P_square)) return
         allocate(P_square(d(1):d(2),d(3):d(4),d(5),n_sp_pol),stat=err)
         call mem_est(what,(/size(P_square)/),errors=(/err/))
         P_square=cZERO
       end select
     else
       select case(what)
       case('DIP_iR')
         if (.not.allocated(DIP_iR)) return
         deallocate(DIP_iR)
       case('DIP_P')
         if (.not.allocated(DIP_P)) return
         deallocate(DIP_P)
       case('DIP_S')
         if (.not.allocated(DIP_S)) return
         deallocate(DIP_S)
       case('DIP_spin')
         if (.not.allocated(DIP_spin)) return
         deallocate(DIP_spin)
       case('DIP_projected')
         if (.not.allocated(DIP_projected)) return
         deallocate(DIP_projected)
       case('P_square')
         if (.not.allocated(P_square)) return
         deallocate(P_square)
       end select
       call mem_est(what)
     endif
   end subroutine
   !
end module

