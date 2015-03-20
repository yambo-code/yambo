!
!        Copyright (C) 2000-2015 the YAMBO team
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
module collision
 !
 use pars, ONLY :SP,LP,schlen
 !
 implicit none
 !
 !====================
 ! STANDARD COLLISION
 !====================
 !
 type ggwinfo
   !
   !  is(3)=(ib,ik,is,isp) --<--:--<-- os(3)=(ob,ok,os,osp)
   !                            :
   !                           /:\ iqs=(ig0,iq,is)
   !                            :
   !  iq_is = bz(ik_is-ok_os)+Go
   !
   integer :: is(4) 
   integer :: os(4)
   integer :: qs(3)
   integer :: ngrho,iqref
   complex(SP), pointer :: gamp(:,:) => null()
   complex(SP), pointer :: rhotw(:)  => null()
   !
 end type ggwinfo
 !
 !====================
 ! EXTENDED COLLISION
 !====================
 !
 integer                  :: V_kind 
 !
 ! Cut off on exchange matrix elements
 ! Sx_cutoff=0 all, (Sx_cutoff>1 or Sx_cutoff<0)=none
 !
 real(SP)                 :: COLLISIONS_cutoff
 real(SP)                 :: COLLISIONS_max
 !
 logical :: COLLISIONS_parallel_SC_structure=.FALSE.
 !
 ! Fragments
 !
 integer, allocatable       :: COLLISIONS_IO_status(:)
 !
 type COLLISIONS_element
   character(schlen)     :: name="none"
   integer               :: kind=0
   integer               :: D(3,2)=0 ! Dimension (v3)
   integer               :: N        ! Linear Dimension
   integer(LP), pointer  :: table(:,:,:)    => null()
   complex(SP), pointer  :: v_SEX(:)        => null()  ! SEX or HF collisions
   complex(SP), pointer  :: v3_SEX(:,:,:)   => null()
   real(SP)   , pointer  :: v_GW(:)         => null()  ! GW NEQ collisions
   real(SP)   , pointer  :: v3_GW(:,:,:)    => null()
 end type
 !
 integer :: nGW_NEQ_collisions=0
 type(COLLISIONS_element), allocatable :: GW_NEQ_collisions(:)
 integer :: nFock_collisions=0
 type(COLLISIONS_element), allocatable :: Fock_collisions(:)
 integer :: nSEX_collisions=0
 type(COLLISIONS_element), allocatable :: SEX_collisions(:)
 integer :: nGW_QED_collisions=0
 type(COLLISIONS_element), allocatable :: GW_QED_collisions(:)
 !
 contains
   !
   subroutine collision_reset(ggw,INIT_ONLY)
     type(ggwinfo)::ggw
     logical,optional::INIT_ONLY
     logical::init_only_=.false.
     !
     if(present(INIT_ONLY)) init_only_=INIT_ONLY
     if(.not.init_only_) then
       if(associated(ggw%gamp))  deallocate(ggw%gamp)
       if(associated(ggw%rhotw)) deallocate(ggw%rhotw)
     endif
     nullify(ggw%gamp,ggw%rhotw)
   end subroutine
   !
end module collision
