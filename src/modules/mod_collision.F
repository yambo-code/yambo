!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
   !                             :
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
 end type ggwinfo
 !
 !====================
 ! EXTENDED COLLISION
 !====================
 !
 integer                  :: n_collisions
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
 type COLLISIONS_element
   character(schlen)         :: name="none"
   integer                   :: kind=0
   integer                   :: D(3,2) ! Dimension
   integer                   :: N      ! Linear Dimension
   integer(LP), allocatable  :: table(:,:,:)
   complex(SP), allocatable  :: v(:)
   complex(SP), allocatable  :: v3(:,:,:)
 end type
 !
 type(COLLISIONS_element), allocatable :: GW_NEQ_collisions(:)
 type(COLLISIONS_element), allocatable :: HF_collisions(:)
 type(COLLISIONS_element), allocatable :: COHSEX_collisions(:)
 type(COLLISIONS_element), allocatable :: STATIC_collisions(:)
 !
 contains
   !
   subroutine COLLISIONS_alloc(what,COLL,N)
     use memory_m,       ONLY:mem_est
     use stderr,         ONLY:intc
     use QP_m,           ONLY:QP_n_states
     use SC,             ONLY:SC_bands
     use D_lattice,      ONLY:nsym
     use R_lattice,      ONLY:nqbz,nkibz
     use global_XC,      ONLY:SC_HF,SC_COHSEX,SE_GW_NEQ
     implicit none
     character(*)             :: what
     integer                  :: N
     type(COLLISIONS_element) :: COLL(N)
     !WS
     integer :: I,V_dim
     !
     if (what=="GW_NEQ") then
       COLL(1)%D(1,:)=(/1,QP_n_states/)
       COLL(1)%D(2,:)=(/1,nsym/)
       COLL(1)%D(3,:)=(/1,nqbz/)
       COLL(1)%kind=SE_GW_NEQ
       COLL(1)%name="GW_NEQ"
     else if (what=="HF".or.what=="COHSEX".or.what=="STATIC") then
       if (what=="HF")     COLL(1)%kind=SC_HF
       if (what=="HF")     COLL(1)%name="HF"
       if (what=="COHSEX") COLL(1)%kind=SC_COHSEX
       if (what=="COHSEX") COLL(1)%name="COHSEX"
       COLL(1)%D(1,:)=SC_bands
       COLL(1)%D(2,:)=SC_bands
       COLL(1)%D(3,:)=(/1,nkibz/)
     endif
     !
     V_dim=(COLL(1)%D(1,2)-COLL(1)%D(1,1)+1)*&
&          (COLL(1)%D(2,2)-COLL(1)%D(2,1)+1)*&
&          (COLL(1)%D(3,2)-COLL(1)%D(3,1)+1)
     !
     do I=1,N
       COLL(I)%D=COLL(1)%D
       allocate(COLL(I)%v3(COLL(1)%D(1,1):COLL(1)%D(1,2),&
&                          COLL(1)%D(2,1):COLL(1)%D(2,2),&
&                          COLL(1)%D(3,1):COLL(1)%D(3,2)))
       allocate(COLL(I)%table(COLL(1)%D(1,1):COLL(1)%D(1,2),&
&                             COLL(1)%D(2,1):COLL(1)%D(2,2),&
&                             COLL(1)%D(3,1):COLL(1)%D(3,2)))
       allocate(COLL(I)%v(V_dim))
     enddo
     call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_table",(/size(COLL(1)%table)/),(/LP/))
     call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v",(/size(COLL(1)%v)/),(/2*SP/))
     call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v3",(/size(COLL(1)%v3)/),(/2*SP/))
     !
   end subroutine
   !
   subroutine COLLISIONS_free(what,COLL,N)
     use memory_m,       ONLY:mem_est
     use stderr,         ONLY:intc
     implicit none
     character(*)             :: what
     integer                  :: N
     type(COLLISIONS_element) :: COLL(N)
     !
     ! Work Space
     !
     integer :: I
     !
     do I=1,N
       !
       if (allocated(COLL(I)%table)) then
         deallocate(COLL(I)%table)
       endif
       !
       if (allocated(COLL(I)%v)) then
         deallocate(COLL(I)%v)
       endif
       !
       if (allocated(COLL(I)%v3)) then
         deallocate(COLL(I)%v3)
       endif
       !
     enddo
     !
     call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_table")
     call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v")
     call mem_est("COLLISIONS_"//trim(COLL(1)%name)//"_v3")
     !
     if (what=="HF".and.allocated(HF_collisions))         deallocate(HF_collisions)
     if (what=="COHSEX".and.allocated(COHSEX_collisions)) deallocate(COHSEX_collisions)
     if (what=="GW_NEQ".and.allocated(GW_NEQ_collisions)) deallocate(GW_NEQ_collisions)
     if (what=="STATIC".and.allocated(STATIC_collisions)) deallocate(STATIC_collisions)
     !
   end subroutine COLLISIONS_free
   !
   subroutine collision_reset(ggw)
     type(ggwinfo)::ggw
     if(associated(ggw%gamp))  deallocate(ggw%gamp)
     if(associated(ggw%rhotw)) deallocate(ggw%rhotw)
     nullify(ggw%gamp,ggw%rhotw)
   end subroutine
   !
end module collision
