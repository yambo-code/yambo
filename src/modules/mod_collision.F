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
 use pars, ONLY :DP,SP,LP,schlen
 !
 implicit none
 !
 !=====================
 ! ELEMENTAL COLLISION
 !=====================
 !
 type elemental_collision
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
   integer :: ngrho
   integer :: iqref
   !
   character(schlen) :: name="El_Coll"
   !
   complex(SP), pointer :: gamp(:,:) => null()
   complex(SP), pointer :: rhotw(:)  => null()
   !
   ! scatter bamp workspace
   !
   complex(DP), pointer :: rho_tw_rs(:) => null()
   complex(SP), pointer :: WF_symm_i(:,:) => null()
   complex(SP), pointer :: WF_symm_o(:,:) => null()
   !
 end type elemental_collision
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
   integer(LP), pointer  :: table(:,:,:)   => null()
   complex(SP), pointer  :: v_c(:)         => null()  ! SEX or HF collisions
   complex(SP), pointer  :: v3_c(:,:,:)    => null()
   real(SP)   , pointer  :: v_r(:)         => null()  ! GW NEQ collisions
   real(SP)   , pointer  :: v3_r(:,:,:)    => null()
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
   subroutine elemental_collision_free(ggw)
     type(elemental_collision)::ggw
     logical::init_only_=.false.
     if(associated(ggw%gamp))      deallocate(ggw%gamp)
     if(associated(ggw%rhotw))     deallocate(ggw%rhotw)
     if(associated(ggw%rho_tw_rs)) deallocate(ggw%rho_tw_rs)
     if(associated(ggw%WF_symm_i)) deallocate(ggw%WF_symm_i)
     if(associated(ggw%WF_symm_o)) deallocate(ggw%WF_symm_o)
     nullify(ggw%gamp,ggw%rhotw,ggw%rho_tw_rs,ggw%WF_symm_i,ggw%WF_symm_o)
   end subroutine
   !
   subroutine elemental_collision_alloc(ggw,NG,NG_GAMP,GRADIENT,TITLE)
     use electrons,      ONLY:n_spinor
     use FFT_m,          ONLY:fft_size
     use memory_m,       ONLY:mem_est
     implicit none
     type(elemental_collision) ::ggw
     integer,         OPTIONAL :: NG
     integer,         OPTIONAL :: NG_GAMP(2)
     logical,         OPTIONAL :: GRADIENT
     character(*),    OPTIONAL :: TITLE
     !
     if (present(TITLE)) then
       ggw%name=TITLE
     endif
     !
     allocate(ggw%rho_tw_rs(fft_size))
     allocate(ggw%WF_symm_i(fft_size,n_spinor))
     allocate(ggw%WF_symm_o(fft_size,n_spinor))
     !
     call mem_est(trim(ggw%name)//"-rho_tw_rs",(/fft_size/),(/2*SP/))
     call mem_est(trim(ggw%name)//"-WF_symm_i",(/fft_size*n_spinor/),(/2*SP/))
     call mem_est(trim(ggw%name)//"-WF_symm_o",(/fft_size*n_spinor/),(/2*SP/))
     !
     if (present(NG)) then
       if (present(GRADIENT)) then
         if (GRADIENT) allocate(ggw%rhotw(3*NG))
       else
         allocate(ggw%rhotw(NG))
       endif
       ggw%ngrho=NG
       call mem_est(trim(ggw%name)//"-rhotw",(/size(ggw%rhotw)/),(/2*SP/))
     endif
     if (present(NG_GAMP)) then
       allocate(ggw%gamp(NG_GAMP(1),NG_GAMP(2)))
       call mem_est(trim(ggw%name)//"-gamp",(/size(ggw%gamp)/),(/2*SP/))
     endif
     !
   end subroutine
   !
end module collision
