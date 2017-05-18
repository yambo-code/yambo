!
!        Copyright (C) 2000-2017 the YAMBO team
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
module collision_el
 !
 !=====================
 ! ELEMENTAL COLLISION
 !=====================
 !
 use pars,       ONLY:DP,SP,LP,schlen
 use parallel_m, ONLY:PP_indexes
 !
#include<memory.h>
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
   complex(SP), allocatable :: gamp(:,:)
   complex(SP), allocatable :: rhotw(:)
   !
   ! scatter bamp workspace
   !
   complex(DP), allocatable :: rho_tw_rs(:)
   complex(SP), allocatable :: WF_symm_i(:,:)
   complex(SP), allocatable :: WF_symm_o(:,:)
   !
 end type elemental_collision
 !
 contains
   !
   subroutine elemental_collision_copy(I,O)
     type(elemental_collision)::I,O
     O%is=I%is
     O%os=I%os
     O%qs=I%qs
   end subroutine
   !
   subroutine elemental_collision_free(ggw,INIT_ONLY)
     type(elemental_collision)::ggw
     logical,optional::INIT_ONLY
     logical         ::init_only_
     !
     init_only_=.false.
     if (present(INIT_ONLY)) then
       init_only_=INIT_ONLY
     endif
     if (.not.init_only_) then
       YAMBO_FREE(ggw%gamp)
       YAMBO_FREE(ggw%rhotw)
       YAMBO_FREE(ggw%rho_tw_rs)
       YAMBO_FREE(ggw%WF_symm_i)
       YAMBO_FREE(ggw%WF_symm_o)
     endif
   end subroutine
   !
   subroutine elemental_collision_alloc(ggw,NG,NG_GAMP,GRADIENT,TITLE)
     use electrons,      ONLY:n_spinor
     use FFT_m,          ONLY:fft_size
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
     if (fft_size>0) then
       YAMBO_ALLOC(ggw%rho_tw_rs,(fft_size))
       YAMBO_ALLOC(ggw%WF_symm_i,(fft_size,n_spinor))
       YAMBO_ALLOC(ggw%WF_symm_o,(fft_size,n_spinor))
     endif
     !
     if (present(NG)) then
       if (present(GRADIENT)) then
         YAMBO_ALLOC(ggw%rhotw,(3*NG))
       else
         YAMBO_ALLOC(ggw%rhotw,(NG))
       endif
       ggw%ngrho=NG
     endif
     if (present(NG_GAMP)) then
       YAMBO_ALLOC(ggw%gamp,(NG_GAMP(1),NG_GAMP(2)))
     endif
     !
   end subroutine
   !
end module collision_el
