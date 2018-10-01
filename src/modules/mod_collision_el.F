!
!        Copyright (C) 2000-2018 the YAMBO team
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
 use pars,        ONLY:DP,SP,LP,schlen
 use parallel_m,  ONLY:PP_indexes
 use qe_pseudo_m, ONLY:bec_type,deallocate_bec_type
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
   ! USPP workspace
   !
   type(bec_type) :: ibec      ! contains:  < beta_j | wf_in>
   type(bec_type) :: obec      ! contains:  < beta_j | wf_om>
   real(DP),    allocatable :: ylmk0(:,:)
   real(DP),    allocatable :: qmod(:)
   complex(DP), allocatable :: qgm(:,:,:,:)
   complex(SP), allocatable :: becprod(:,:,:,:)
   complex(SP), allocatable :: eiqtau(:)
   !
#if defined _CUDA
   complex(SP), allocatable, device :: rhotw_d(:)
   complex(SP), allocatable, device :: gamp_d(:,:)
   complex(DP), allocatable, device :: rho_tw_rs_d(:)
   complex(SP), allocatable, device :: WF_symm_i_d(:,:)
   complex(SP), allocatable, device :: WF_symm_o_d(:,:)
#endif
   !
 end type elemental_collision
 !
 contains
   !
   subroutine elemental_collision_copy(I,O)
     type(elemental_collision), target::I,O
     O%is=I%is
     O%os=I%os
     O%qs=I%qs
   end subroutine
   !
   subroutine elemental_collision_free(ggw)
     type(elemental_collision), target::ggw
     !
     YAMBO_FREE(ggw%gamp)
     YAMBO_FREE(ggw%rhotw)
     YAMBO_FREE(ggw%rho_tw_rs)
     YAMBO_FREE(ggw%WF_symm_i)
     YAMBO_FREE(ggw%WF_symm_o)
     ! 
#if defined _CUDA
     if(allocated(ggw%rhotw_d))     deallocate(ggw%rhotw_d)
     if(allocated(ggw%gamp_d))      deallocate(ggw%gamp_d)
     if(allocated(ggw%rho_tw_rs_d)) deallocate(ggw%rho_tw_rs_d)
     if(allocated(ggw%WF_symm_i_d)) deallocate(ggw%WF_symm_i_d)
     if(allocated(ggw%WF_symm_o_d)) deallocate(ggw%WF_symm_o_d)
#endif
     !
     ! USPP workspace
     call deallocate_bec_type(ggw%ibec)
     call deallocate_bec_type(ggw%obec)
     YAMBO_FREE(ggw%ylmk0)
     YAMBO_FREE(ggw%qmod)
     YAMBO_FREE(ggw%qgm)
     YAMBO_FREE(ggw%becprod)
     YAMBO_FREE(ggw%eiqtau)
     !
   end subroutine
   !
   subroutine elemental_collision_alloc(ggw,NG,NG_GAMP,GRADIENT,TITLE)
     use electrons,      ONLY:n_spinor
     use FFT_m,          ONLY:fft_size
     type(elemental_collision), target ::ggw
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
#if defined _CUDA
       allocate(ggw%rho_tw_rs_d(fft_size))
       allocate(ggw%WF_symm_i_d(fft_size,n_spinor))
       allocate(ggw%WF_symm_o_d(fft_size,n_spinor))
#endif
     endif
     !
     if (present(NG)) then
       if (NG>0) then
         if (present(GRADIENT)) then
           YAMBO_ALLOC(ggw%rhotw,(3*NG))
         else
           YAMBO_ALLOC(ggw%rhotw,(NG))
         endif
       endif
#if defined _CUDA
       if (present(GRADIENT)) then
         if (GRADIENT) allocate(ggw%rhotw_d(3*NG))
       else
         allocate(ggw%rhotw_d(NG))
       endif
#endif
       ggw%ngrho=NG
     endif
     if (present(NG_GAMP)) then
       if (all((/NG_GAMP>0/))) then
         YAMBO_ALLOC(ggw%gamp,(NG_GAMP(1),NG_GAMP(2)))
       endif
     endif
     !
   end subroutine
   !
end module collision_el
