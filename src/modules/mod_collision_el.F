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
 implicit none
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
#if defined(_CUDA)
   complex(SP), pointer, device :: rhotw_d(:)  => null()
   complex(DP), pointer, device :: rho_tw_rs_d(:) => null()
   complex(SP), pointer, device :: WF_symm_i_d(:,:) => null()
   complex(SP), pointer, device :: WF_symm_o_d(:,:) => null()
#endif
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
     logical::init_only_=.false.
     !
     if(present(INIT_ONLY)) init_only_=INIT_ONLY
     if(.not.init_only_) then
       if(associated(ggw%gamp))      deallocate(ggw%gamp)
       if(associated(ggw%rhotw))     deallocate(ggw%rhotw)
       if(associated(ggw%rho_tw_rs)) deallocate(ggw%rho_tw_rs)
       if(associated(ggw%WF_symm_i)) deallocate(ggw%WF_symm_i)
       if(associated(ggw%WF_symm_o)) deallocate(ggw%WF_symm_o)
#if defined(_CUDA)
       if(associated(ggw%rhotw_d))     deallocate(ggw%rhotw_d)
       if(associated(ggw%rho_tw_rs_d)) deallocate(ggw%rho_tw_rs_d)
       if(associated(ggw%WF_symm_i_d)) deallocate(ggw%WF_symm_i_d)
       if(associated(ggw%WF_symm_o_d)) deallocate(ggw%WF_symm_o_d)
#endif
     endif
     nullify(ggw%gamp,ggw%rhotw,ggw%rho_tw_rs,ggw%WF_symm_i,ggw%WF_symm_o)
#if defined(_CUDA)
     nullify(ggw%rhotw_d,ggw%rho_tw_rs_d,ggw%WF_symm_i_d,ggw%WF_symm_o_d)
#endif
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
#if defined _CUDA
     allocate(ggw%rho_tw_rs_d(fft_size))
     allocate(ggw%WF_symm_i_d(fft_size,n_spinor))
     allocate(ggw%WF_symm_o_d(fft_size,n_spinor))
#endif
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
#if defined _CUDA
       if (present(GRADIENT)) then
         if (GRADIENT) allocate(ggw%rhotw_d(3*NG))
       else
         allocate(ggw%rhotw_d(NG))
       endif
#endif
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
end module collision_el
