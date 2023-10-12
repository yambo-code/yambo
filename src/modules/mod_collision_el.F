!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
 use cuda_m,      ONLY:have_cuda
 !
#include<dev_defs.h>
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
   integer :: gamp_nrows
   integer :: gamp_ncols
   integer :: gamp_rows(2)
   integer :: gamp_cols(2)
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
   complex(SP), allocatable DEV_ATTR :: rhotw_d(:)
   complex(SP), allocatable DEV_ATTR :: gamp_d(:,:)
   complex(DP), allocatable DEV_ATTR :: rho_tw_rs_d(:)
   complex(SP), allocatable DEV_ATTR :: WF_symm_i_d(:,:)
   complex(SP), allocatable DEV_ATTR :: WF_symm_o_d(:,:)
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
     YAMBO_FREE(ggw%gamp_d)
     YAMBO_FREE(ggw%rhotw_d)
     !
     YAMBO_FREE(ggw%rho_tw_rs_d)
     YAMBO_FREE(ggw%WF_symm_i_d)
     YAMBO_FREE(ggw%WF_symm_o_d)
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
   subroutine elemental_collision_alloc(ggw,NG,GAMP_NG,GAMP_ROWS,GAMP_COLS,&
&                                       CPU_ONLY,GRADIENT,TITLE)
     use electrons,      ONLY:n_spinor
     use FFT_m,          ONLY:fft_size
     type(elemental_collision), target ::ggw
     integer,         OPTIONAL :: NG
     integer,         OPTIONAL :: GAMP_NG(2)
     integer,         OPTIONAL :: GAMP_ROWS(2)
     integer,         OPTIONAL :: GAMP_COLS(2)
     logical,         OPTIONAL :: CPU_ONLY
     logical,         OPTIONAL :: GRADIENT
     character(*),    OPTIONAL :: TITLE
     !
     integer :: RHOSIZE
     logical :: CPU_ONLY_
     !
     if (present(TITLE)) then
       ggw%name=TITLE
     endif
     !
     CPU_ONLY_=.false.
     if (present(CPU_ONLY)) CPU_ONLY_=CPU_ONLY
     !
     ! workspace
     !
     if (fft_size>0.and..not.allocated(ggw%rho_tw_rs)) then
       YAMBO_ALLOC(ggw%rho_tw_rs,(fft_size))
       YAMBO_ALLOC(ggw%WF_symm_i,(fft_size,n_spinor))
       YAMBO_ALLOC(ggw%WF_symm_o,(fft_size,n_spinor))
       !
       if (have_cuda.and..not.CPU_ONLY_) then
         YAMBO_ALLOC(ggw%rho_tw_rs_d,(fft_size))
         YAMBO_ALLOC(ggw%WF_symm_i_d,(fft_size,n_spinor))
         YAMBO_ALLOC(ggw%WF_symm_o_d,(fft_size,n_spinor))
       endif
     endif
     !
     ! rhotw
     !
     if (present(NG)) then
       if (NG>0) then
         !
         if (     present(GRADIENT)) RHOSIZE=3*NG
         if (.not.present(GRADIENT)) RHOSIZE=1*NG
         YAMBO_ALLOC(ggw%rhotw,(RHOSIZE))
         !
         if (have_cuda.and..not.CPU_ONLY_) then
           YAMBO_ALLOC(ggw%rhotw_d,(RHOSIZE))
         endif
         !
       endif
       ggw%ngrho=NG
     endif
     !
     ! GAMP
     !
     if (present(GAMP_NG)) then
       if (all((/GAMP_NG>0/))) then
         YAMBO_ALLOC(ggw%gamp,(GAMP_NG(1),GAMP_NG(2)))
       endif
     endif
     if (present(GAMP_ROWS).and.present(GAMP_COLS)) then
       if (present(GAMP_NG)) call error("[COLL] GAMP_NG should not be present")
       YAMBO_ALLOC(ggw%gamp,(GAMP_ROWS(1):GAMP_ROWS(2),GAMP_COLS(1):GAMP_COLS(2)))
     endif
     if (have_cuda.and.allocated(ggw%gamp).and..not.CPU_ONLY_) then
       YAMBO_ALLOC_MOLD(ggw%gamp_d,ggw%gamp)
     endif
     if (allocated(ggw%gamp)) then
       ggw%gamp_rows(1)=lbound(ggw%gamp,1)
       ggw%gamp_rows(2)=ubound(ggw%gamp,1)
       ggw%gamp_cols(1)=lbound(ggw%gamp,2)
       ggw%gamp_cols(2)=ubound(ggw%gamp,2)
       ggw%gamp_nrows=ggw%gamp_rows(2)-ggw%gamp_rows(1)+1
       ggw%gamp_ncols=ggw%gamp_cols(2)-ggw%gamp_cols(1)+1
     endif
     !
   end subroutine
   !
end module collision_el
