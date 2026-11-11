!
! License-Identifier: GPL
!
! Copyright (C) 2026 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
! headers
!
#include<dev_defs.h>
!
! GPU only. Matrix elements of the BSE kernel built from the oscillators
! for a whole block of transitions (i_Tk,i_Tp), computed with a single GEMM
! instead of one device reduction per matrix element.
!
! mode "R","F" : resonant/anti-resonant, mode "C": coupling
!
subroutine K_exchange_batched(iq,mode,BS_n_g,BS_T_grp_k,BS_T_grp_p,H_x_batched)
 !
 ! Same quantity as K_exchange_kernel[_coupling], for all the (i_Tk,i_Tp) of the block:
 !
 !  resonant: H_x_batched(i_Tk,i_Tp) = sum_G conj(O_p(G)) O_k(G) / q(G)^2
 !  coupling: H_x_batched(i_Tk,i_Tp) = sum_G O_p(-G)      O_k(G) / q(G)^2
 !
 use pars,           ONLY:SP,cZERO,cONE
 use BS,             ONLY:BS_T_group,l_BS_magnons
 use electrons,      ONLY:n_sp_pol
 use R_lattice,      ONLY:bare_qpg,DEV_VAR(bare_qpg),minus_G,DEV_VAR(minus_G)
 use devxlib,        ONLY:devxlib_xGEMM_gpu,devxlib_memcpy_d2h
#include<y_memory.h>
 !
 !
 integer,                  intent(in)  :: iq,BS_n_g
 character(*),             intent(in)  :: mode
 type(BS_T_group), target, intent(in)  :: BS_T_grp_k,BS_T_grp_p
 complex(SP),              intent(out) :: H_x_batched(BS_T_grp_k%size,BS_T_grp_p%size)
 !
 ! Work Space
 !
 integer  :: nTk,nTp,ig,iT
 complex(SP), allocatable DEV_ATTR :: S_g(:,:)
 complex(SP), allocatable DEV_ATTR :: Pm_g(:,:)
 complex(SP), allocatable DEV_ATTR :: H_x_g(:,:)
 complex(SP), pointer     DEV_ATTR :: Ok_p(:,:)
 complex(SP), pointer     DEV_ATTR :: Op_p(:,:)
 !
 nTk=BS_T_grp_k%size
 nTp=BS_T_grp_p%size
 !
 H_x_batched=cZERO
 !
 if (l_BS_magnons.and.n_sp_pol==2) return
 !
 YAMBO_ALLOC_GPU(H_x_g,(nTk,nTp))
 YAMBO_ALLOC_GPU(S_g,(BS_n_g,nTk))
 !
 Ok_p => DEV_VAR(BS_T_grp_k%O_x)
 Op_p => DEV_VAR(BS_T_grp_p%O_x)
 !
 ! S(G,k)=O_k(G)/q(G)^2
 !DEV_ACC_DEBUG data present(S_g,Ok_p,bare_qpg)
 !DEV_ACC parallel loop collapse(2)
 !DEV_CUF kernel do(2) <<<*,*>>>
 !DEV_OMPGPU target map(present,alloc:S_g,Ok_p,bare_qpg)
 !DEV_OMPGPU teams loop collapse(2)
 do iT=1,nTk
   do ig=1,BS_n_g
     S_g(ig,iT)=Ok_p(ig,iT)/DEV_VAR(bare_qpg)(iq,ig)**2
   enddo
 enddo
 !DEV_OMPGPU end target
 !DEV_ACC_DEBUG end data
 !
 if (mode/="C") then
   !
   ! Hx = conjg( S^H O_p )
   call devxlib_xGEMM_gpu('C','N',nTk,nTp,BS_n_g,cONE,S_g,BS_n_g,Op_p,size(Op_p,1),cZERO,H_x_g,nTk)
   !
   !DEV_ACC_DEBUG data present(H_x_g)
   !DEV_ACC parallel loop collapse(2)
   !DEV_CUF kernel do(2) <<<*,*>>>
   !DEV_OMPGPU target map(present,alloc:H_x_g)
   !DEV_OMPGPU teams loop collapse(2)
   do iT=1,nTp
     do ig=1,nTk
       H_x_g(ig,iT)=conjg(H_x_g(ig,iT))
     enddo
   enddo
   !DEV_OMPGPU end target
   !DEV_ACC_DEBUG end data
   !
 else
   !
   ! Hx = S^T O_p(-G)
   YAMBO_ALLOC_GPU(Pm_g,(BS_n_g,nTp))
   !DEV_ACC_DEBUG data present(Pm_g,Op_p,minus_G)
   !DEV_ACC parallel loop collapse(2)
   !DEV_CUF kernel do(2) <<<*,*>>>
   !DEV_OMPGPU target map(present,alloc:Pm_g,Op_p,minus_G)
   !DEV_OMPGPU teams loop collapse(2)
   do iT=1,nTp
     do ig=1,BS_n_g
       Pm_g(ig,iT)=Op_p(DEV_VAR(minus_G)(ig),iT)
     enddo
   enddo
   !DEV_OMPGPU end target
   !DEV_ACC_DEBUG end data
   call devxlib_xGEMM_gpu('T','N',nTk,nTp,BS_n_g,cONE,S_g,BS_n_g,Pm_g,BS_n_g,cZERO,H_x_g,nTk)
   YAMBO_FREE_GPU(Pm_g)
   YAMBO_FREE(Pm_g)
   !
 endif
 !
 call devxlib_memcpy_d2h(H_x_batched,H_x_g)
 !
 YAMBO_FREE_GPU(S_g)
 YAMBO_FREE(S_g)
 YAMBO_FREE_GPU(H_x_g)
 YAMBO_FREE(H_x_g)
 !
end subroutine K_exchange_batched
!
!
!
!
subroutine K_tddft_batched(mode,BS_n_g,BS_T_grp_k,BS_T_grp_p,H_tddft_batched)
 !
 ! Same quantity as TDDFT_ALDA_eh_space_G_kernel[_coupling], for all the (i_Tk,i_Tp) of the block
 ! (G-space ALDA kernel), summed over the spinor components when n_spinor==2:
 !
 !  resonant: H_tddft_batched(i_Tk,i_Tp) =       sum_G conj(O_L(G)) O_R(G) / 4pi
 !  coupling: H_tddft_batched(i_Tk,i_Tp) = conjg(sum_G O_L(-G)      O_R(G)) / 4pi
 !
 ! Not implemented for n_sp_pol==2 (the caller must not call it in that case).
 !
 use pars,           ONLY:SP,pi,cZERO,cONE
 use BS,             ONLY:BS_T_group
 use electrons,      ONLY:n_spinor,n_sp_pol
 use R_lattice,      ONLY:minus_G,DEV_VAR(minus_G)
 use devxlib,        ONLY:devxlib_xGEMM_gpu,devxlib_memcpy_d2h
#include<y_memory.h>
 !
 !
 integer,                  intent(in)  :: BS_n_g
 character(*),             intent(in)  :: mode
 type(BS_T_group), target, intent(in)  :: BS_T_grp_k,BS_T_grp_p
 complex(SP),              intent(out) :: H_tddft_batched(BS_T_grp_k%size,BS_T_grp_p%size)
 !
 ! Work Space
 !
 integer  :: nTk,nTp,ig,iT,i_sp,o_sp
 logical  :: l_coupling
 complex(SP), allocatable DEV_ATTR :: Pm_g(:,:)
 complex(SP), allocatable DEV_ATTR :: H_g(:,:)
 complex(SP), pointer     DEV_ATTR :: Ok_p(:,:)
 complex(SP), pointer     DEV_ATTR :: Op_p(:,:)
 !
 if (n_sp_pol==2) call error(" K_tddft_batched: spin polarized ALDA not implemented")
 !
 nTk=BS_T_grp_k%size
 nTp=BS_T_grp_p%size
 l_coupling=(mode=="C")
 !
 YAMBO_ALLOC_GPU(H_g,(nTk,nTp))
 if (l_coupling) then
   YAMBO_ALLOC_GPU(Pm_g,(BS_n_g,nTk))
 endif
 !
 do i_sp=1,n_spinor
   do o_sp=1,n_spinor
     !
     Ok_p => DEV_VAR(BS_T_grp_k%O_tddft_L)(:,:,i_sp,o_sp)
     Op_p => DEV_VAR(BS_T_grp_p%O_tddft_R)(:,:,i_sp,o_sp)
     !
     if (.not.l_coupling) then
       !
       if (i_sp==1.and.o_sp==1) then
         call devxlib_xGEMM_gpu('C','N',nTk,nTp,BS_n_g,cONE,Ok_p,size(Ok_p,1),Op_p,size(Op_p,1),cZERO,H_g,nTk)
       else
         call devxlib_xGEMM_gpu('C','N',nTk,nTp,BS_n_g,cONE,Ok_p,size(Ok_p,1),Op_p,size(Op_p,1),cONE ,H_g,nTk)
       endif
       !
     else
       !
       !DEV_ACC_DEBUG data present(Pm_g,Ok_p,minus_G)
       !DEV_ACC parallel loop collapse(2)
       !DEV_CUF kernel do(2) <<<*,*>>>
       !DEV_OMPGPU target map(present,alloc:Pm_g,Ok_p,minus_G)
       !DEV_OMPGPU teams loop collapse(2)
       do iT=1,nTk
         do ig=1,BS_n_g
           Pm_g(ig,iT)=Ok_p(DEV_VAR(minus_G)(ig),iT)
         enddo
       enddo
       !DEV_OMPGPU end target
       !DEV_ACC_DEBUG end data
       !
       if (i_sp==1.and.o_sp==1) then
         call devxlib_xGEMM_gpu('T','N',nTk,nTp,BS_n_g,cONE,Pm_g,BS_n_g,Op_p,size(Op_p,1),cZERO,H_g,nTk)
       else
         call devxlib_xGEMM_gpu('T','N',nTk,nTp,BS_n_g,cONE,Pm_g,BS_n_g,Op_p,size(Op_p,1),cONE ,H_g,nTk)
       endif
       !
     endif
     !
   enddo
 enddo
 !
 if (l_coupling) then
   YAMBO_FREE_GPU(Pm_g)
   YAMBO_FREE(Pm_g)
 endif
 !
 ! conjg (coupling only) and 1/4pi
 !DEV_ACC_DEBUG data present(H_g)
 !DEV_ACC parallel loop collapse(2)
 !DEV_CUF kernel do(2) <<<*,*>>>
 !DEV_OMPGPU target map(present,alloc:H_g)
 !DEV_OMPGPU teams loop collapse(2)
 do iT=1,nTp
   do ig=1,nTk
     if (l_coupling) H_g(ig,iT)=conjg(H_g(ig,iT))/4._SP/pi
     if (.not.l_coupling) H_g(ig,iT)=H_g(ig,iT)/4._SP/pi
   enddo
 enddo
 !DEV_OMPGPU end target
 !DEV_ACC_DEBUG end data
 !
 call devxlib_memcpy_d2h(H_tddft_batched,H_g)
 !
 YAMBO_FREE_GPU(H_g)
 YAMBO_FREE(H_g)
 !
end subroutine K_tddft_batched
!
!
!
!
subroutine K_block_assemble(i_block,i_Tgrp_k,i_Tgrp_p,H_pos_start,coef,mode_now,l_matrix_init,l_write_kernel,H_x_block)
 !
 ! Assembles the kernel block i_block from the matrix elements H_x_block(i_Tk,i_Tp) of the
 ! exchange, TDDFT or correlation kernel (computed for the whole block by K_exchange_batched,
 ! K_tddft_batched or K_correlation_batched):
 !
 !  BS_blk(i_block)%mat(i_Tk,i_Tp) = sqrt(f_k) * H_x * coef * sqrt(f_p)          (times i for the coupling)
 !
 ! with coef=Co for exchange and TDDFT, and coef=-1 (or -WF_exx_fraction) for correlation.
 !
 ! added to the previous value of the matrix if .not.l_matrix_init.
 ! It does what the transitions loop of K_kernel does for these kernels, but
 ! the conditions on the elements are evaluated once per column and not per element.
 !
 use pars,           ONLY:SP,cZERO,cI
 use BS,             ONLY:BS_blk,BS_T_grp,l_BSE_restart
 !
 implicit none
 !
 integer,      intent(in) :: i_block,i_Tgrp_k,i_Tgrp_p,H_pos_start(2)
 complex(SP),  intent(in) :: coef
 character(1), intent(in) :: mode_now
 logical,      intent(in) :: l_matrix_init,l_write_kernel
 complex(SP),  intent(in) :: H_x_block(BS_T_grp(i_Tgrp_k)%size,BS_T_grp(i_Tgrp_p)%size)
 !
 ! Work Space
 !
 integer     :: nTk,nTp,i_Tk,i_Tp,i_Tk_max
 complex(SP) :: sqrt_f_itp,mat_tmp
 complex(SP), allocatable :: sqrt_f_k(:)
 !
 nTk=BS_T_grp(i_Tgrp_k)%size
 nTp=BS_T_grp(i_Tgrp_p)%size
 !
 allocate(sqrt_f_k(nTk))
 do i_Tk=1,nTk
   sqrt_f_k(i_Tk)=sqrt(cmplx(BS_T_grp(i_Tgrp_k)%f(i_Tk),kind=SP))
 enddo
 !
 do i_Tp=1,nTp
   !
   sqrt_f_itp=sqrt(cmplx(BS_T_grp(i_Tgrp_p)%f(i_Tp),kind=SP))
   !
   ! The elements below the diagonal of the whole matrix, H_pos(1)>H_pos(2), are not computed,
   ! with H_pos=H_pos_start+(i_Tk,i_Tp)
   !
   i_Tk_max=min(nTk,i_Tp+H_pos_start(2)-H_pos_start(1))
   !
   do i_Tk=1,i_Tk_max
     !
     if (l_BSE_restart) then
       if (BS_blk(i_block)%done(i_Tk,i_Tp)=="t") cycle
     endif
     !
     mat_tmp=sqrt_f_k(i_Tk)*(H_x_block(i_Tk,i_Tp)*coef-cZERO)*sqrt_f_itp
     if (mode_now=="C") mat_tmp=cI*mat_tmp
     !
     if (     l_matrix_init) BS_blk(i_block)%mat(i_Tk,i_Tp) = mat_tmp
     if (.not.l_matrix_init) BS_blk(i_block)%mat(i_Tk,i_Tp) = BS_blk(i_block)%mat(i_Tk,i_Tp)+mat_tmp
     !
     if (l_write_kernel) BS_blk(i_block)%done(i_Tk,i_Tp)="t"
     !
   enddo
   !
 enddo
 !
 deallocate(sqrt_f_k)
 !
end subroutine K_block_assemble
