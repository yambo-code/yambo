!
! License-Identifier: GPL
!
! Copyright (C) 2023 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS
!
#include<dev_defs.h>
!
subroutine build_inverse_eh_table(nk)
 !
 use electrons,       ONLY:n_sp_pol
 use BS,              ONLY:BS_H_dim,BS_bands
 use BS_solvers,      ONLY:BSS_eh_table,BSS_eh_table_m1
 !
 implicit none
 !
 integer, intent(in) :: nk
 !
 integer :: iv,ic,i_H,ik_bz
 !
 if (.not.allocated(BSS_eh_table_m1)) then
   allocate(BSS_eh_table_m1(nk,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2),n_sp_pol,n_sp_pol))
   BSS_eh_table_m1=0
 endif 
 !call live_timing('BSE inverse eh-table',BS_H_dim)
 !
 do i_H=1,BS_H_dim
   !
   ik_bz = BSS_eh_table(i_H,1)
   iv    = BSS_eh_table(i_H,2)
   ic    = BSS_eh_table(i_H,3)
   !
   BSS_eh_table_m1(ik_bz,iv,ic,1,1)=i_H
   !call live_timing(steps=1)
   !
 enddo
 !call live_timing( )
 !
end subroutine build_inverse_eh_table
!
subroutine BS_exchange_oscillators_alloc(iG)
 use BS,        ONLY:BS_T_grp,BS_n_g_exch
 use gpu_m,     ONLY:have_gpu
#include<y_memory.h>
 integer, intent(in) :: iG
 if (BS_T_grp(iG)%size==0) return
 !DEV_ACC enter data copyin(BS_T_grp(iG))
 YAMBO_ALLOC(BS_T_grp(iG)%O_x,(BS_n_g_exch,BS_T_grp(iG)%size))
 if (have_gpu) then
   YAMBO_ALLOC_GPU(DEV_VAR(BS_T_grp(iG)%O_x),(BS_n_g_exch,BS_T_grp(iG)%size))
 endif   
end subroutine BS_exchange_oscillators_alloc
!
subroutine BS_correlation_oscillators_alloc(iB)
 use BS,        ONLY:BS_blk,O_ng
 use gpu_m,     ONLY:have_gpu
#include<y_memory.h>
 integer, intent(in) :: iB
 if (BS_blk(iB)%N_oscillators==0) return
 !DEV_ACC enter data copyin(BS_blk(iB))
 YAMBO_ALLOC(BS_blk(iB)%O_c,(O_ng,BS_blk(iB)%N_oscillators))
 if (have_gpu) then
   YAMBO_ALLOC_GPU(DEV_VAR(BS_blk(iB)%O_c),(O_ng,BS_blk(iB)%N_oscillators))
 endif   
end subroutine BS_correlation_oscillators_alloc
!
subroutine BS_exchange_oscillators_free(iG_ref)
 use BS,        ONLY:BS_T_grp
#include<y_memory.h>
 integer, intent(in) :: iG_ref
 integer :: iG
 do iG=iG_ref,1,-1
   YAMBO_FREE_GPU(DEV_VAR(BS_T_grp(iG)%O_x))
   YAMBO_FREE(BS_T_grp(iG)%O_x)
   !DEV_ACC exit data delete(BS_T_grp(iG))
 enddo
end subroutine BS_exchange_oscillators_free
!
subroutine BS_correlation_oscillators_free(iB_ref,l_std_alg)
 use BS,        ONLY:BS_T_grp,l_BSE_minimize_memory,BS_blk,n_BS_blks
#include<y_memory.h>
 integer, intent(in) :: iB_ref
 logical, intent(in) :: l_std_alg
 integer :: iB,ik_loop,ip_loop,ik_now,ip_now
 ik_now=BS_blk(iB_ref)%ik
 ip_now=BS_blk(iB_ref)%ip
 if(iB_ref==n_BS_blks) then
   ik_now=0
   ip_now=0
 endif
 do iB=iB_ref,1,-1
   ik_loop=BS_blk(iB)%ik
   ip_loop=BS_blk(iB)%ip
   if ( ik_now==ik_loop .and. ip_now==ip_loop .and. &
   &   .not.(l_BSE_minimize_memory.or.l_std_alg)) cycle
   if (.not.allocated(BS_blk(iB)%O_c)) exit
   YAMBO_FREE_GPU(DEV_VAR(BS_blk(iB)%O_c))
   YAMBO_FREE(BS_blk(iB)%O_c)
   YAMBO_FREE(BS_blk(iB)%kp_table)
   ! O_table has rank>7, there is not interface for dev_allocated function
   YAMBO_FREE_NO_DEV_CHECK(BS_blk(iB)%O_table)
   !DEV_ACC exit data delete(BS_blk(iB))
 enddo
end subroutine BS_correlation_oscillators_free
!
subroutine TDDFT_oscillators_alloc_L(iGL)
 use electrons, ONLY:n_spin,n_spinor
 use BS,        ONLY:BS_T_grp,BS_n_g_fxc
 use gpu_m,     ONLY:have_gpu
#include<y_memory.h>
 integer, intent(in) :: iGL
 if (BS_T_grp(iGL)%size>0) then
   !DEV_ACC enter data copyin(BS_T_grp(iGL))
   YAMBO_ALLOC(BS_T_grp(iGL)%O_tddft_L,(BS_n_g_fxc,BS_T_grp(iGL)%size,n_spinor,n_spinor))
   if (have_gpu) then
     YAMBO_ALLOC_GPU(DEV_VAR(BS_T_grp(iGL)%O_tddft_L),(BS_n_g_fxc,BS_T_grp(iGL)%size,n_spinor,n_spinor))
   endif
 endif
end subroutine TDDFT_oscillators_alloc_L
!
subroutine TDDFT_oscillators_alloc_R(iGR)
 use electrons, ONLY:n_spin,n_spinor
 use BS,        ONLY:BS_T_grp,BS_n_g_fxc
 use gpu_m,     ONLY:have_gpu
#include<y_memory.h>
 integer, intent(in) :: iGR
 if (BS_T_grp(iGR)%size>0) then
   !DEV_ACC enter data copyin(BS_T_grp(iGR))
   YAMBO_ALLOC(BS_T_grp(iGR)%O_tddft_R,(BS_n_g_fxc,BS_T_grp(iGR)%size,n_spin,n_spin))
   if (have_gpu) then
      YAMBO_ALLOC_GPU(DEV_VAR(BS_T_grp(iGR)%O_tddft_R),(BS_n_g_fxc,BS_T_grp(iGR)%size,n_spin,n_spin))
   endif
 endif
end subroutine TDDFT_oscillators_alloc_R
!
subroutine TDDFT_oscillators_free_L(iG_ref)
 use BS,        ONLY:BS_T_grp
#include<y_memory.h>
 integer, intent(in) :: iG_ref
 integer :: iGL
 do iGL=iG_ref,1,-1
   YAMBO_FREE_GPU(DEV_VAR(BS_T_grp(iGL)%O_tddft_L))
   YAMBO_FREE(BS_T_grp(iGL)%O_tddft_L)
   !DEV_ACC exit data delete(BS_T_grp(iGL))
 enddo
end subroutine TDDFT_oscillators_free_L
!
subroutine TDDFT_oscillators_free_R(iG_ref)
 use BS,        ONLY:BS_T_grp
#include<y_memory.h>
 integer, intent(in) :: iG_ref
 integer :: iGR
 do iGR=iG_ref,1,-1
   YAMBO_FREE_GPU(DEV_VAR(BS_T_grp(iGR)%O_tddft_R))
   YAMBO_FREE(BS_T_grp(iGR)%O_tddft_R)
   !DEV_ACC exit data delete(BS_T_grp(iGR))
 enddo
end subroutine TDDFT_oscillators_free_R
