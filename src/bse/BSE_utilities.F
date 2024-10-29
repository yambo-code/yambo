!
! License-Identifier: GPL
!
! Copyright (C) 2023 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS
!
! headers
!
#include<dev_defs.h>
#include<y_memory.h>
!
!
subroutine BS_exchange_oscillators_alloc(iG)
 use BS,        ONLY:BS_T_grp,BS_n_g_exch
 use gpu_m,     ONLY:have_gpu
 USE_MEMORY
 !
 implicit none
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
 USE_MEMORY
 !
 implicit none
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
 USE_MEMORY
 !
 implicit none
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
 USE_MEMORY
 !
 implicit none
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
 USE_MEMORY
 !
 implicit none
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
 USE_MEMORY
 !
 implicit none
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
 USE_MEMORY
 !
 implicit none
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
 USE_MEMORY
 !
 implicit none
 integer, intent(in) :: iG_ref
 integer :: iGR
 do iGR=iG_ref,1,-1
   YAMBO_FREE_GPU(DEV_VAR(BS_T_grp(iGR)%O_tddft_R))
   YAMBO_FREE(BS_T_grp(iGR)%O_tddft_R)
   !DEV_ACC exit data delete(BS_T_grp(iGR))
 enddo
end subroutine TDDFT_oscillators_free_R
