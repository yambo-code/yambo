!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AM HM DS
!
subroutine K_blocks( )
 !
 ! Create and Fill the BSK blocks
 !
 !type BS_block
 !  integer                       :: ik         ! ik index
 !  integer                       :: ip         ! ip index (ik -> ip transition)
 !  integer                       :: iT_k       ! Index (among n_BS_T_grps) of the cv transitions @ k
 !  integer                       :: iT_p       ! Index (among n_BS_T_grps) of the cv transitions @ p
 !  integer                       :: size(2)    ! rows x columns
 !  character(1)                  :: mode       ! "R"esonant/"C"oupling
 !  complex(SP), allocatable      :: mat(:,:)
 !end type BS_block
 !
 use pars,         ONLY:SP,IP,IPL
 use parallel_m,   ONLY:PAR_IND_T_ordered,myid,ncpu
 use parallel_int, ONLY:PP_redux_wait,PARALLEL_live_message
 use BS,           ONLY:BS_blk,n_BS_blks,n_BS_blks,BS_K_coupling,BS_T_grp,BS_nT_grps,&
&                       BS_K_dim,BS_Block_size,BS_LiveTiming_steps,BS_H_dim,n_BS_blks_min,&
&                       BS_n_eh_spaces,BS_res_ares_n_mat,BS_IO_dim,l_BS_ares_from_res
 use com,          ONLY:msg
 !
#include<parallel_memory.h>
 !
 complex(SP), allocatable :: BS_MAT(:)
 !
 ! Work Space
 !
 integer(IPL) :: TMP_SIZE
 integer      :: i_T,j_T,i_T_start,j_T_start,i_block,diag_size,iB
 integer      :: i_res_ares,j_res_ares,n_new_blocks,n_BS_blks_CPUs(ncpu)
 integer(IPL) :: local_steps
 real(SP)     :: N_Ts_total,N_Ts_local
 !
 ! Each pair of transition corresponds to a new block unless coupling is on
 ! and the anti-res block can be derived from the res. In this case
 ! only the resonant transitions are included and we need for each pair 2 blocks
 !
 n_new_blocks=1
 if (BS_K_coupling.and.l_BS_ares_from_res) n_new_blocks=2
 !
 ! Count the blocks
 n_BS_blks=0
 do j_T=1,BS_nT_grps
   do i_T=1,BS_nT_grps
     !
     i_res_ares=BS_T_grp(i_T)%i_res_ares
     j_res_ares=BS_T_grp(j_T)%i_res_ares
     if((.not.BS_K_coupling) .and. i_res_ares/=j_res_ares) cycle
     !
     if (.not.PAR_IND_T_ordered%element_2D(i_T,j_T)) cycle
     n_BS_blks=n_BS_blks+n_new_blocks
     !
   enddo
 enddo
 !
 allocate(BS_blk(n_BS_blks))
 BS_blk(:)%size(1)=0
 BS_blk(:)%size(2)=0
 !
 ! Allocate the blocks
 ! Proceed by columns since we are in fortran
 !
 n_BS_blks=0
 TMP_SIZE=0
 BS_IO_dim=0
 i_T_start=1
 j_T_start=1
 do j_T=1,BS_nT_grps
   do i_T=1,BS_nT_grps
     !
     i_res_ares=BS_T_grp(i_T)%i_res_ares
     j_res_ares=BS_T_grp(j_T)%i_res_ares
     if((.not.BS_K_coupling) .and. i_res_ares/=j_res_ares) cycle
     !
     i_T_start=BS_T_grp(i_T)%i_T_ref
     j_T_start=BS_T_grp(j_T)%i_T_ref
     !
     if (.not.PAR_IND_T_ordered%element_2D(i_T,j_T)) cycle
     !
     do i_block=1,n_new_blocks
       iB = n_BS_blks + i_block
       BS_blk(iB)%size=(/BS_T_grp(i_T)%size,BS_T_grp(j_T)%size/)
       TMP_SIZE=TMP_SIZE+int(BS_T_grp(i_T)%size*BS_T_grp(j_T)%size,IPL)
       if (l_BS_ares_from_res) then
         if (i_block==1) BS_blk(iB)%mode="R"
         if (i_block==2) BS_blk(iB)%mode="C"
       else
         if (i_res_ares==1.and.j_res_ares==1) BS_blk(iB)%mode="R"
         if (i_res_ares==1.and.j_res_ares==2) BS_blk(iB)%mode="C"
         if (i_res_ares==2.and.j_res_ares==1) BS_blk(iB)%mode="Q"
         if (i_res_ares==2.and.j_res_ares==2) BS_blk(iB)%mode="A"
       endif
#if !defined _PAR_IO
       if (BS_blk(iB)%mode=="R") BS_IO_dim(1,1)=BS_IO_dim(1,1)+BS_blk(iB)%size(1)*BS_blk(iB)%size(2)
       if (BS_blk(iB)%mode=="C") BS_IO_dim(1,2)=BS_IO_dim(1,2)+BS_blk(iB)%size(1)*BS_blk(iB)%size(2)
       if (BS_blk(iB)%mode=="A") BS_IO_dim(1,3)=BS_IO_dim(1,3)+BS_blk(iB)%size(1)*BS_blk(iB)%size(2)
#endif
       BS_blk(iB)%iT_k=i_T
       BS_blk(iB)%iT_p=j_T
       BS_blk(iB)%iT_k_st=i_T_start
       BS_blk(iB)%iT_p_st=j_T_start
       BS_blk(iB)%ik=BS_T_grp(i_T)%ik
       BS_blk(iB)%ip=BS_T_grp(j_T)%ik
       BS_blk(iB)%ira_k=i_res_ares
       BS_blk(iB)%ira_p=j_res_ares
       BS_blk(iB)%coordinate=(/sum(BS_T_grp(i_T_start:i_T-1)%size)+1,sum(BS_T_grp(j_T_start:j_T-1)%size)+1/)
     enddo
     !
     n_BS_blks=n_BS_blks+n_new_blocks
     !
   enddo
 enddo
 !
 n_BS_blks_CPUs=0
 n_BS_blks_CPUs(myid+1)=n_BS_blks
 call PP_redux_wait(n_BS_blks_CPUs)
 n_BS_blks_min=minval(n_BS_blks_CPUs)
 !
 ! Allocate the blocks
 !
 YAMBO_PAR_ALLOC_CHECK1(BS_MAT,(/TMP_SIZE/))
 !
 ! Live Timing Blocks
 !====================
 !
 local_steps=0
 do iB=1,n_BS_blks
   local_steps=local_steps+int(BS_Block_size(iB),IPL)
 enddo
 BS_LiveTiming_steps=int(local_steps,IP)
 if( int(BS_LiveTiming_steps,IPL)/=local_steps) BS_LiveTiming_steps=-1
 !
 N_Ts_local=real(local_steps,SP)
 N_Ts_total=real(local_steps,SP)
 !
 call PP_redux_wait(N_Ts_total)
 !
 ! REPORT
 !========
 !
 if(BS_n_eh_spaces==1) call msg('rsn','[BSK] Size (resonant)',BS_K_dim(1))
 if(BS_n_eh_spaces==2) call msg('rsn','[BSK] Size (res,ares)',BS_K_dim)
 !
 call msg('rsn','[BSK]         (total)',BS_H_dim)
 call msg('rsn','[BSK] N. of matrices ',BS_res_ares_n_mat)
 !
 call PARALLEL_live_message("Kernel matrix elements",ENVIRONMENT="Response_T_space",LOADED_r=N_Ts_local,TOTAL_r=N_Ts_total)
 !
end subroutine
!
!
subroutine K_block_alloc( iB, mode )
 !
 use pars,         ONLY:SP,cZERO
 use BS,           ONLY:BS_blk,l_BSE_kernel_complete
 use BS_solvers,   ONLY:BSS_kernel_IO_on_the_fly
 !
#include<y_memory.h>
 !
 integer, intent(in) :: iB
 character(*), intent(in) :: mode
 !
 integer :: diag_size
 !
 if (mode=="matonly") then
   allocate(BS_blk(iB)%mat(BS_blk(iB)%size(1),BS_blk(iB)%size(2)))
   return
 endif
 YAMBO_ALLOC(BS_blk(iB)%mat,(BS_blk(iB)%size(1),BS_blk(iB)%size(2)))
 allocate(BS_blk(iB)%done(BS_blk(iB)%size(1),BS_blk(iB)%size(2)))
 BS_blk(iB)%mat=cZERO
 BS_blk(iB)%done(:,:)="f"
 if (BS_blk(iB)%coordinate(1)==BS_blk(iB)%coordinate(2)) then
   diag_size=minval(BS_blk(iB)%size(:))
   allocate(BS_blk(iB)%table(5,diag_size))
   allocate(BS_blk(iB)%E(diag_size))
   BS_blk(iB)%E=-1._SP
   BS_blk(iB)%table=0
 endif
 !
end subroutine K_block_alloc
!
!
subroutine K_block_free( iB, mode )
 !
 use BS,           ONLY:BS_blk,l_BSE_kernel_complete
 use BS_solvers,   ONLY:BSS_kernel_IO_on_the_fly
 !
#include<y_memory.h>
 !
 integer, intent(in) :: iB
 character(*), intent(in) :: mode
 !
 if (mode=="matonly") then
   deallocate(BS_blk(iB)%mat)
   return
 endif
 YAMBO_FREE(BS_blk(iB)%mat)
 deallocate(BS_blk(iB)%done)
 if (BS_blk(iB)%coordinate(1)==BS_blk(iB)%coordinate(2)) then
   deallocate(BS_blk(iB)%table)
   deallocate(BS_blk(iB)%E)
 endif
 !
end subroutine K_block_free
!
!
subroutine K_block_symmetrize(i_B)
 use BS,           ONLY:BS_blk,l_BS_ares_from_res
 implicit none
 integer, intent(in) :: i_B
 !
 ! Symmetrize the DIAGONAL blocks only (i_Tk=i_Tp)
 !
 integer :: i_Tk,i_Tp,i_r,i_c
 !
 i_Tk  =BS_blk(i_B)%iT_k
 i_Tp  =BS_blk(i_B)%iT_p
 !
 if (i_Tk/=i_Tp) return
 !
 select case(BS_blk(i_B)%mode)
 case("R","A")
   ! Resonant ("R") and anti-resonant ("A") parts are hermitian
   do i_r=1,BS_blk(i_B)%size(1)
     BS_blk(i_B)%mat(i_r,i_r) = real(BS_blk(i_B)%mat(i_r,i_r))
     do i_c=i_r+1,BS_blk(i_B)%size(2)
       BS_blk(i_B)%mat(i_c,i_r) = conjg(BS_blk(i_B)%mat(i_r,i_c))
     enddo
   enddo
 case("C","Q")
   if (.not.l_BS_ares_from_res) return
   ! Coupling ("C") and anti-coupling ("Q") are symmetric if l_BS_ares_from_res=.TRUE.
   do i_r=1,BS_blk(i_B)%size(1)
     !This gives fails with test-suite test Benzene 06_tddft_cpl
     !BS_blk(i_B)%mat(i_r,i_r) = real(BS_blk(i_B)%mat(i_r,i_r))
     do i_c=i_r+1,BS_blk(i_B)%size(2)
       BS_blk(i_B)%mat(i_c,i_r) = BS_blk(i_B)%mat(i_r,i_c)
     enddo
   enddo
 end select
 !
end subroutine K_block_symmetrize
!
!
subroutine BS_blks_free(i_BS_mat)
 use BS,           ONLY:BS_blk,n_BS_blks
 implicit none
 integer, intent(in) :: i_BS_mat
 integer :: iB
 do iB=1,n_BS_blks
   if(i_BS_mat/=BS_blk(iB)%ira_k) cycle
   if(.not.allocated(BS_blk(iB)%mat)) cycle
   call K_block_free( iB, "all" )
 enddo
end subroutine
!
!
subroutine BS_blks_symmetrize_K()
 use BS,           ONLY:BS_blk,n_BS_blks
 implicit none
 integer :: i_B
 do i_B=1,n_BS_blks
   call K_block_symmetrize(i_B)
 enddo
end subroutine BS_blks_symmetrize_K

