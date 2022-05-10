!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, HM, DS
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
 use pars,         ONLY:cZERO,SP,IP,IPL
 use parallel_m,   ONLY:PAR_IND_T_ordered,myid,ncpu,PAR_COM_HOST
 use parallel_int, ONLY:PP_redux_wait,PARALLEL_live_message
 use BS,           ONLY:BS_blk,n_BS_blks,n_BS_blks,BS_K_coupling,BS_T_grp,BS_nT_grps,&
&                       BS_K_dim,BS_Block_size,BS_LiveTiming_steps,BS_H_dim,n_BS_blks_min,&
&                       BS_n_eh_spaces,BS_res_ares_n_mat,BS_IO_dim,l_BS_ares_from_res,BS_K_anti_resonant
 use com,          ONLY:msg
 !
#include<memory.h>
 !
 ! YAMBO_PAR_ALLOC
 !
 integer(IPL) :: HOST_SIZE(1),TMP_SIZE,LOCAL_SIZE(1)
 complex(SP), allocatable :: BS_MAT(:)
 !
 ! Work Space
 !
 integer      :: i_T,j_T,i_T_start,j_T_start,i_block,iB,i_res_ares,j_res_ares,n_new_blocks,n_BS_blks_CPUs(ncpu)
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
 do iB=1,n_BS_blks
   YAMBO_ALLOC(BS_blk(iB)%mat,(BS_blk(iB)%size(1),BS_blk(iB)%size(2)))
   allocate(BS_blk(iB)%done(BS_blk(iB)%size(1),BS_blk(iB)%size(2)))
   BS_blk(iB)%mat=cZERO
   bs_blk(iB)%done(:,:)="f"
 enddo
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
