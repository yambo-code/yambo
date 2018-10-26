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
 !  character(1)                  :: mode       ! "R"esonant/"A"ntiresonant
 !  complex(SP), allocatable      :: mat(:,:)
 !end type BS_block
 !
 use pars,         ONLY:cZERO,SP
 use parallel_m,   ONLY:PAR_IND_T_ordered,PP_redux_wait
 use BS,           ONLY:BS_blk,n_BS_blks,n_BS_blks,BS_K_coupling,BS_T_grp,BS_nT_grps,&
&                       BS_K_dim,BS_Block_size,BS_LiveTiming_steps,BS_H_dim
 use com,          ONLY:msg
 use interfaces,   ONLY:PARALLEL_live_message
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer    :: i_T,j_T,i_block,iB,n_new_blocks
 real(SP)   :: N_Ts_total,N_Ts_local
 !
 if (BS_K_coupling) then 
    n_new_blocks = 2
 else                    
    n_new_blocks = 1
 end if
 !
 ! Count the blocks
 n_BS_blks=count(PAR_IND_T_ordered%element_2D)
 allocate(BS_blk(n_BS_blks*n_new_blocks))
 !
 ! Allocate the blocks
 n_BS_blks=0
 do i_T=1,BS_nT_grps
   do j_T=1,BS_nT_grps
     !
     if (.not.PAR_IND_T_ordered%element_2D(i_T,j_T)) cycle
     !
     do i_block=1,n_new_blocks
        iB = n_BS_blks + i_block
        YAMBO_ALLOC(BS_blk(iB)%mat,(BS_T_grp(i_T)%size,BS_T_grp(j_T)%size))
        BS_blk(iB)%mat=cZERO
        allocate(BS_blk(iB)%done(BS_T_grp(i_T)%size,BS_T_grp(j_T)%size))
        bs_blk(ib)%done(:,:)="f"
        BS_blk(iB)%size=(/BS_T_grp(i_T)%size,BS_T_grp(j_T)%size/)
        if (i_block==1) BS_blk(iB)%mode="R"
        if (i_block==2) BS_blk(iB)%mode="C"
        BS_blk(iB)%iT_k=i_T
        BS_blk(iB)%iT_p=j_T
        BS_blk(iB)%ik=BS_T_grp(i_T)%ik
        BS_blk(iB)%ip=BS_T_grp(j_T)%ik
        BS_blk(iB)%coordinate=(/sum(BS_T_grp(:i_T-1)%size)+1,sum(BS_T_grp(:j_T-1)%size)+1/)
     enddo
     !
     n_BS_blks=n_BS_blks+n_new_blocks
     !
   enddo
 enddo
 !
 ! Live Timing Blocks
 !====================
 !
 do iB=1,n_BS_blks
   BS_LiveTiming_steps=BS_LiveTiming_steps+BS_Block_size(iB)
 enddo
 !
 N_Ts_local=real(BS_LiveTiming_steps,SP)
 N_Ts_total=real(BS_LiveTiming_steps,SP)
 !
 call PP_redux_wait(N_Ts_total)
 !
 ! REPORT
 !========
 !
 call msg('rsn','[BSK] Size (resonant)',BS_K_dim)
 call msg('rsn','[BSK]         (total)',BS_H_dim)
 !
 call PARALLEL_live_message("Kernel matrix elements",ENVIRONMENT="Response_T_space",LOADED_r=N_Ts_local,TOTAL_r=N_Ts_total)
 !
end subroutine
