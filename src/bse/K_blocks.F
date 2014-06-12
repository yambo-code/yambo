!
!        Copyright (C) 2000-2014 the YAMBO team
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
 ! Create and Fille the BSK blocks
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
 use memory_m,     ONLY:mem_est
 use parallel_m,   ONLY:PAR_IND_T,PAR_BS_nT_grps,PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X,PAR_COM_eh_INDEX
 use interfaces,   ONLY:PARALLEL_live_message
 use BS,           ONLY:BS_blk,n_BS_blks,n_BS_blks,BS_K_coupling,BS_T_grp,BS_nT_grps,BS_K_dim,BS_bands
 use com,          ONLY:msg
 !
 implicit none
 !
 ! Work Space
 !
 integer              :: i_T,j_T,i_loop,n_tot_blocks,tot_size
 !
 call section('=','CPU-dependent Block structure')
 !
 do i_loop=1,2
   !
   tot_size=0
   !
   if (i_loop==2) then
     allocate(BS_blk(n_BS_blks))
   endif
   !
   n_BS_blks=0
   !
   do i_T=1,PAR_BS_nT_grps
     !
     do j_T=1,PAR_BS_nT_grps
       !
       if (.not.PAR_IND_T%element_2D(i_T,j_T)) cycle 
       !
       if (BS_T_grp(i_T)%ik<=BS_T_grp(j_T)%ik) then
         !
         n_BS_blks=n_BS_blks+1
         !
         if (i_loop==2) then
           allocate(BS_blk(n_BS_blks)%mat(BS_T_grp(i_T)%size,BS_T_grp(j_T)%size))
           tot_size=tot_size+BS_T_grp(i_T)%size*BS_T_grp(j_T)%size
           BS_blk(n_BS_blks)%mode="R"
           BS_blk(n_BS_blks)%iT_k=i_T
           BS_blk(n_BS_blks)%iT_p=j_T
           BS_blk(n_BS_blks)%ik=BS_T_grp(i_T)%ik
           BS_blk(n_BS_blks)%ip=BS_T_grp(j_T)%ik
         endif
         !
       endif
       !
       if (BS_K_coupling) then
         n_BS_blks=n_BS_blks+1
         if (i_loop==2) then
           allocate(BS_blk(n_BS_blks)%mat(BS_T_grp(i_T)%size,BS_T_grp(j_T)%size))
           tot_size=tot_size+BS_T_grp(i_T)%size*BS_T_grp(j_T)%size
           BS_blk(n_BS_blks)%mode="A"
           BS_blk(n_BS_blks)%iT_k=i_T
           BS_blk(n_BS_blks)%iT_p=j_T
           BS_blk(n_BS_blks)%ik=BS_T_grp(i_T)%ik
           BS_blk(n_BS_blks)%ip=BS_T_grp(j_T)%ik
         endif
       endif
       !
     enddo
     !
   enddo
   !
 enddo
 !
 n_tot_blocks=(BS_nT_grps**2-BS_nT_grps)/2+BS_nT_grps
 if (BS_K_coupling) n_tot_blocks=n_tot_blocks+BS_nT_grps**2
 !
 call PARALLEL_live_message("BSK blocks",LOADED=n_BS_blks,TOTAL=n_tot_blocks)
 !
 call PARALLEL_live_message("BSK CON bands",&
&                            LOADED=PAR_IND_CON_BANDS_X(5)%n_of_elements(PAR_COM_eh_INDEX%n_CPU+1),&
&                            TOTAL=BS_bands(2)-BS_bands(1)+1)
 !
 call PARALLEL_live_message("BSK VAL bands",&
&                            LOADED=PAR_IND_VAL_BANDS_X(5)%n_of_elements(PAR_COM_eh_INDEX%n_CPU+1),&
&                            TOTAL=BS_bands(2)-BS_bands(1)+1)
 !
 call mem_est("BSK",(/tot_size/))
 !
 ! REPORT
 !
 call msg('r','[BSK] Size   (total)',BS_K_dim)
 call msg('r','[BSK] Blocks (total)',n_tot_blocks)
 !
end subroutine
