!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine K_compress(iq,ID_compr)
 !
 use pars,         ONLY:cZERO
 use com,          ONLY:msg
 use LIVE_t,       ONLY:live_timing
 use IO_m,         ONLY:io_control,OP_WR,WR
 use BS,           ONLY:BS_blk,BS_T_grp,BS_max_val,BS_K_cutoff,n_BS_blks
 !
 implicit none
 !
 integer, intent(in)    :: iq
 integer, intent(inout) :: ID_compr
 !
 ! Work space
 !
 integer      :: i_block,i_Tgrp_k,i_Tgrp_p,i_Tp,i_Tk,io_BS_err,H_pos(2)
 integer, external :: io_BS_PAR_init
 !
 ! STEP 1: Check which matrix elements can be neglected
 !======================================================
 !
 call msg("s","Kernel compression with parameter",BS_K_cutoff)
 !
 call io_control(ACTION=OP_WR,ID=ID_compr)
 io_BS_err=io_BS_PAR_init(iq,ID_compr,"compressed_head")
 !
 if (n_BS_blks>0) call live_timing('Kernel compression',n_BS_blks)
 !
 do i_block=1,n_BS_blks
   !
   ! Read BS_blk
   ! This is not needed as long as the data are already in memory
   !
   !call io_control(ACTION=RD,ID=ID)
   !call io_BS_PAR_block(iq,i_block,ID)
   !
   i_Tgrp_k=BS_blk(i_block)%iT_k
   i_Tgrp_p=BS_blk(i_block)%iT_p
   !
   do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
     do i_Tk=1,BS_T_grp(i_Tgrp_k)%size
       !
       H_pos(1) = sum(BS_T_grp(:I_Tgrp_k-1)%size)+i_Tk
       H_pos(2) = sum(BS_T_grp(:I_Tgrp_p-1)%size)+i_Tp
       !
       if (H_pos(1)>H_pos(2)) cycle
       !
       if( abs(BS_blk(i_block)%mat(i_Tk,i_Tp)) >= BS_max_val*BS_K_cutoff ) cycle
       !
       BS_blk(i_block)%done(i_Tk,i_Tp)="n"
       !
       BS_blk(i_block)%mat(i_Tk,i_Tp)=cZERO
       !
     enddo
   enddo
   !
   ! Update BS_blk with %done 
   !
   call io_control(ACTION=WR,ID=ID_compr)
   call io_BS_PAR_block(iq,i_block,ID_compr,"compressed_head")
   !
   call live_timing(steps=1)
   !
 enddo
 !
 if (n_BS_blks>0) call live_timing( )
 !
end subroutine K_compress
!
!
subroutine K_compressed_IO(iq,io_BS_err,mode)
 !
 use pars,         ONLY:IPL
 use LIVE_t,       ONLY:live_timing
 use IO_m,         ONLY:io_control,OP_APP,OP_RD,RD,WR
 use BS,           ONLY:BS_lin_size_compressed,BS_blk,BS_nT_grps,n_BS_blks
 use parallel_m,   ONLY:ncpu,myid,PP_redux_wait,PAR_IND_T_ordered
 !
 implicit none
 !
 integer,      intent(in) :: iq
 integer,      intent(out):: io_BS_err
 character(*), intent(in) :: mode
 !
 integer      :: ID_head,ID_compr,IO_ACT
 integer      :: i_block,i_block_ref_CPU,last_block_CPU,i_Tgrp_k,i_Tgrp_p,x1,x2
 integer(IPL) :: imx_ref(ncpu),idx_CPU,idx,idx_tmp,idx_ref
 !
 integer, external :: io_BS_PAR_init
 !
 ! STEP 2: Compute compressed size
 !==================================
 !
 call live_timing('Compressed size',BS_nT_grps)
 !
 ID_head=0   ! Not used here
 imx_ref=0
 !
 idx_CPU=0
 last_block_CPU=1
 i_block_ref_CPU=1
 !
 ! Proceed by columns since we are in fortran
 !
 do i_Tgrp_p=1,BS_nT_grps
   do i_Tgrp_k=1,BS_nT_grps
     !
     idx_ref=0
     !
     if (PAR_IND_T_ordered%element_2D(i_Tgrp_k,i_Tgrp_p)) then
       !
       ! It is my block
       !
       do i_block=i_block_ref_CPU,n_BS_blks
       !do i_block=1,n_BS_blks
         !
         if(BS_blk(i_block)%iT_k/=i_Tgrp_k .or. BS_blk(i_block)%iT_p/=i_Tgrp_p) cycle
         !
         x1=BS_blk(i_block)%coordinate(1)
         x2=BS_blk(i_block)%coordinate(2)
         idx_ref = int(x2,kind=IPL)*int(x2-1,kind=IPL)/int(2,kind=IPL)+int(x1,kind=IPL)
         !
         i_block_ref_CPU=i_block
         !
         exit
         !
       enddo
       !
     endif
     !
     call PP_redux_wait((/idx_ref/))
     !
     idx_tmp=idx_CPU
     do i_block=last_block_CPU,n_BS_blks
     !idx_tmp=0
     !do i_block=1,n_BS_blks
       x1=BS_blk(i_block)%coordinate(1)
       x2=BS_blk(i_block)%coordinate(2)
       idx = int(x2,kind=IPL)*int(x2-1,kind=IPL)/int(2,kind=IPL)+int(x1,kind=IPL)
       if(idx>=idx_ref) exit
       !if(idx>=idx_ref) cycle
       idx_tmp=idx_tmp+int(count(BS_blk(i_block)%done(:,:)=="t"),IPL)
     enddo
     !
     last_block_CPU=i_block
     idx_CPU=idx_tmp
     !
     call PP_redux_wait((/idx_tmp/))
     !
     if (PAR_IND_T_ordered%element_2D(i_Tgrp_k,i_Tgrp_p)) then
       BS_blk(i_block_ref_CPU)%idx_compr=idx_tmp+1
       imx_ref(myid+1)=max(imx_ref(myid+1),idx_tmp+count(BS_blk(i_block_ref_CPU)%done(:,:)=="t"))
     endif
     !
   enddo
   call live_timing(steps=1)
 enddo
 !
 call PP_redux_wait(imx_ref)
 !
 BS_lin_size_compressed=maxval(imx_ref)
 !
 call live_timing( )
 !
 ! STEP 3: IO of compressed kernel
 !=================================
 !
 if(mode=="read" ) IO_ACT=OP_RD
 if(mode=="write") IO_ACT=OP_APP
 !
 call io_control(ACTION=IO_ACT,ID=ID_compr)
 io_BS_err=io_BS_PAR_init(iq,ID_compr,"compressed_mat")
 !
 if (n_BS_blks>0) call live_timing('Compressed BSE_mat '//trim(mode),n_BS_blks)
 !
 if(mode=="read" ) IO_ACT=RD
 if(mode=="write") IO_ACT=WR
 !
 do i_block=1,n_BS_blks
   !
   call io_control(ACTION=IO_ACT,ID=ID_compr)
   call io_BS_PAR_block(iq,i_block,ID_compr,"compressed_mat")
   !
   call live_timing(steps=1)
   !
 enddo
 !
 if (n_BS_blks>0) call live_timing( )
 !
 call io_BS_PAR_free(ID_head,ID_compr,"compressed_mat",.true.)
 !
end subroutine  K_compressed_IO
