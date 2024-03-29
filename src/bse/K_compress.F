!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine K_compress(iq,ID_compr)
 !
 use pars,         ONLY:cZERO
 use com,          ONLY:msg
 use LIVE_t,       ONLY:live_timing
 use IO_int,       ONLY:io_control
 use IO_m,         ONLY:OP_WR,WR
 use BS,           ONLY:BS_blk,BS_T_grp,BS_max_val,BS_K_cutoff,n_BS_blks
 !
 implicit none
 !
 integer, intent(in)    :: iq
 integer, intent(inout) :: ID_compr
 !
 ! Work space
 !
 integer :: i_block,i_Tgrp_k,i_Tgrp_p,i_Tp,i_Tk,io_BS_err,H_pos(2)
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
 use IO_int,       ONLY:io_control
 use IO_m,         ONLY:OP_APP,OP_RD,RD,WR
 use BS,           ONLY:BS_lin_size_compressed,BS_blk,BS_nT_grps,n_BS_blks
 use parallel_m,   ONLY:ncpu,myid,PAR_IND_T_ordered
 use parallel_int, ONLY:PP_redux_wait
 !
 implicit none
 !
 integer,      intent(in) :: iq
 integer,      intent(out):: io_BS_err
 character(*), intent(in) :: mode
 !
 integer      :: ID_compr,IO_ACT
 integer      :: i_block,i_block_ref_CPU,last_block_CPU,i_Tgrp_k,i_Tgrp_p,x1,x2,ib2,N(1)
 integer(IPL) :: idx,imx,idx_CPU,idx_tmp(maxval(BS_blk(:)%size(2))),&
&                idx_ref(1),imx_ref(1),imx_last(ncpu)
 !
 integer, external :: io_BS_PAR_init
 !
 ! STEP 2: Compute compressed size
 !==================================
 !
 call live_timing('Compressed size',BS_nT_grps)
 !
 imx_last=0
 !
 idx_CPU=0
 last_block_CPU=0
 i_block_ref_CPU=0
 !
 ! Proceed by columns since we are in fortran
 !
 do i_Tgrp_p=1,BS_nT_grps
   do i_Tgrp_k=1,BS_nT_grps
     !
     idx_ref=0
     imx_ref=0
     N=0
     !
     if (PAR_IND_T_ordered%element_2D(i_Tgrp_k,i_Tgrp_p)) then
       !
       ! It is my block
       !
       do i_block=i_block_ref_CPU+1,n_BS_blks
         !
         if(BS_blk(i_block)%iT_k/=i_Tgrp_k .or. BS_blk(i_block)%iT_p/=i_Tgrp_p) cycle
         !
         i_block_ref_CPU=i_block
         !
         x1=BS_blk(i_block)%coordinate(1)
         x2=BS_blk(i_block)%coordinate(2)
         idx_ref = int(x2,kind=IPL)*int(x2-1,kind=IPL)/int(2,kind=IPL)+int(x1,kind=IPL)
         !
         x1=x1+BS_blk(i_block)%size(1)-1
         x2=x2+BS_blk(i_block)%size(2)-1
         imx_ref = int(x2,kind=IPL)*int(x2-1,kind=IPL)/int(2,kind=IPL)+int(x1,kind=IPL)
         !
         N=BS_blk(i_block_ref_CPU)%size(2)
         !
         allocate(BS_blk(i_block)%idx_compr(BS_blk(i_block)%size(2)))
         !
         exit
         !
       enddo
       !
     endif
     !
     call PP_redux_wait(idx_ref)
     call PP_redux_wait(imx_ref)
     call PP_redux_wait(N)
     !
     idx_tmp=idx_CPU
     do i_block=last_block_CPU+1,n_BS_blks
       !
       x1=BS_blk(i_block)%coordinate(1)
       x2=BS_blk(i_block)%coordinate(2)
       idx = int(x2,kind=IPL)*int(x2-1,kind=IPL)/int(2,kind=IPL)+int(x1,kind=IPL)
       !
       if(idx> imx_ref(1)) exit
       !
       ! Present block plus blocks below
       !
       if(idx>=idx_ref(1)) then
         if(idx==idx_ref(1)) idx_tmp(:N(1))=idx_tmp(:N(1))+1
         do ib2=2,N(1)
           idx_tmp(ib2)=idx_tmp(ib2)+int(count(BS_blk(i_block)%done(:,:ib2-1)=="t"),IPL)
         enddo
         cycle
       endif
       !
       x1=x1+BS_blk(i_block)%size(1)-1
       x2=x2+BS_blk(i_block)%size(2)-1
       imx = int(x2,kind=IPL)*int(x2-1,kind=IPL)/int(2,kind=IPL)+int(x1,kind=IPL)
       !
       ! Blocks above
       !
       if(imx>idx_ref(1)) then
         do ib2=1,N(1)
           idx_tmp(ib2)=idx_tmp(ib2)+int(count(BS_blk(i_block)%done(:,:ib2)=="t"),IPL)
         enddo
         cycle
       endif
       !
       ! Blocks before
       !
       idx_tmp(1:N(1))=idx_tmp(1:N(1))+int(count(BS_blk(i_block)%done(:,:)=="t"),IPL)
       !
       last_block_CPU=i_block
       idx_CPU=idx_tmp(1)
       !
     enddo
     !
     call PP_redux_wait(idx_tmp)
     !
     if (PAR_IND_T_ordered%element_2D(i_Tgrp_k,i_Tgrp_p)) then
       BS_blk(i_block_ref_CPU)%idx_compr(:N(1))=idx_tmp(:N(1))
       imx_last(myid+1)=idx_tmp(N(1))+max(int(count(BS_blk(i_block_ref_CPU)%done(:,N(1))=="t")-1,IPL),0)
     endif
     !
   enddo
   call live_timing(steps=1)
 enddo
 !
 call PP_redux_wait(imx_last)
 !
 BS_lin_size_compressed=maxval(imx_last)
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
 call io_BS_PAR_free(0,ID_compr,"compressed_mat",.true.)
 !
end subroutine  K_compressed_IO
