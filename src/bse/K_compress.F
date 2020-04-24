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
subroutine K_compress(iq,i_steps,n_steps,ID_head)
 !
 use pars,         ONLY:IPL,cZERO
 use LIVE_t,       ONLY:live_timing
 use IO_m,         ONLY:io_control,OP_WR,RD,WR
 use BS,           ONLY:BS_max_val,BS_lin_size_compressed,BS_K_cutoff,BS_blk,&
&                       BS_nT_grps,BS_T_grp,n_BS_blks
 use parallel_m,   ONLY:ncpu,myid,PP_redux_wait,PAR_IND_T_ordered
 !
 implicit none
 !
 integer, intent(in)    :: iq,i_steps,n_steps
 integer, intent(inout) :: ID_head
 !
 ! Work space
 !
 integer      :: ID,i_block,i_block_ref,i_tgrp_k,i_Tgrp_p,i_Tp,i_Tk,x1,x2
 integer(IPL) :: imx_ref(ncpu),idx,idx_tmp,idx_ref
 !
 ! STEP 1: Check which matrix elements can be neglected
 !======================================================
 !
 if (n_steps>0) call live_timing('Kernel checking elements to be neglected',n_steps)
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
   call io_control(ACTION=WR,ID=ID_head)
   call io_BS_PAR_block(iq,i_block,ID_head,ID,"head")
   !
   call live_timing(steps=i_steps)
   !
 enddo
 !
 if (n_steps>0) call live_timing( )
 !
 ! STEP 2: Compute compressed size
 !==================================
 !
 call live_timing('Kernel compute compressed size',BS_nT_grps)
 !
 imx_ref=0
 !
 do i_Tgrp_k=1,BS_nT_grps
   do i_Tgrp_p=1,BS_nT_grps
     !
     idx_ref=0
     !
     if (PAR_IND_T_ordered%element_2D(i_Tgrp_k,i_Tgrp_p)) then
       !
       ! It is my block
       !
       i_block_ref=-1
       ! 
       do i_block=1,n_BS_blks
         !
         if(BS_blk(i_block)%iT_k/=i_Tgrp_k .or. BS_blk(i_block)%iT_p/=i_Tgrp_p) cycle
         !
         x1=BS_blk(i_block)%coordinate(1)
         x2=BS_blk(i_block)%coordinate(2)
         idx_ref = int(x2,kind=IPL)*int(x2-1,kind=IPL)/int(2,kind=IPL)+int(x1,kind=IPL)
         !
         i_block_ref=i_block
         !
         exit
         !
       enddo
       !
     endif
     !
     call PP_redux_wait((/idx_ref/))
     !
     idx_tmp=0
     do i_block=1,n_BS_blks
       x1=BS_blk(i_block)%coordinate(1)
       x2=BS_blk(i_block)%coordinate(2)
       idx = int(x2,kind=IPL)*int(x2-1,kind=IPL)/int(2,kind=IPL)+int(x1,kind=IPL)
       if(idx>=idx_ref) cycle
       idx_tmp=idx_tmp+int(count(BS_blk(i_block)%done(:,:)=="t"),IPL)
     enddo
     !
     call PP_redux_wait((/idx_tmp/))
     !
     if (PAR_IND_T_ordered%element_2D(i_Tgrp_k,i_Tgrp_p)) then
       BS_blk(i_block_ref)%idx_compr=idx_tmp+1
       imx_ref(myid+1)=max(imx_ref(myid+1),idx_tmp+count(BS_blk(i_block_ref)%done(:,:)=="t"))
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
 call io_control(ACTION=OP_WR,ID=ID)
 call io_BS_PAR_init(iq,ID_head,ID,"compressed")
 !
 if (n_steps>0) call live_timing('Kernel I/O compressed',n_steps)
 !
 do i_block=1,n_BS_blks
   !
   call io_control(ACTION=WR,ID=ID)
   call io_BS_PAR_block(iq,i_block,ID_head,ID,"compressed")
   !
   call live_timing(steps=i_steps)
   !
 enddo
 !
 if (n_steps>0) call live_timing( )
 !
 call io_BS_PAR_free(ID_head,ID,"compressed",.true.)
 !
end subroutine
