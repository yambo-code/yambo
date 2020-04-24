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
subroutine K_compress(iq,i_steps,n_steps,ID)
 !
 use pars,         ONLY:cZERO
 use LIVE_t,       ONLY:live_timing
 use IO_m,         ONLY:io_control,RD,WR
 use BS,           ONLY:BS_max_val,BS_K_cutoff,BS_blk,BS_T_grp,n_BS_blks
 !
 implicit none
 !
 integer, intent(in)    :: iq,i_steps,n_steps
 integer, intent(inout) :: ID
 !
 integer :: i_block,i_tgrp_k,i_Tgrp_p,i_Tp,i_Tk
 !
 if (n_steps>0) call live_timing('Kernel compression',n_steps)
 !
 do i_block=1,n_BS_blks
   !
   ! Read BS_blk
   !
   call io_control(ACTION=RD,ID=ID)
   call io_BS_PAR_block(iq,i_block,ID)
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
   ! Write BS_blk with %mat set to zero
   !
   call io_control(ACTION=WR,ID=ID)
   call io_BS_PAR_block(iq,i_block,ID)
   !
   call live_timing(steps=i_steps)
   !
 enddo
 !
 if (n_steps>0) call live_timing( )
 !
end subroutine
