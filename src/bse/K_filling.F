! 
! Copyright (C) 2000-2011 A. Marini and the YAMBO team
!              http://www.yambo-code.org
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
subroutine K_filling(E_eh_range,S_eh_range,n_K_elements,tot_n_K_elements,&
&                    live_timing_steps,live_timing_step_size)
 !
 use pars,         ONLY:SP,schlen
 use com,          ONLY:msg
 use functions,    ONLY:K_scatter
 use BS,           ONLY:BS_eh_E,BS_columns,BS_blk_dim,BS_k_and_row_restart
 use par_proc_m,   ONLY:myid,ncpu,pp_redux_wait
 use timing,       ONLY:live_timing,nhash
 implicit none
 !
 integer(8) :: n_K_elements(ncpu),tot_n_K_elements
 integer    :: live_timing_steps,live_timing_step_size
 real(SP)   :: E_eh_range(2),S_eh_range(2)
 !
 ! Work Space
 !
 integer(8) :: i8id,i8nc,test_LT_elements
 integer    :: ik2,ik1,ikk1,ikk2,icv1,icv2,i_cpu,n_blocks,i_scale
 real(SP)   :: filling
 character(schlen) :: ch
 !
 i8nc=ncpu
 i8id=myid
 !
 tot_n_K_elements=0
 n_K_elements=0
 !
 ! Evaluate first the number of blocks to be done
 !
 n_blocks=0
 do ik2=1,BS_columns
   do ik1=ik2,1,-1
     if (any((/ik2<BS_k_and_row_restart(2),ik2==BS_k_and_row_restart(2).and.ik1>BS_k_and_row_restart(1)/))) cycle
     n_blocks=n_blocks+1
   enddo
 enddo
 !
 ! Blocks loop 
 !
 call live_timing('Filling Estimation',n_blocks)
 do ik2=1,BS_columns
   do ik1=ik2,1,-1
     !
     ikk1=sum(BS_blk_dim(:ik1-1))
     ikk2=sum(BS_blk_dim(:ik2-1))
     !
     call live_timing(steps=1)
     if (ik2<BS_k_and_row_restart(2)) cycle
     if (ik2==BS_k_and_row_restart(2).and.ik1>=BS_k_and_row_restart(1)) cycle
     !
     do icv1=1,BS_blk_dim(ik1)
       do icv2=1,BS_blk_dim(ik2)
         if (ik1==ik2.and.icv2<icv1) cycle
         tot_n_K_elements=tot_n_K_elements+1
         if (.not.K_scatter(abs(BS_eh_E(ikk1+icv1)),abs(BS_eh_E(ikk2+icv2)),&
&                           E_eh_range,S_eh_range)) cycle
         if (mod(tot_n_K_elements-i8id,i8nc)==1) &
&                     n_K_elements(myid+1)=n_K_elements(myid+1)+1
         if (ncpu==1) n_K_elements(myid+1)=n_K_elements(myid+1)+1
       enddo
     enddo
     !
   enddo
 enddo
 call live_timing()
 call pp_redux_wait(n_K_elements)
 !
 ! Report matrix elements assigned to each CPU
 !
 do i_cpu=1,ncpu
   write (ch,'(a,i2.2,a)') '[P ',i_cpu,'] Kernel filling [o/o]'
   if (tot_n_K_elements>0) then
     call msg('rs',trim(ch),real(n_K_elements(i_cpu))/real(tot_n_K_elements)*100._SP)
   else
     call msg('rs',trim(ch),0._SP)
   endif
 enddo
 !
 ! Rescale the total number of K elements that are passed to the
 ! the live_timing routine. This is done in case the integer(4) used 
 ! in mod_timing are not enough to store tot_n_K_elements
 !
 i_scale=10
 test_LT_elements=n_K_elements(myid+1)/i_scale/nhash
 do while(int(test_LT_elements,4)/=test_LT_elements)
   i_scale=i_scale*10
   test_LT_elements=n_K_elements(myid+1)/i_scale/nhash
 enddo
 live_timing_steps=test_LT_elements
 live_timing_step_size=i_scale*nhash
 !
 if (mod(n_K_elements(myid+1),int(live_timing_step_size,8))/=0) live_timing_steps=live_timing_steps+1
 !
end subroutine
