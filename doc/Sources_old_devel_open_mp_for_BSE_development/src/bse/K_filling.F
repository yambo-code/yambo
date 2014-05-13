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
subroutine K_filling(E_eh_range,S_eh_range,Par_kernel)
 !
 use pars,         ONLY:SP,schlen
 use com,          ONLY:msg
 use functions,    ONLY:K_scatter
 use BS,           ONLY:BS_eh_E,BS_k_and_row_restart,BS_IO_index
 use R_lattice,    ONLY:nXkibz
 use parallel_m
!,   ONLY:myid,ncpu,pp_redux_wait,pp_indexes,POOLS_initialize,n_pools,ncpu
 use interfaces,   ONLY:PARALLEL_index
 use LIVE_t,       ONLY:live_timing
 use stderr,       ONLY:intc
 implicit none
 !
 real(SP)         :: E_eh_range(2),S_eh_range(2)
 type(PP_indexes) :: Par_kernel
 !
 ! Work Space
 !
 integer(8) :: n_K_elements(ncpu),tot_n_K_elements
 integer    :: ik2,ik1,ikk1,ikk2,icv1,icv2,i_cpu,n_blocks
 character(schlen) :: ch
 !
 n_K_elements    =0
 !
 ! Evaluate first the number of blocks to be done
 !
 n_blocks=0
 do ik2=1,nXkibz
   do ik1=ik2,1,-1
     if (any((/ik2<BS_k_and_row_restart(2),ik2==BS_k_and_row_restart(2).and.ik1>BS_k_and_row_restart(1)/))) cycle
     n_blocks=n_blocks+1
   enddo
 enddo
 !
 ! Report
 !========
 !
 !call PARALLEL_index(Par_kernel,(/n_blocks/),USE_GROUP=.true.)
 !DEBUG>
 !do ik1=1,n_blocks
 !  if (Par_kernel%element_1D(ik1)) write (*,*) myid+1,ik1
 !enddo
 !DEBUG<
 !
 ! Blocks loop 
 !
 call live_timing('Filling Estimation',Par_kernel%n_of_elements(myid+1))
 !
 do ik2=1,nXkibz
   do ik1=ik2,1,-1
     !
     !ikk1=sum(BS_blk_dim(:ik1-1))
     !ikk2=sum(BS_blk_dim(:ik2-1))
     !
     if (ik2<BS_k_and_row_restart(2)) cycle
     if (ik2==BS_k_and_row_restart(2).and.ik1>=BS_k_and_row_restart(1)) cycle
     !
     if (.not.Par_kernel%element_1D(BS_IO_index(ik1,ik2)-1)) cycle
     !
     call live_timing(steps=1)
     !
     !do icv1=1,BS_blk_dim(ik1)
       !do icv2=1,BS_blk_dim(ik2)
         if (ik1==ik2.and.icv2<icv1) cycle
         !
         if (.not.K_scatter(abs(BS_eh_E(ikk1+icv1)),abs(BS_eh_E(ikk2+icv2)),E_eh_range,S_eh_range)) cycle
         !
         n_K_elements(myid+1)=n_K_elements(myid+1)+1
         !
       !enddo
     !enddo
     !
   enddo
 enddo
 call live_timing()
 call PP_redux_wait(n_K_elements)
 !
 tot_n_K_elements=sum(n_K_elements)
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
end subroutine
