! 
! Copyright (C) 2000-2008 A. Marini and the YAMBO team
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
subroutine K_filling(E_eh_range,S_eh_range,n_K_elements,tot_n_K_elements)
 !
 use pars,         ONLY:SP,schlen
 use com,          ONLY:msg
 use functions,    ONLY:K_scatter
 use BS,           ONLY:BS_eh_E,BS_columns,BS_K_dim,BS_blk_dim,BS_blk_coord
 use par_proc_m,   ONLY:myid,ncpu,pp_redux_wait
 use timing,       ONLY:live_timing
 implicit none
 !
 integer(8) :: n_K_elements(ncpu),tot_n_K_elements
 real(SP)   :: E_eh_range(2),S_eh_range(2)
 !
 ! Work Space
 !
 integer(8) :: i8id,i8nc,full_n_K_elements
 integer    :: ik2,ik1,ikk1,ikk2,icv1,icv2,i_cpu,n_blocks
 real(SP)   :: filling
 character(schlen) :: ch
 !
 i8nc=ncpu
 i8id=myid
 !
 tot_n_K_elements=0
 n_K_elements=0
 !
 full_n_K_elements=BS_K_dim
 full_n_K_elements=(full_n_K_elements**2-full_n_K_elements)/2+full_n_K_elements
 !
 ! Evaluate first the number of blocks to be done
 !
 n_blocks=0
 do ik2=1,BS_columns
   do ik1=ik2,1,-1
     if (any((/ik2<BS_blk_coord(2),ik2==BS_blk_coord(2).and.ik1>BS_blk_coord(1)/))) cycle
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
     if (any((/ik2<BS_blk_coord(2),ik2==BS_blk_coord(2).and.ik1>BS_blk_coord(1)/))) cycle
     call live_timing(steps=1)
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
 do i_cpu=1,ncpu
   write (ch,'(a,i2.2,a)') '[P ',i_cpu,'] Kernel filling [o/o]'
   call msg('rs',trim(ch),real(n_K_elements(i_cpu))/real(tot_n_K_elements)*100._SP)
 enddo
 !
end subroutine
