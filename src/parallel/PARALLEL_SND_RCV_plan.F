!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine PARALLEL_SND_RCV_plan( COMM, N )
 !
 use parallel_m,     ONLY:MPI_comm,N_SND_RCV_operations,SND_RCV_op
 use LIVE_t,         ONLY:LIVE_message
 use stderr,         ONLY:intc
 !
 implicit none
 !
 type(MPI_comm), intent(in) :: COMM
 integer,        intent(in) :: N
 !
 ! Work Space
 !
 integer :: i_s(2),i_err,i_c,i_cycle,i_op,n_pairs,n_inactive
 integer :: MAT(N,N)
 !
 if (COMM%n_CPU==1) return
 !
 do i_cycle=1,2
   !
   MAT       =0
   n_inactive=0
   do i_c=1,N
     MAT(i_c,i_c)=-1
   enddo
   i_op=0
   i_s=(/1,1/)
   do while(any(MAT==0))
     i_err=1
     call PROPOSE_pair( i_err , .FALSE. )
     if (i_err<0) exit
     i_op=i_op+1
     MAT(i_s(1),i_s(2))=i_op
     if (i_cycle==2) then
       SND_RCV_op(i_op)%RCV_from_id(i_s(1))=i_s(2)
       SND_RCV_op(i_op)%SND_to_id(i_s(2))=i_s(1)
     endif
     n_pairs=1
     do while(i_err==1)
       call PROPOSE_pair( i_err , .TRUE. )
       if (i_cycle==2.and.i_err==1) then
         SND_RCV_op(i_op)%RCV_from_id(i_s(1))=i_s(2)
         SND_RCV_op(i_op)%SND_to_id(i_s(2))=i_s(1)
         n_pairs=n_pairs+1
       endif
     enddo
     n_inactive=max(n_inactive,N-n_pairs*2)
   enddo
   !
   if (i_cycle==1) then
     N_SND_RCV_operations=i_op
     allocate(SND_RCV_op(N_SND_RCV_operations))
     do i_op=1,N_SND_RCV_operations
       allocate(SND_RCV_op(i_op)%RCV_from_id(N))
       allocate(SND_RCV_op(i_op)%SND_to_id(N))
       SND_RCV_op(i_op)%RCV_from_id=0
       SND_RCV_op(i_op)%SND_to_id=0
     enddo
   endif
   !
 enddo
 !
 call LIVE_message("[PARALLEL] Found "//trim(intc(N_SND_RCV_operations))//" async SND and RCV operations",lfmt="n")
 if (n_inactive>0) then
   call LIVE_message("[PARALLEL] Max "//trim(intc(n_inactive))//" CPU remain inactive during the operations",lfmt="n")
 endif
 !
 contains
   !
   subroutine PROPOSE_pair( i_succ , EXCLUSIVE )
     integer :: i_succ,i_try(2),i_c,i_cp
     logical :: EXCLUSIVE
     do i_c=1,N
       do i_cp=1,N
         if (i_c==i_cp) cycle
         i_try=(/i_c,i_cp/)
         if (EXCLUSIVE.and.any(MAT(i_try(1),:)==i_op)) cycle
         if (EXCLUSIVE.and.any(MAT(:,i_try(1))==i_op)) cycle
         if (EXCLUSIVE.and.any(MAT(:,i_try(2))==i_op)) cycle
         if (EXCLUSIVE.and.any(MAT(i_try(2),:)==i_op)) cycle
         if (MAT(i_try(1),i_try(2))==0.and.MAT(i_try(2),i_try(1))==0) then
           MAT(i_try(1),i_try(2))=i_op
           MAT(i_try(2),i_try(1))=i_op
           i_s=i_try
           i_succ=1
           return
         endif
       enddo
     enddo
     i_succ=-1
   end subroutine
   !
end subroutine PARALLEL_SND_RCV_plan
