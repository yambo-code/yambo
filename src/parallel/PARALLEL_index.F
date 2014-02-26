!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
subroutine PARALLEL_index(px,uplim,low_range,COMM,CONSECUTIVE)
 !
 use parallel_m, ONLY:PP_redux_wait,PP_indexes,ncpu,myid,p_prod,mpi_comm_world,&
&                     MPI_comm,comm_default_value
 use com,        ONLY:error
 implicit none
 type(PP_indexes)       ::px
 integer                ::uplim(:)
 integer,       optional::low_range(:)
 type(MPI_comm),optional::COMM
 logical       ,optional::CONSECUTIVE
 ! 
 ! Work Space
 !
 integer :: nindxs,i1,i2,i3,d2,ic,average_steps,tot_steps,myid_loop,ncpu_loop,local_comm,&
&           i_range(2)
 logical :: SERIAL
 integer,allocatable :: i_done(:),lowlim(:)
 !
 myid_loop =myid
 ncpu_loop =ncpu
 local_comm=mpi_comm_world
 SERIAL    =.FALSE.
 !
 if (present(COMM)) then
   ncpu_loop =COMM%n_CPU
   myid_loop =COMM%CPU_id
   local_comm=COMM%COMM
 endif
 !
 if (ncpu_loop==1) then
   myid_loop =0
   local_comm=comm_default_value
   SERIAL    =.TRUE.
 endif
 !
 nindxs=size(uplim)
 allocate(lowlim(nindxs))
 lowlim=1
 if (present(low_range))  lowlim=low_range
 !
 allocate(px%n_of_elements(ncpu_loop))
 px%n_of_elements=0
 !
 if (nindxs==1.and.present(CONSECUTIVE)) then
   !
   allocate(px%element_1D(uplim(1)),i_done(uplim(1)))
   px%element_1D=.false.
   i_done=1
   i_done(:lowlim(1))=0
   average_steps=(uplim(1)-lowlim(1)+1)/ncpu_loop
   do i1=1,ncpu_loop
     if (i1==myid_loop+1) then
       i_range=(/lowlim(1)+average_steps*(i1-1),lowlim(1)+average_steps*i1-1/)
       px%element_1D(i_range(1):i_range(2))=.TRUE.
       px%n_of_elements(myid_loop+1)=average_steps
       i_done(i_range(1):i_range(2))=i_done(i_range(1):i_range(2))-1
     endif
   enddo
   !
   do i1=lowlim(1)+average_steps*ncpu_loop+myid_loop,uplim(1),ncpu_loop
     px%element_1D(i1)=.TRUE.
     px%n_of_elements(myid_loop+1)=px%n_of_elements(myid_loop+1)+1
     i_done(i1)=i_done(i1)-1
   enddo
   !
   if (.not.SERIAL) call PP_redux_wait(i_done,p_prod,COMM=local_comm)
   if (any(i_done==1)) call error('Incomplete Parallel Index Filling')
   !
 else if (nindxs==1.and..not.present(CONSECUTIVE)) then
   if (.not.associated(px%weight_1D)) then
     allocate(px%weight_1D(uplim(1)))
     px%weight_1D=1
     if (lowlim(1)>1) px%weight_1D(1:lowlim(1)-1)=0
   endif
   allocate(px%element_1D(uplim(1)),i_done(uplim(1)),px%first_of_1D(ncpu_loop))
   px%first_of_1D(myid_loop+1)=-1
   px%element_1D=.false.
   tot_steps=sum(px%weight_1D)
   i_done=1
   if (lowlim(1)>1) i_done(:lowlim(1)-1) =0
   average_steps=(tot_steps+ncpu_loop-mod(tot_steps,ncpu_loop))/ncpu_loop
   !
   ! Here the linear chain is distributed
   !
   do i1=lowlim(1)+myid_loop,uplim(1),ncpu_loop
     if (px%n_of_elements(myid_loop+1)>=average_steps) cycle
     if (px%first_of_1D(myid_loop+1)==-1) px%first_of_1D(myid_loop+1)=i1
     px%element_1D(i1)=.true.
     i_done(i1)=0
     px%n_of_elements(myid_loop+1)=px%n_of_elements(myid_loop+1)+px%weight_1D(i1)
   enddo
   if (.not.SERIAL) call PP_redux_wait(i_done,p_prod,COMM=local_comm)
   !
   ! Fill the remaining elements trying to not go beyond the average_steps
   !
   do ic=1,ncpu_loop
     if (ic==myid_loop+1) then
       do i1=lowlim(1),uplim(1)
         if (i_done(i1)==0) cycle
         if (px%n_of_elements(myid_loop+1)>=average_steps) cycle
         px%element_1D(i1)=.true.
         i_done(i1)=0
         px%n_of_elements(myid_loop+1)=px%n_of_elements(myid_loop+1)+px%weight_1D(i1)
       enddo
     endif
     if (.not.SERIAL) call PP_redux_wait(i_done,p_prod,COMM=local_comm)
   enddo
   !
   if (any(i_done==1)) call error('Incomplete Parallel Index Filling')
   deallocate(i_done)
   !
 else if (nindxs==2) then
   !
   allocate(px%element_2D(uplim(1),uplim(2)))
   d2=uplim(2)-lowlim(2)+1
   px%element_2D=.false.
   do i1=lowlim(1),uplim(1)
     do i2=lowlim(2),uplim(2)
       i3=(i1-lowlim(1))*d2+i2-lowlim(2)+1-myid_loop
       if (mod(i3,ncpu_loop)==1) px%element_2D(i1,i2)=.true.
       if (ncpu_loop==1) px%element_2D(i1,i2)=.true.
       if (px%element_2D(i1,i2)) px%n_of_elements(myid_loop+1)=px%n_of_elements(myid_loop+1)+1
     enddo
   enddo
   !
 endif
 !
 deallocate(lowlim)
 if(allocated(i_done)) deallocate(i_done)
 !
end subroutine
