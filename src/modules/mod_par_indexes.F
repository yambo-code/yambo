!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
module PARALLEL_index_m
 !
 implicit none
 !
 contains
   !
   subroutine PARALLEL_index(px,uplim,low_range)
     use parallel_m, ONLY:PP_redux_wait,PP_indexes,ncpu,myid,p_prod
     use com,        ONLY:error
     type(PP_indexes)  ::px
     integer           ::uplim(:)
     integer, optional ::low_range(:)
     ! 
     ! Work Space
     !
     integer :: nindxs,i1,i2,i3,d2,ic,average_steps,tot_steps
     integer,allocatable :: i_done(:),lowlim(:)
     !
     nindxs=size(uplim)
     allocate(lowlim(nindxs))
     lowlim=1
     if (present(low_range))  lowlim=low_range
     !
     allocate(px%n_of_elements(ncpu))
     px%n_of_elements=0
     select case (nindxs)
       case (1)
         if (.not.associated(px%weight_1D)) then
           allocate(px%weight_1D(uplim(1)))
           px%weight_1D=1
           if (lowlim(1)>1) px%weight_1D(1:lowlim(1)-1)=0
         endif
         allocate(px%element_1D(uplim(1)),i_done(uplim(1)),px%first_of_1D(ncpu))
         px%first_of_1D(myid+1)=-1
         px%element_1D=.false.
         tot_steps=sum(px%weight_1D)
         i_done=1
         if (lowlim(1)>1) i_done(:lowlim(1)-1) =0
         average_steps=(tot_steps+ncpu-mod(tot_steps,ncpu))/ncpu
         do i1=lowlim(1)+myid,uplim(1),ncpu
           if (px%n_of_elements(myid+1)>=average_steps) cycle
           if (px%first_of_1D(myid+1)==-1) px%first_of_1D(myid+1)=i1
           px%element_1D(i1)=.true.
           i_done(i1)=0
           px%n_of_elements(myid+1)=px%n_of_elements(myid+1)+px%weight_1D(i1)
         enddo
         call PP_redux_wait(i_done,p_prod)
         do ic=1,ncpu
           if (ic==myid+1) then
             do i1=lowlim(1),uplim(1)
               if (i_done(i1)==0) cycle
               if (px%n_of_elements(myid+1)>=average_steps) cycle
               px%element_1D(i1)=.true.
               i_done(i1)=0
               px%n_of_elements(myid+1)=px%n_of_elements(myid+1)+px%weight_1D(i1)
             enddo
           endif
           call PP_redux_wait(i_done,p_prod)
         enddo
         if (any(i_done==1)) call error('Incomplete Parallel Index Filling')
         deallocate(i_done)
       case (2)
         allocate(px%element_2D(uplim(1),uplim(2)))
         d2=uplim(2)-lowlim(2)+1
         px%element_2D=.false.
         do i1=lowlim(1),uplim(1)
           do i2=lowlim(2),uplim(2)
             i3=(i1-lowlim(1))*d2+i2-lowlim(2)+1-myid
             if (mod(i3,ncpu)==1) px%element_2D(i1,i2)=.true.
             if (ncpu==1) px%element_2D(i1,i2)=.true.
             if (px%element_2D(i1,i2)) px%n_of_elements(myid+1)=px%n_of_elements(myid+1)+1
           enddo
         enddo
     end select
     !
     deallocate(lowlim)
     if(allocated(i_done)) deallocate(i_done)
     !
   end subroutine
   !
end module PARALLEL_index_m
