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
subroutine PARALLEL_assign_chains_and_COMMs(n_elements,ROLE,COMM_index_1,COMM_index_2,COMM_index_3,&
&                                           COMM_index_4,COMM_A2A_1,COMM_A2A_2)
 !
 use parallel_m,    ONLY:MPI_comm,INTRA_CHAIN,INTER_CHAIN,PARALLEL_n_structures_active,&
&                        PARALLEL_CPU_used,ncpu,COMM_copy,PARALLEL_CPU_role,&
&                        n_chains
 !
 implicit none
 !
 integer                 :: n_elements
 character(*)            :: ROLE(:)
 type(MPI_comm)          :: COMM_index_1
 type(MPI_comm),optional :: COMM_index_2
 type(MPI_comm),optional :: COMM_index_3
 type(MPI_comm),optional :: COMM_index_4
 type(MPI_comm),optional :: COMM_A2A_1
 type(MPI_comm),optional :: COMM_A2A_2
 !
 ! Work Space
 !
 integer :: i_chain(n_elements),i_c
 !
 if (ncpu==1) return
 !
 i_chain=0
 !
 select case (n_elements)
   !
   case (4)
     !
     do i_c=1,PARALLEL_n_structures_active
       !
       if (trim(PARALLEL_CPU_role(i_c))==trim(ROLE(1))) then
         i_chain(1)=log(real(ncpu/PARALLEL_CPU_used(i_c)))/log(2.)
         if (i_chain(1)==0) then
           call COMM_copy(INTER_CHAIN(n_chains),COMM_index_1)
           if (present(COMM_A2A_1)) then
             call COMM_copy(INTRA_CHAIN(n_chains),COMM_A2A_1)
           endif
         else
           call COMM_copy(INTRA_CHAIN(i_chain(1)),COMM_index_1)
           if (present(COMM_A2A_1)) then
             call COMM_copy(INTER_CHAIN(i_chain(1)),COMM_A2A_1)
           endif
         endif
       endif
       !
     enddo
     do i_c=1,PARALLEL_n_structures_active
       !
       if (trim(PARALLEL_CPU_role(i_c))==trim(ROLE(2))) then
         i_chain(2)  =i_chain(1)-log(real(PARALLEL_CPU_used(i_c)))/log(2.)
         call PARALLEL_build_up_child_chains(i_chain(2),COMM_index_2,i_chain(1))
         if (present(COMM_A2A_2)) then
           call COMM_copy(INTER_CHAIN(i_chain(2)),COMM_A2A_2)
         endif
       endif
       !
     enddo
     do i_c=1,PARALLEL_n_structures_active
       !
       if (trim(PARALLEL_CPU_role(i_c))==trim(ROLE(3))) then
         if (i_chain(2)/=0) then
           i_chain(3)  =i_chain(2)-log(real(PARALLEL_CPU_used(i_c)))/log(2.)
           call PARALLEL_build_up_child_chains(i_chain(3),COMM_index_3,i_chain(2))
         else
           i_chain(3)  =i_chain(1)-log(real(PARALLEL_CPU_used(i_c)))/log(2.)
           call PARALLEL_build_up_child_chains(i_chain(3),COMM_index_3,i_chain(1))
         endif
       endif
       !
     enddo
     do i_c=1,PARALLEL_n_structures_active
       !
       if (trim(PARALLEL_CPU_role(i_c))==trim(ROLE(4))) then
         if (i_chain(3)/=0) then
           call COMM_copy(INTER_CHAIN(i_chain(3)),COMM_index_4)
         else if (i_chain(2)/=0) then
           call COMM_copy(INTER_CHAIN(i_chain(2)),COMM_index_4)
         else
           call COMM_copy(INTER_CHAIN(i_chain(1)),COMM_index_4)
         endif
       endif
       !
     enddo
     !
   case (3)
     !
     do i_c=1,PARALLEL_n_structures_active
       !
       if (trim(PARALLEL_CPU_role(i_c))==trim(ROLE(1))) then
         i_chain(1)=log(real(ncpu/PARALLEL_CPU_used(i_c)))/log(2.)
         if (i_chain(1)==0) then
           call COMM_copy(INTER_CHAIN(n_chains),COMM_index_1)
           if (present(COMM_A2A_1)) then
             call COMM_copy(INTRA_CHAIN(n_chains),COMM_A2A_1)
           endif
         else
           call COMM_copy(INTRA_CHAIN(i_chain(1)),COMM_index_1)
           if (present(COMM_A2A_1)) then
             call COMM_copy(INTER_CHAIN(i_chain(1)),COMM_A2A_1)
           endif
         endif
       endif
       !
     enddo
     do i_c=1,PARALLEL_n_structures_active
       !
       if (trim(PARALLEL_CPU_role(i_c))==trim(ROLE(2))) then
         i_chain(2)  =i_chain(1)-log(real(PARALLEL_CPU_used(i_c)))/log(2.)
         call PARALLEL_build_up_child_chains(i_chain(2),COMM_index_2,i_chain(1))
         if (present(COMM_A2A_2)) then
           call COMM_copy(INTER_CHAIN(i_chain(2)),COMM_A2A_2)
         endif
       endif
       !
     enddo
     do i_c=1,PARALLEL_n_structures_active
       !
       if (trim(PARALLEL_CPU_role(i_c))==trim(ROLE(3))) then
         if (i_chain(2)/=0) then
           call COMM_copy(INTER_CHAIN(i_chain(2)),COMM_index_3)
         else
           call COMM_copy(INTER_CHAIN(i_chain(1)),COMM_index_3)
         endif
       endif
       !
     enddo
     !
   case (2)
     !
     do i_c=1,PARALLEL_n_structures_active
       !
       if (trim(PARALLEL_CPU_role(i_c))==trim(ROLE(1))) then
         i_chain(1)=log(real(ncpu/PARALLEL_CPU_used(i_c)))/log(2.)
         if (i_chain(1)==0) then
           call COMM_copy(INTER_CHAIN(n_chains),COMM_index_1)
           if (present(COMM_A2A_1)) then
             call COMM_copy(INTRA_CHAIN(n_chains),COMM_A2A_1)
           endif
         else
           call COMM_copy(INTRA_CHAIN(i_chain(1)),COMM_index_1)
           if (present(COMM_A2A_1)) then
             call COMM_copy(INTER_CHAIN(i_chain(1)),COMM_A2A_1)
           endif
         endif
       endif
       !
     enddo
     do i_c=1,PARALLEL_n_structures_active
       !
       if (trim(PARALLEL_CPU_role(i_c))==trim(ROLE(2))) then
         i_chain(2)  =i_chain(1)-log(real(PARALLEL_CPU_used(i_c)))/log(2.)
         call COMM_copy(INTER_CHAIN(i_chain(1)),COMM_index_2)
       endif
       !
     enddo
     !
   case (1)
     !
     do i_c=1,PARALLEL_n_structures_active
       !
       if (trim(PARALLEL_CPU_role(i_c))==trim(ROLE(1))) then
         i_chain(1)  =log(real(PARALLEL_CPU_used(i_c)))/log(2.)
         call COMM_copy(INTER_CHAIN(i_chain(1)),COMM_index_1)
       endif
       !
     enddo
 end select
 !
end subroutine
