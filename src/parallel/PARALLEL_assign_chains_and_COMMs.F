!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine PARALLEL_assign_chains_and_COMMs(n_elements,COMM_index_1,COMM_index_2,COMM_index_3,&
&                                           COMM_index_4,COMM_index_5,&
&                                           COMM_A2A_1,COMM_A2A_2,COMM_A2A_3,COMM_A2A_4)
 !
 use parallel_m,    ONLY:MPI_comm,INTRA_CHAIN,INTER_CHAIN,ncpu,COMM_copy,&
&                        nchains_group,ncpu_chain,n_groups
 !
 implicit none
 !
 integer                 :: n_elements
 type(MPI_comm)          :: COMM_index_1
 type(MPI_comm),optional :: COMM_index_2
 type(MPI_comm),optional :: COMM_index_3
 type(MPI_comm),optional :: COMM_index_4
 type(MPI_comm),optional :: COMM_index_5
 type(MPI_comm),optional :: COMM_A2A_1
 type(MPI_comm),optional :: COMM_A2A_2
 type(MPI_comm),optional :: COMM_A2A_3
 type(MPI_comm),optional :: COMM_A2A_4
 !
 ! Work Space
 !
 integer :: i_chain(5),chain_now,i_cpu(5)
 !
 if (ncpu==1) return
 !
 chain_now=n_groups*2-1
 !
 ! Level 1
 !
 i_chain(1)=nchains_group(chain_now)
 i_cpu(1)  =ncpu_chain(i_chain(1))
 !
 call COMM_copy(INTRA_CHAIN(i_chain(1)),COMM_index_1)
 if (present(COMM_A2A_1)) then
   call COMM_copy(INTER_CHAIN(i_chain(1)),COMM_A2A_1)
 endif
 !
 if(n_elements==1) return 
 !
 ! Level 2
 !
 i_chain(2)=nchains_group(chain_now)+1
 i_cpu(2)  =ncpu_chain(i_chain(2))*i_cpu(1)
 call PARALLEL_build_up_child_INTER_chains(i_chain(2),COMM_index_2,i_chain(1))
 if (present(COMM_A2A_2).and.i_cpu(2)/=ncpu) then
   call COMM_copy(INTER_CHAIN(i_chain(2)),COMM_A2A_2)
 endif
 !
 if(n_elements==2) return 
 !
 ! Level 3
 !
 i_chain(3)=nchains_group(chain_now)+2
 i_cpu(3)  =ncpu_chain(i_chain(3))*i_cpu(2)
 if (i_cpu(2)/=ncpu) then
   call PARALLEL_build_up_child_INTER_chains(i_chain(3),COMM_index_3,i_chain(2))
 else
   call PARALLEL_build_up_child_INTER_chains(i_chain(3),COMM_index_3,i_chain(1))
 endif
 if (present(COMM_A2A_3).and.i_cpu(3)/=ncpu) then
   call COMM_copy(INTER_CHAIN(i_chain(3)),COMM_A2A_3)
 endif
 !
 if(n_elements==3) return 
 !
 ! Level 4
 !
 i_chain(4)=nchains_group(chain_now)+3
 i_cpu(4)  =ncpu_chain(i_chain(4))*i_cpu(3)
 if (i_cpu(3)/=ncpu) then
   call PARALLEL_build_up_child_INTER_chains(i_chain(4),COMM_index_4,i_chain(3))
 else if (i_cpu(2)/=ncpu) then
   call PARALLEL_build_up_child_INTER_chains(i_chain(4),COMM_index_4,i_chain(2))
 else
   call PARALLEL_build_up_child_INTER_chains(i_chain(4),COMM_index_4,i_chain(1))
 endif
 if (present(COMM_A2A_4).and.i_cpu(4)/=ncpu) then
   call COMM_copy(INTER_CHAIN(i_chain(4)),COMM_A2A_4)
 endif
 !
 if(n_elements==4) return 
 !
 ! Level 5
 !
 i_chain(5)=nchains_group(chain_now)+4
 i_cpu(5)  =ncpu_chain(i_chain(5))*i_cpu(4)
 if (i_cpu(4)/=ncpu) then
   call PARALLEL_build_up_child_INTER_chains(i_chain(5),COMM_index_5,i_chain(4))
 else if (i_cpu(3)/=ncpu) then
   call PARALLEL_build_up_child_INTER_chains(i_chain(5),COMM_index_5,i_chain(3))
 else if (i_cpu(2)/=ncpu) then
   call PARALLEL_build_up_child_INTER_chains(i_chain(5),COMM_index_5,i_chain(2))
 else
   call PARALLEL_build_up_child_INTER_chains(i_chain(5),COMM_index_5,i_chain(1))
 endif
 !
end subroutine
