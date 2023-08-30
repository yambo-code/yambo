!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine PARALLEL_assign_chains_and_COMMs(n_elements,COMM_index_1,COMM_index_2,COMM_index_3,&
&                                           COMM_index_4,COMM_index_5,COMM_A2A_1,COMM_A2A_2,&
                                            COMM_A2A_3,COMM_A2A_4,COMM_index_global_2)
 !
 use parallel_m,    ONLY:yMPI_comm,INTRA_CHAIN,INTER_CHAIN,ncpu,COMM_copy,&
&                        nchains_group,ncpu_chain,n_groups
 !
 implicit none
 !
 integer                 :: n_elements
 type(yMPI_comm)          :: COMM_index_1
 type(yMPI_comm),optional :: COMM_index_2
 type(yMPI_comm),optional :: COMM_index_3
 type(yMPI_comm),optional :: COMM_index_4
 type(yMPI_comm),optional :: COMM_index_5
 type(yMPI_comm),optional :: COMM_A2A_1
 type(yMPI_comm),optional :: COMM_A2A_2
 type(yMPI_comm),optional :: COMM_A2A_3
 type(yMPI_comm),optional :: COMM_A2A_4
 type(yMPI_comm),optional :: COMM_index_global_2
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
 if (present(COMM_index_global_2)) then
   call COMM_copy(INTRA_CHAIN(i_chain(2)),COMM_index_global_2)
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
