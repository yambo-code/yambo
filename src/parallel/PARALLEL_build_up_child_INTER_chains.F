!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine PARALLEL_build_up_child_INTER_chains(i_child,COMM_child,i_father)
 !
 ! Create hierarchy-based INTER (=non countiguous) communicators
 !
#if defined _MPI
 !
 use parallel_m,    ONLY:yMPI_comm,myid,PAR_COM_WORLD,ncpu_chain,nchains_group,&
&                        n_groups,ncpu,i_INTER_CHAIN,CREATE_the_COMM
 !
 implicit none
 !
 type(yMPI_comm) :: COMM_child
 integer        :: i_child,i_father,n_father,n_child
 !
 n_father = ncpu/product(ncpu_chain(nchains_group(n_groups*2-1):i_father))
 n_child  = ncpu_chain(i_child)
 !
 COMM_child%my_CHAIN=i_INTER_CHAIN(n_father,n_child)
 call CREATE_the_COMM(PAR_COM_WORLD%COMM,COMM_child,myid)
 !
 COMM_child%chain_order=i_child-nchains_group(n_groups*2-1)+1
 !
#endif
 !
end subroutine
