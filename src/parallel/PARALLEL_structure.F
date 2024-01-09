!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine PARALLEL_structure(n_elements,ROLE)
 !
 use pars,        ONLY:schlen
 use stderr,      ONLY:intc
 use parallel_m,  ONLY:ncpu,myid,INTER_CHAIN,nchains_group,n_groups,INTRA_CHAIN,&
&                      PARALLEL_CPU_role,PARALLEL_CPU_used,PARALLEL_n_structures_active,&
&                      ncpu_chain,mpi_comm_world,CREATE_the_COMM
 use parallel_m,  ONLY:PAR_Q_ibz_range,PAR_QP_range,PAR_n_v_bands,PAR_n_c_bands,PAR_K_range,PAR_n_bands,&
&                      PAR_EH_range,PAR_n_G_vectors,PAR_n_freqs
 use parallel_int,ONLY:PARALLEL_check_phase_space
 !
 implicit none
 !
 integer        :: n_elements 
 character(*)   :: ROLE(n_elements)
 !
 ! Work Space 
 !
 integer     ::ID(n_elements),i_cpu,i_chain,chain_now,i_order,&
&              tmp_comm_inter,tmp_comm_intra,i_structure,PHASE_space
 integer, volatile :: i_chain_lim(2)
 logical     ::USER_structure_is_ok
 character(schlen) ::USER_structure_msg
 !
 USER_structure_is_ok=.TRUE.
 !
 if (ncpu==1) then
   INTRA_CHAIN(1)%n_CPU=1
   INTER_CHAIN(1)%n_CPU=1
   return
 endif
 !
#if defined _MPI
 !
 n_groups=n_groups+1
 chain_now=n_groups*2-1
 if(chain_now>1) then
   nchains_group(chain_now)  =nchains_group(chain_now-1)+1
   nchains_group(chain_now+1)=nchains_group(chain_now-1)+n_elements
 else
   nchains_group(chain_now)  =1
   nchains_group(chain_now+1)=n_elements
 endif
 !
 i_chain_lim(1:2)=nchains_group(chain_now:chain_now+1)
 !
 do i_chain=i_chain_lim(1),i_chain_lim(2)
   i_order=i_chain-i_chain_lim(1)+1
   !
   ncpu_chain(i_chain)=1
   do i_structure = 1, PARALLEL_n_structures_active
     if ( trim(PARALLEL_CPU_role(i_structure)) == trim(ROLE(i_order)) ) then
       if (trim(ROLE(i_order))=="g" ) PHASE_space=PAR_n_G_vectors
       if (trim(ROLE(i_order))=="k" ) PHASE_space=PAR_K_range
       if (trim(ROLE(i_order))=="q" ) PHASE_space=PAR_Q_ibz_range(2)-PAR_Q_ibz_range(1)+1
       if (trim(ROLE(i_order))=="qp") PHASE_space=PAR_QP_range
       if (trim(ROLE(i_order))=="eh") PHASE_space=PAR_EH_range
       if (trim(ROLE(i_order))=="b" ) PHASE_space=PAR_n_bands(2)-PAR_n_bands(1)+1
       if (trim(ROLE(i_order))=="c" ) PHASE_space=PAR_n_c_bands(2)-PAR_n_c_bands(1)+1
       if (trim(ROLE(i_order))=="v" ) PHASE_space=PAR_n_v_bands(2)-PAR_n_v_bands(1)+1
       if (trim(ROLE(i_order))=="w" ) PHASE_space=PAR_n_freqs
       !
       if (PARALLEL_CPU_used(i_structure)>PHASE_space.and.trim(ROLE(i_order))/="eh") then
         USER_structure_msg=trim(intc(PARALLEL_CPU_used(i_structure)))//" CPU for "//&
&                           trim(intc(PHASE_space))//" elements (ROLE is '"//trim(ROLE(i_order))//"')"
         call PARALLEL_check_phase_space(PHASE_space/PARALLEL_CPU_used(i_structure),MSG=trim(USER_structure_msg))
       endif
       ncpu_chain(i_chain)=PARALLEL_CPU_used(i_structure)
     endif
   enddo
   !
 enddo
 !
 ! I need to define: ID,chain_order,my_chain=ID(1+i_chain),n_CPU
 !
 ID=0
 !
 do i_cpu=1,ncpu
   !
   ID(:)=ID(:)+1 ! CHAINS
   !
   if (myid+1==i_cpu) then
     !
     do i_chain=i_chain_lim(1),i_chain_lim(2)
       i_order=i_chain-i_chain_lim(1)+1
       INTRA_CHAIN(i_chain)%chain_order=i_order
       INTRA_CHAIN(i_chain)%my_CHAIN=ID(i_order)
       INTRA_CHAIN(i_chain)%n_CPU=product(ncpu_chain(i_chain_lim(1):i_chain))
     enddo
     !
   endif
   !
   do i_chain=i_chain_lim(1),i_chain_lim(2)
     i_order=i_chain-i_chain_lim(1)+1
     if ( mod( i_cpu,ncpu/product(ncpu_chain(i_chain_lim(1):i_chain)) )==0) ID(i_order)=0
   enddo
   !
 enddo
 !
 ! INTRA Communicators
 !
 tmp_comm_intra = mpi_comm_world
 !
 do i_chain=i_chain_lim(2),i_chain_lim(1),-1
   i_order=i_chain-nchains_group(chain_now)+1
   !
   call CREATE_the_COMM(tmp_comm_intra,INTRA_CHAIN(i_chain),myid)
   tmp_comm_intra = INTRA_CHAIN(i_chain)%COMM
   !
 enddo
 !
 ! INTER Communicators
 !
 tmp_comm_inter = mpi_comm_world
 !
 do i_chain=i_chain_lim(1),i_chain_lim(2)
   !
   INTER_CHAIN(i_chain)%my_CHAIN   =INTRA_CHAIN(i_chain)%CPU_id
   INTER_CHAIN(i_chain)%n_CPU      =ncpu/product(ncpu_chain(i_chain_lim(1):i_chain))
   INTER_CHAIN(i_chain)%chain_order=i_order
   !
   call CREATE_the_COMM(tmp_comm_inter,INTER_CHAIN(i_chain),myid)
   tmp_comm_inter = INTER_CHAIN(i_chain)%COMM
   !
 enddo
 !
#endif
 !
end subroutine
