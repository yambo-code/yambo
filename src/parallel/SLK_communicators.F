!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SLK_communicators(INTRA_world,INTER_world,N)
 !
 ! Here I have an example with 12 cpu's divided in two INTRA of 6 cpu's. In each INTRA I want to build-up a SLK structure.
 !
 !          MPI WORLD 
 !  ________________________
 !
 !    INTRA            
 !  ___________  
 !  0 0 0 0 0 0 0 0 0 0 0 0  
 !  |___________|
 !    INTER
 !
 ! PAR_COM_SLK is made of the N cpu's in INTRA 
 ! 
 ! Let's assume  N=4
 ! 
 !  PAR_COM_SLK PAR_COM_SLK
 !  _______     _______
 !  0 0 0 0 0 0 0 0 0 0 0 0  
 !  |___________|
 !   PAR_COM_SLK_INDEX_global
 !
 ! In this case there is only one PAR_COM_SLK in INTRA so PAR_COM_SLK_INDEX_local is the serial COM.
 ! If there are two or more PAR_COM_SLK in INTRA PAR_COM_SLK_INDEX_local connects 1-to-1 the cpu's
 ! in PAR_COM_SLK's contained in INTRA.
 !
 ! Input:
 !
 ! 1. INTRA_world is the communicator among the cpu's of the local world containg the SLK instances.
 !    For example the single Q set of cpu's.
 !
 ! 2. INTER_world is the communicator among the heads of the INTRA_world. This refers to the MPI_WORLD.
 !
 ! 3. N is the number of CPU's (connected by the INTRA_world)  I want to use to build the single 
 !    SLK communicator (PAR_COM_SLK).
 !
 ! The heads of ALL PAR_COM_SLK's communicate trough PAR_COM_SLK_INDEX_global
 ! The heads of the PAR_COM_SLK's in the INTRA_world communicate trough PAR_COM_SLK_INDEX_local
 !
 ! Therefore the Output is:
 !
 ! a. PAR_COM_SLK
 ! b. PAR_COM_SLK_INDEX_global
 ! c. PAR_COM_SLK_INDEX_local
 !
 use parallel_m,    ONLY:yMPI_comm,COMM_copy,PAR_COM_SLK,PAR_COM_SLK_INDEX_global,&
&                        PAR_COM_SLK_INDEX_local,PAR_COM_NULL,&
&                        PAR_COM_WORLD,myid,i_INTER_CHAIN,CREATE_the_COMM
 !
 implicit none
 !
 type(yMPI_comm) :: INTRA_world,INTER_world
 integer        :: N
 !
 ! First the simple cases where all (or none) CPU's of the "world" (/= from the MPI world) are used.
 !
 if (N==1.or.N==INTRA_world%n_CPU) then
   !
   call COMM_copy(INTRA_world ,PAR_COM_SLK)
   call COMM_copy(INTER_world ,PAR_COM_SLK_INDEX_global)
   ! 
   ! If all cpu's are used for SLK no further parallelization is possible
   call COMM_copy(PAR_COM_NULL,PAR_COM_SLK_INDEX_local)
   ! 
   ! If Linear Algebra is serail all cpus can be used for further parallelization
   if (N==1) call COMM_copy(INTER_world,PAR_COM_SLK_INDEX_local)
   !
   return
 endif
 !
 ! Second case is when INTRA_world%n_CPU is an integer multiple of N
 !
 if (mod(INTRA_world%n_CPU,N)==0) then
   !
   PAR_COM_SLK%my_CHAIN=myid/N
   call CREATE_the_COMM(PAR_COM_WORLD%COMM,PAR_COM_SLK,myid)
   !
   PAR_COM_SLK_INDEX_global%my_CHAIN=PAR_COM_SLK%CPU_id
   call CREATE_the_COMM(PAR_COM_WORLD%COMM,PAR_COM_SLK_INDEX_global,myid)
   !
   PAR_COM_SLK_INDEX_local%my_CHAIN =i_INTER_CHAIN( INTRA_world%n_CPU, INTRA_world%n_CPU/N)
   call CREATE_the_COMM(PAR_COM_WORLD%COMM,PAR_COM_SLK_INDEX_local,myid)
   !
 endif
 !
end subroutine
