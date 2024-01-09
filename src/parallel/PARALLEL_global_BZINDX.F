!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine PARALLEL_global_BZINDX(E,Xk,q,ENVIRONMENT)
 !
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use IO_int,        ONLY:IO_and_Messaging_switch
 use openmp,        ONLY:n_threads_BZINDX,OPENMP_set_threads
 use parallel_int,  ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use parallel_m,    ONLY:PAR_build_index
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_BZINDXk_ibz_A2A,PAR_COM_BZINDXk_ibz_INDEX
 ! IND
 use parallel_m,    ONLY:PAR_IND_BZINDXk_ibz,PAR_IND_BZINDXk_bz
 ! INDEX
 use parallel_m,    ONLY:PAR_BZINDXk_ibz_index,PAR_BZINDXk_bz_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_BZINDXk_nibz,PAR_BZINDXk_nbz
 ! ID's
 use parallel_m,    ONLY:PAR_IND_BZINDXk_bz_ID,PAR_IND_BZINDXk_ibz_ID
 !
 implicit none
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 character(*)         :: ENVIRONMENT
 !
 ! Work space
 !
 call PARALLEL_structure(1,(/"k"/))
 !
 call PARALLEL_assign_chains_and_COMMs(1,COMM_index_1=PAR_COM_BZINDXk_ibz_INDEX,&
&                                        COMM_A2A_1=PAR_COM_BZINDXk_ibz_A2A)
 !
 ! K-points (IBZ)
 !
 call PARALLEL_index(PAR_IND_BZINDXk_ibz,(/Xk%nibz/),COMM=PAR_COM_BZINDXk_ibz_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_BZINDXk_ibz_ID=PAR_COM_BZINDXk_ibz_INDEX%CPU_id
 !
 ! ... indexes
 !
 allocate(PAR_BZINDXk_bz_index(Xk%nbz))
 call PARALLEL_distribute_BZk_using_IBZk(PAR_COM_BZINDXk_ibz_INDEX,Xk,PAR_IND_BZINDXk_ibz,PAR_IND_BZINDXk_ibz_ID,&
&                                                         PAR_IND_BZINDXk_bz, PAR_IND_BZINDXk_bz_ID,&
&                                                         PAR_BZINDXk_bz_index,PAR_BZINDXk_nbz)
 !
 allocate(PAR_BZINDXk_ibz_index(Xk%nibz))
 call PAR_build_index(PAR_IND_BZINDXk_ibz,Xk%nibz,PAR_BZINDXk_ibz_index,PAR_BZINDXk_nibz)
 !
 call PARALLEL_live_message("K(ibz)",ENVIRONMENT=ENVIRONMENT,LOADED=PAR_BZINDXk_nibz,TOTAL=Xk%nibz,&
&                             NCPU=PAR_COM_BZINDXk_ibz_INDEX%n_CPU)
 !
 ! I/O privileges (for now just one structur exists)
 !
 call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_BZINDXk_ibz_A2A%CPU_id==0)
 !
 call OPENMP_set_threads(n_threads_in=n_threads_BZINDX)
 !
end subroutine PARALLEL_global_BZINDX
