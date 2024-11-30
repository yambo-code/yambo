!
! License-Identifier: GPL
!
! Copyright (C) 2020 the YAMBO team
!
! Authors (see AUTHORS file for details): CA MG DS
!
subroutine PARALLEL_global_Oscillators(E,Xk,q)
 !
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
#if defined _SC || defined _RT || defined _NL
 use collision_ext, ONLY:COLL_bands
#endif
 use IO_int,        ONLY:IO_and_Messaging_switch
 use parallel_int,  ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use parallel_m,    ONLY:ncpu,COMM_copy,PAR_build_index,PAR_INDEX_copy
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_G_b_A2A,PAR_COM_Xk_ibz_INDEX,PAR_COM_WF_k_INDEX,PAR_G_bands_INDEX, &
 &                       PAR_COM_Xk_ibz_A2A,PAR_COM_G_b_INDEX,PAR_n_bands,n_WF_bands_to_load
 ! IND
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_G_b,PAR_IND_WF_k,PAR_IND_Xk_bz
 ! INDEX
 use parallel_m,    ONLY:PAR_Xk_ibz_index,PAR_Xk_bz_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_nG_bands,PAR_nQP,PAR_Xk_nbz,PAR_Xk_nibz
 ! ID's
 use parallel_m,    ONLY:PAR_IND_G_b_ID,PAR_IND_WF_k_ID,&
&                        PAR_IND_Xk_bz_ID,PAR_IND_Xk_ibz_ID
 use openmp,        ONLY:n_threads_OSCLL,OPENMP_set_threads
 use QP_m,          ONLY:QP_n_states,QP_table
 !
#include<y_memory.h>
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 !
 ! Work space
 !
 integer      :: nb_mat 
 !
 CALL PARALLEL_structure(2,(/"k ","b "/))
 !
 call PARALLEL_assign_chains_and_COMMs(2,COMM_index_1=PAR_COM_Xk_ibz_INDEX, &
&                                        COMM_index_2=PAR_COM_G_b_INDEX,&
&                                        COMM_A2A_1=PAR_COM_Xk_ibz_A2A)
 !
 ! The routine PARALLEL_assign_chains_and_COMMs cannot define COMMUNICATORS for internal
 ! A2A when there is no internal distribution
 !
if (PAR_COM_Xk_ibz_INDEX%n_CPU==1) then
   call COMM_copy(PAR_COM_Xk_ibz_A2A,PAR_COM_G_b_A2A)
 endif
 !
 ! K-points
 ! 
 call PARALLEL_index(PAR_IND_Xk_ibz,(/Xk%nibz/),COMM=PAR_COM_Xk_ibz_INDEX)
 PAR_IND_Xk_ibz_ID=PAR_COM_Xk_ibz_INDEX%CPU_id
 PAR_Xk_nibz=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1)
 call PARALLEL_live_message("K-points",ENVIRONMENT="Oscillators",         &
&                           LOADED=PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1), &
&                           TOTAL=Xk%nibz,NCPU=PAR_COM_Xk_ibz_INDEX%n_CPU)
 !
 YAMBO_ALLOC(PAR_Xk_ibz_index,(Xk%nibz))
 call PAR_build_index(PAR_IND_Xk_ibz,Xk%nibz,PAR_Xk_ibz_index,PAR_Xk_nibz)
 !
 ! Bands
 !
#if defined _SC || defined _RT || defined _NL
 nb_mat=(COLL_bands(2)-COLL_bands(1)+1)**2
 !
 n_WF_bands_to_load=PAR_n_bands(2)
 !
 call PARALLEL_index(PAR_IND_G_b,(/nb_mat/),COMM=PAR_COM_G_b_INDEX)
 PAR_IND_G_b_ID=PAR_COM_G_b_INDEX%CPU_id
 PAR_nG_bands=PAR_IND_G_b%n_of_elements(PAR_IND_G_b_ID+1)
 YAMBO_ALLOC(PAR_G_bands_INDEX,(nb_mat))
 call PAR_build_index(PAR_IND_G_b,nb_mat,PAR_G_bands_index,PAR_nG_bands)
 !
 call PARALLEL_live_message("OSCL bands",ENVIRONMENT="Oscillators",&
&                             LOADED=PAR_IND_G_b%n_of_elements(PAR_IND_G_b_ID+1),&
&                             TOTAL=nb_mat,NCPU=PAR_COM_G_b_INDEX%n_CPU)
#endif
 !
 ! Copy to WF
 !
 call COMM_copy(PAR_COM_Xk_ibz_INDEX,PAR_COM_WF_k_INDEX)
 PAR_IND_WF_k_ID=PAR_COM_WF_k_INDEX%CPU_id
 !
 ! Io and messaging
 !
 call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_G_b_INDEX%CPU_id==0)
 !
 call IO_and_Messaging_switch("+output",CONDITION=PAR_COM_G_b_INDEX%CPU_id==0)
 !
 call OPENMP_set_threads(n_threads_in=n_threads_OSCLL) 
 !
end subroutine PARALLEL_global_Oscillators
