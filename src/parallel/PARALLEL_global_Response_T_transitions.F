!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine PARALLEL_global_Response_T_transitions(Xk)
 !
 use R_lattice,     ONLY:bz_samp,nXkibz
 use BS,            ONLY:BS_nT_at_k,BS_nT_grps,BS_n_eh_spaces
 use IO_int,        ONLY:IO_and_Messaging_switch
 use parallel_int,  ONLY:PARALLEL_index,PARALLEL_live_message
 use parallel_m,    ONLY:PAR_n_c_bands,PAR_n_v_bands,PP_indexes_reset,master_cpu
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_eh_INDEX,PAR_COM_T_INDEX, &
&                        PAR_COM_CON_INDEX_DIP,PAR_COM_VAL_INDEX_DIP
 ! IND
 use parallel_m,    ONLY:PAR_IND_T_groups,PAR_IND_T_ordered,PAR_IND_eh,  &
&                        PAR_IND_CON_BANDS_DIP,PAR_IND_VAL_BANDS_DIP
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_BS_nT_col_grps
 ! ID's
 use parallel_m,    ONLY:PAR_IND_CON_BANDS_DIP_ID,PAR_IND_VAL_BANDS_DIP_ID
 !
#include<memory.h>
 !
 type(bz_samp)        :: Xk
 !
 ! Work space
 !
 integer              :: i_k
 !
 ! I/O privileges
 !
 call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
 !
 call IO_and_Messaging_switch("+output",CONDITION=master_cpu)
 !
 ! E/h pairs (k resolved)
 !
 ! In this part I distribute the eh transitions within each k. The COMM for this indexing is PAR_COM_eh_INDEX.
 ! I fill the PAR_IND_eh for all k in order to define the total number of Transition groups
 !
 if (allocated(PAR_IND_eh)) deallocate(PAR_IND_eh)
 allocate(PAR_IND_eh(nXkibz*BS_n_eh_spaces))
 !
 do i_k=1,nXkibz*BS_n_eh_spaces
   !
   call PP_indexes_reset(PAR_IND_eh(i_k))
   call PARALLEL_index(PAR_IND_eh(i_k),(/BS_nT_at_k(i_k)/),COMM=PAR_COM_eh_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.FALSE.)
   !
 enddo
 !
 ! Now I find calculate the total (BS_nT_grps) and cpu-restricted (PAR_BS_nT_grps) number of Transition groups.
 ! In this case the PAR_BS_nT_grps groups belong only to the columns of the kernel.
 !
 call PP_indexes_reset(PAR_IND_T_groups)
 call PARALLEL_Transitions_grouping( )
 !
 call PARALLEL_live_message("(e/h) Groups",ENVIRONMENT="Response_T_space",LOADED=PAR_BS_nT_col_grps,TOTAL=BS_nT_grps,&
&                           NCPU=PAR_COM_eh_INDEX%n_CPU)
 !
 ! Now each CPU of the PAR_COM_eh_INDEX has PAR_BS_nT_grps  groups of e/h pairs
 !
 ! The task now is to distribute the transitions:
 !  
 ! Group@k (among BS_nT_grps) ->Group'@p (among BS_nT_grps)
 !
 call PP_indexes_reset(PAR_IND_T_ordered)
 !
 call PARALLEL_index(PAR_IND_T_ordered,(/BS_nT_grps,BS_nT_grps/),COMM=PAR_COM_T_INDEX,&
&                    MASK=PAR_IND_T_groups%element_1D,ORDERED=.TRUE.,NO_EMPTIES=.FALSE.)
 !
 call PARALLEL_live_message("(e/h)->(e/h)' Transitions (ordered)",ENVIRONMENT="Response_T_space",&
&                           LOADED=PAR_IND_T_ordered%n_of_elements(PAR_COM_T_INDEX%CPU_id+1),&
&                           TOTAL=BS_nT_grps*(BS_nT_grps+1)/2,NCPU=PAR_COM_T_INDEX%n_CPU)
 !
 ! Linear Algebra setup moved in the solver_driver...
 !
 ! Conduction band
 !
 call PP_indexes_reset(PAR_IND_CON_BANDS_DIP)
 call PARALLEL_index(PAR_IND_CON_BANDS_DIP,(/PAR_n_c_bands(2)/),low_range=(/PAR_n_c_bands(1)/),&
&                     COMM=PAR_COM_CON_INDEX_DIP,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_CON_BANDS_DIP_ID=PAR_COM_CON_INDEX_DIP%CPU_id
 !   
 call PARALLEL_live_message("CON bands",ENVIRONMENT="Response_T_space",&
&                             LOADED=PAR_IND_CON_BANDS_DIP%n_of_elements(PAR_COM_CON_INDEX_DIP%CPU_id+1),&
&                             TOTAL=PAR_n_c_bands(2)-PAR_n_c_bands(1)+1,&
&                             NCPU=PAR_COM_CON_INDEX_DIP%n_CPU)
 !
 ! Response functions valence bands
 !
 call PP_indexes_reset(PAR_IND_VAL_BANDS_DIP)
 call PARALLEL_index(PAR_IND_VAL_BANDS_DIP,(/PAR_n_v_bands(2)/),low_range=(/PAR_n_v_bands(1)/),&
&                    COMM=PAR_COM_VAL_INDEX_DIP,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_VAL_BANDS_DIP_ID=PAR_COM_VAL_INDEX_DIP%CPU_id
 !
 call PARALLEL_live_message("VAL bands",ENVIRONMENT="Response_T_space",&
&                           LOADED=PAR_IND_VAL_BANDS_DIP%n_of_elements(PAR_COM_VAL_INDEX_DIP%CPU_id+1),&
&                           TOTAL=PAR_n_v_bands(2)-PAR_n_v_bands(1)+1,&
&                           NCPU=PAR_COM_VAL_INDEX_DIP%n_CPU)
 !  
end subroutine PARALLEL_global_Response_T_transitions
