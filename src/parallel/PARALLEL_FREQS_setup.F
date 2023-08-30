!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_FREQS_setup(W,IND,COMM)
 !
 use frequency,   ONLY:w_samp
 use parallel_m,  ONLY:PP_indexes,yMPI_comm,PAR_IND_FREQ_ID,PAR_n_freqs,PP_indexes_reset,&
&                      PAR_build_index,PAR_FREQS_index
 use parallel_int,ONLY:PARALLEL_index,PARALLEL_live_message
 !
#include<memory.h>
 !
 type(w_samp)    :: W
 type(PP_indexes):: IND
 type(yMPI_comm)  :: COMM
 !
 call PP_indexes_reset(IND)
 !
 ! ... and frequencies distribution
 !
 call PARALLEL_index(IND,(/W%n_freqs/),COMM=COMM,CONSECUTIVE=.TRUE.)
 PAR_IND_FREQ_ID=COMM%CPU_id+1
 call PARALLEL_live_message("X Frequencies",LOADED=IND%n_of_elements(PAR_IND_FREQ_ID),&
&                                           TOTAL=W%n_freqs,NCPU=COMM%n_CPU)
 PAR_n_freqs=IND%n_of_elements(PAR_IND_FREQ_ID)
 !
 if (PAR_n_freqs==0) return
 !
 YAMBO_ALLOC(PAR_FREQS_index,(PAR_n_freqs))
 call PAR_build_index(IND,PAR_n_freqs,PAR_FREQS_index,PAR_n_freqs)
 !
end subroutine
