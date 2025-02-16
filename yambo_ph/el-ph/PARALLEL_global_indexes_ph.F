!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine PARALLEL_global_indexes_ph(E,Xk,q,ENVIRONMENT)
 !
 ! LOGICALS
 use parallel_m,    ONLY:l_par_SE
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_CON_INDEX_X,PAR_COM_VAL_INDEX_X
 ! IND
 use parallel_m,    ONLY:PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X
 ! ID's
 use parallel_m,    ONLY:PAR_IND_CON_BANDS_X_ID,PAR_IND_VAL_BANDS_X_ID
 ! DIMENSIONS 
 use parallel_m,    ONLY:PAR_Dip_ib,PAR_Dip_ib_lim,PAR_X_ib,PAR_X_iq,PAR_n_c_bands,PAR_n_v_bands
 ! WORLDS
 use parallel_m,    ONLY:PAR_COM_WORLD,PAR_COM_X_WORLD,PAR_COM_X_WORLD_RL_resolved,COMM_copy
 use parallel_int,  ONLY:PARALLEL_index,PARALLEL_live_message
 use electrons,     ONLY:levels
 use drivers,       ONLY:l_elph_corr
 use ELPH,          ONLY:elph_use_q_grid
 use R_lattice,     ONLY:bz_samp
 !
#include<y_memory.h>
 !
 type(levels)               ::E
 type(bz_samp)              ::Xk,q
 character(*)               ::ENVIRONMENT
 !
 ! Work Space
 !
 logical              :: Do_dip_limits
 character(8)         :: MORE
 integer              :: X_type
 !
 X_type=1
 MORE=""
 call PARALLEL_global_reset(ENVIRONMENT,X_type)
 !
 !========
 ! WORLDS
 !========
 call COMM_copy(PAR_COM_WORLD,PAR_COM_X_WORLD)
 call COMM_copy(PAR_COM_WORLD,PAR_COM_X_WORLD_RL_resolved)
 !
 !==========
 ! Logicals
 !==========
 !
 l_par_SE           =index(ENVIRONMENT,"Self_Energy")>0
 !
 !==================================
 ! USER provided PARALLEL structure
 !==================================
 !
 !... via the ENVIRONMENT variable
 !
 call PARALLEL_get_ENVIRONMENT_structure(ENVIRONMENT)
 !
 !...via the input file
 !
 call PARALLEL_get_user_structure(ENVIRONMENT,.TRUE.)
 !
 !============
 ! Dimensions
 !============
 !
 call PARALLEL_global_dimensions_elph(E,Xk,q,ENVIRONMENT)
 !
 !==========
 ! DEFAULTS
 !==========
 !
 CALL PARALLEL_global_defaults(ENVIRONMENT//trim(MORE))
 !
 !==============================
 ! ENVIRONMENT DEPENDENT SCHEMES
 !==============================
 !
 if (l_par_SE) then
   MORE="ibz"
   if (l_elph_corr.and.     elph_use_q_grid)    MORE="bz"
   if (l_elph_corr.and..not.elph_use_q_grid)    MORE="RIM"
   call PARALLEL_global_Self_Energy(E,Xk,q,MORE)
 endif
 !
end subroutine
