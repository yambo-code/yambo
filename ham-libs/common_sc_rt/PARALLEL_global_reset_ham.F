!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_global_reset_ham(ENVIRONMENT,X_type)
 !
 use R_lattice,     ONLY:bz_samp
 use parallel_m,    ONLY:PP_indexes_reset
 ! COMMUNICATORS
 use parallel_m,    ONLY:COMM_reset,&  
&                        PAR_COM_PLASMA_INDEX
 ! IND
 use parallel_m,    ONLY:PAR_IND_B_mat_ordered,PAR_IND_B_mat,PAR_IND_Plasma,&
&                        PAR_IND_Bp_mat
 ! INDEX
 use parallel_m,    ONLY:PAR_PLASMA_index
 ! ID's
 use parallel_m,    ONLY:PAR_IND_PLASMA_ID
 !
 use collision_ext, ONLY:COH_collisions,HXC_collisions,GW_NEQ_collisions,P_collisions
 !
#include <y_memory.h>
 !
 type(bz_samp)        :: Xk
 character(*)         :: ENVIRONMENT
 integer              :: X_type
 !
 ! Work Space
 !
 call PARALLEL_global_reset(ENVIRONMENT,X_type)
 !
 call PP_indexes_reset(PAR_IND_Plasma)
 call PP_indexes_reset(PAR_IND_B_mat)
 call PP_indexes_reset(PAR_IND_Bp_mat)
 call PP_indexes_reset(PAR_IND_B_mat_ordered)
 !
 YAMBO_FREE(PAR_PLASMA_index)
 !
 PAR_IND_PLASMA_ID=0
 !
 call COMM_reset(PAR_COM_PLASMA_INDEX)
 !
 call PARALLEL_collisions_reset(HXC_collisions)
 call PARALLEL_collisions_reset(COH_collisions)
 call PARALLEL_collisions_reset(GW_NEQ_collisions)
 call PARALLEL_collisions_reset(P_collisions)
 !
end subroutine PARALLEL_global_reset_ham
