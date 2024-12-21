!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine PARALLEL_global_indexes_rt(E,Xk,q,ENVIRONMENT,X,Dip,RESET,Dip_limits_pre_defined)
 !
 ! LOGICALS
 use parallel_m,    ONLY:l_par_X_T,l_par_RT,l_par_SE,l_par_X_G,l_par_X_G_finite_q,&
&                        l_par_DIP,l_par_BZINDX,l_par_NL,l_par_SC,l_par_OSCLL
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
 use drivers,       ONLY:l_eval_collisions
 use DIPOLES,       ONLY:DIPOLE_t
 use X_m,           ONLY:X_t,l_X_terminator
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use pars,          ONLY:SP
 !
#include<y_memory.h>
 !
 type(levels)               ::E
 type(bz_samp)              ::Xk,q
 character(*)               ::ENVIRONMENT
 type(X_t),     optional    ::X
 type(DIPOLE_t),optional    ::Dip
 logical,       optional    ::RESET,Dip_limits_pre_defined
 !
 ! Work Space
 !
 logical              :: Do_dip_limits
 integer              :: X_type
 character(8)         :: MORE
 !
 !========================
 ! Response function kind
 !========================
 X_type=1
 if (present(X))                      X_type=X%whoami
 !
 MORE=""
 !========
 ! Resets
 !========
 if (present(RESET)) then
   if (RESET) then
     call PARALLEL_global_reset(ENVIRONMENT,X_type)
     return
   endif
 else
   call PARALLEL_global_reset(ENVIRONMENT,X_type)
 endif
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
 l_par_RT           =ENVIRONMENT=="Real_Time"
 l_par_SC           =ENVIRONMENT=="Self_Energy"
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
 if (present(X)) PAR_X_ib=X%ib
 if (present(X)) PAR_X_iq=X%iq
 !
 if (present(Dip)) then
   Do_dip_limits=.TRUE.
   if (present( Dip_limits_pre_defined )) then
     Do_dip_limits=.not.Dip_limits_pre_defined
   endif
   if (Do_dip_limits) then
     if (Dip%bands_ordered.or.Dip%Energy_treshold<0._SP) then
       Dip%ib_lim(1)=maxval(E%nbm)
       Dip%ib_lim(2)=minval(E%nbf)+1
       if (l_X_terminator) Dip%ib_lim(2)=Dip%ib(1)
     else
       Dip%ib_lim(1)=Dip%ib(2)
       Dip%ib_lim(2)=Dip%ib(1)
     endif
   endif
   PAR_Dip_ib=Dip%ib
   PAR_Dip_ib_lim=Dip%ib_lim
   if (trim(Dip%approach)=="Shifted grids") MORE="_SHIFTED"
 endif
 !
 call PARALLEL_global_dimensions_rt(E,Xk,q,ENVIRONMENT)
 !
 !==========
 ! DEFAULTS
 !==========
 !
 CALL PARALLEL_global_defaults(ENVIRONMENT//trim(MORE))
 !
 if (      ENVIRONMENT=="Real_Time")           call PARALLEL_global_Real_Time(E,Xk,q,X_type)
 !
end subroutine
