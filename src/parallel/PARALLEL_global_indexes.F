!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine PARALLEL_global_indexes(E,Xk,q,ENVIRONMENT,X,Dip,RESET,Dip_limits_pre_defined)
 !
 ! LOGICALS
 use parallel_m,    ONLY:l_par_X_T,l_par_RT,l_par_SE,l_par_X_G,l_par_X_G_finite_q,&
&                        l_par_DIP,l_par_BZINDX,l_par_NL,l_par_SC
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
 use DIPOLES,       ONLY:DIPOLE_t
 use X_m,           ONLY:X_t,l_X_terminator
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use pars,          ONLY:SP
 !
#include<memory.h>
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
 if (ENVIRONMENT=="Response_T_space") X_type=5
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
 l_par_BZINDX       =ENVIRONMENT=="BZ_Indexes"
 l_par_DIP          =ENVIRONMENT=="DIPOLES"
 l_par_X_T          =ENVIRONMENT=="Response_T_space"
 l_par_X_G_finite_q =ENVIRONMENT=="Response_G_space_and_IO"
 l_par_X_G          =ENVIRONMENT=="Response_G_space"
 l_par_SE           =ENVIRONMENT=="Self_Energy"
 l_par_RT           =ENVIRONMENT=="Real_Time"
 l_par_NL           =ENVIRONMENT=="Non_Linear"
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
 call PARALLEL_global_dimensions(E,Xk,q,ENVIRONMENT)
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
 if (index(ENVIRONMENT, "Response_G_space")>0) call PARALLEL_global_Response_G(E,Xk,q,ENVIRONMENT,X_type)
 !
 if (      ENVIRONMENT=="BZ_Indexes")          call PARALLEL_global_BZINDX(E,Xk,q,ENVIRONMENT)
 !
 if (      ENVIRONMENT=="DIPOLES")             call PARALLEL_global_DIPOLES(E,Xk,q,ENVIRONMENT)
 !
 if (      ENVIRONMENT=="Self_Energy")         call PARALLEL_global_Self_Energy(E,Xk,q)
 !
 if (      ENVIRONMENT=="Response_T_space")    call PARALLEL_global_Response_T_base(Xk)
 !
#if defined _RT
 if (      ENVIRONMENT=="Real_Time")           call PARALLEL_global_Real_Time(E,Xk,q,X_type)
#endif
 !
#if defined _NL
 if (      ENVIRONMENT=="Non_Linear")          call PARALLEL_global_Non_Linear(E,Xk,q,ENVIRONMENT)
#endif
 !
#if defined _SCALAPACK
 if (      ENVIRONMENT=="ScaLapacK")           call PARALLEL_global_ScaLapack( )
#endif
 !
 !==============================================================================
 if ( ENVIRONMENT=="Response_G_space".or.ENVIRONMENT=="Response_G_space_and_IO") then
   !============================================================================
   !
   ! Response functions conduction bands
   !
   if (l_X_terminator) then
       call PARALLEL_index(PAR_IND_CON_BANDS_X(X_type),(/PAR_n_c_bands(2)/),low_range=(/PAR_n_v_bands(1)/),&
&                          COMM=PAR_COM_CON_INDEX_X(X_type),CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   else
       call PARALLEL_index(PAR_IND_CON_BANDS_X(X_type),(/PAR_n_c_bands(2)/),low_range=(/PAR_n_c_bands(1)/),&
&                          COMM=PAR_COM_CON_INDEX_X(X_type),CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   endif
   !
   PAR_IND_CON_BANDS_X_ID(X_type)=PAR_COM_CON_INDEX_X(X_type)%CPU_id
   !
   if (l_X_terminator) then
       call PARALLEL_live_message("CON bands",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_CON_BANDS_X(X_type)%n_of_elements(PAR_COM_CON_INDEX_X(X_type)%CPU_id+1),&
&                             TOTAL=PAR_n_c_bands(2)-PAR_n_v_bands(1)+1,&
&                             NCPU=PAR_COM_CON_INDEX_X(X_type)%n_CPU)
   else
       call PARALLEL_live_message("CON bands",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_CON_BANDS_X(X_type)%n_of_elements(PAR_COM_CON_INDEX_X(X_type)%CPU_id+1),&
&                             TOTAL=PAR_n_c_bands(2)-PAR_n_c_bands(1)+1,&
&                             NCPU=PAR_COM_CON_INDEX_X(X_type)%n_CPU)
   endif
   !
   ! Response functions valence bands
   !
   call PARALLEL_index(PAR_IND_VAL_BANDS_X(X_type),(/PAR_n_v_bands(2)/),low_range=(/PAR_n_v_bands(1)/),&
&                      COMM=PAR_COM_VAL_INDEX_X(X_type),CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   PAR_IND_VAL_BANDS_X_ID(X_type)=PAR_COM_VAL_INDEX_X(X_type)%CPU_id
   !
   call PARALLEL_live_message("VAL bands",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_VAL_BANDS_X(X_type)%n_of_elements(PAR_COM_VAL_INDEX_X(X_type)%CPU_id+1),&
&                             TOTAL=PAR_n_v_bands(2)-PAR_n_v_bands(1)+1,&
&                             NCPU=PAR_COM_VAL_INDEX_X(X_type)%n_CPU)
   !
 endif
 !
end subroutine
