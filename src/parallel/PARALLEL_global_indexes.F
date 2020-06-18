!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
!
! This file is distributed under the terms of the GNU
! General Public License. You can redistribute it and/or
! modify it under the terms of the GNU General Public
! License as published by the Free Software Foundation;
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will
! be useful, but WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A
! PARTICULAR PURPOSE.  See the GNU General Public License
! for more details.
!
! You should have received a copy of the GNU General Public
! License along with this program; if not, write to the Free
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine PARALLEL_global_indexes(E,Xk,q,ENVIRONMENT,X,Dip,RESET)
 !
 ! LOGICALS
 use parallel_m,    ONLY:l_par_X_T,l_par_RT,l_par_SE,l_par_X_G,l_par_X_G_finite_q,l_par_DIP,l_par_NL
#if defined _QED
 use parallel_m,    ONLY:l_par_SC
#endif
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
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_live_message
 use DIPOLES,       ONLY:DIPOLE_t
 use X_m,           ONLY:X_t,l_X_terminator
 use drivers,       ONLY:l_life,l_elphoton_corr
#if defined _QED
 use parallel_m,    ONLY:l_par_SC
#endif
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use pars,          ONLY:SP
 !
#include<memory.h>
 !
 type(levels)               ::E
 type(bz_samp)              ::Xk,q
 character(*)               ::ENVIRONMENT
 type(X_t), optional        ::X
 type(DIPOLE_t),optional    ::Dip
 logical,   optional        ::RESET
 !
 ! Work Space
 !
 integer              :: X_type
 !
 !========================
 ! Response function kind
 !========================
 X_type=1
 if (present(X)) then
   X_type=X%whoami
 endif
 if (ENVIRONMENT=="Response_T_space") X_type=5
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
 l_par_DIP          =ENVIRONMENT=="DIPOLES"
 l_par_X_T          =ENVIRONMENT=="Response_T_space"
 l_par_X_G_finite_q =ENVIRONMENT=="Response_G_space_and_IO"
 l_par_X_G          =ENVIRONMENT=="Response_G_space"
 l_par_SE           =ENVIRONMENT=="Self_Energy"
 l_par_RT           =ENVIRONMENT=="Real_Time"
 l_par_NL           =ENVIRONMENT=="Non_Linear"
#if defined _QED
 l_par_SC           =ENVIRONMENT=="Self_Energy"
#endif
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
   if (Dip%bands_ordered.or.Dip%Energy_treshold<0._SP) then
     Dip%ib_lim(1)=E%nbm
     Dip%ib_lim(2)=E%nbf+1
     if (l_X_terminator) Dip%ib_lim(2)=Dip%ib(1)
   else
     Dip%ib_lim(1)=Dip%ib(2)
     Dip%ib_lim(2)=Dip%ib(1)
   endif
   PAR_Dip_ib=Dip%ib
   PAR_Dip_ib_lim=Dip%ib_lim
 endif
 !
 call PARALLEL_global_dimensions(E,Xk,q,ENVIRONMENT)
 !
 !==========
 ! DEFAULTS
 !==========
 !
 CALL PARALLEL_global_defaults(ENVIRONMENT)
 !
 !==============================
 ! ENVIRONMENT DEPENDENT SCHEMES
 !==============================
 !
 if (index(ENVIRONMENT, "Response_G_space")>0) call PARALLEL_global_Response_G(E,Xk,q,ENVIRONMENT,X_type)
 !
 if (      ENVIRONMENT=="DIPOLES")             call PARALLEL_global_DIPOLES(E,Xk,q,ENVIRONMENT)
 !
 if (      ENVIRONMENT=="Self_Energy")         call PARALLEL_global_Self_Energy(E,Xk,q)
 !
 if (      ENVIRONMENT=="Response_T_space")    call PARALLEL_global_Response_T(Xk,X_type)
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
 !
 !==========================================================================================================
 if ( ENVIRONMENT=="Response_G_space".or.ENVIRONMENT=="Response_G_space_and_IO".or.&
&    (ENVIRONMENT=="Self_Energy".and.l_life.and.l_elphoton_corr) ) then
   !========================================================================================================
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
