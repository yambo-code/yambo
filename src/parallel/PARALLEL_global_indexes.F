!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine PARALLEL_global_indexes(E,Xk,q,ENVIRONMENT,X,RESET)
 !
 use pars,          ONLY:IP
 use parallel_m,    ONLY:PP_indexes,PP_indexes_reset,MPI_comm,COMM_copy,n_c_bands,n_v_bands
 ! LOGICALS
 use parallel_m,    ONLY:l_par_X_T,l_par_X_G_q0,l_par_RT,l_par_SE,l_par_X_G_all_q,l_par_X_G_finite_q
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_CON_INDEX,PAR_COM_VAL_INDEX
 ! IND
 use parallel_m,    ONLY:PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X
 ! ID's
 use parallel_m,    ONLY:PAR_IND_CON_BANDS_X_ID,PAR_IND_VAL_BANDS_X_ID
 use interfaces,    ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use X_m,           ONLY:X_t,X_reset,X_duplicate,l_X_terminator
 use drivers,       ONLY:l_HF_and_locXC,l_life,l_elphoton_corr
#if defined _QED
 use parallel_m,    ONLY:l_par_SC
#endif
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use IO_m,          ONLY:IO_and_Messaging_switch
 use com,           ONLY:warning
 !
 implicit none
 !
 type(levels)               ::E
 type(bz_samp)              ::Xk,q
 character(*)               ::ENVIRONMENT
 type(X_t), optional        ::X
 logical,   optional        ::RESET
 !
 ! Work Space
 !
 type(X_t)            :: X_local
 integer              :: X_type
 logical              :: reset_all
 !
 ! Resets...
 !
 reset_all=.TRUE.
 if (present(RESET)) reset_all=RESET
 !
 call X_reset(X_local)
 if(present(X)) call X_duplicate(X,X_local)
 !
 X_type=1
 if (present(X)) X_type=X%whoami
 if (ENVIRONMENT=="Response_T_space") X_type=5
 !
 if (reset_all) then
   call PARALLEL_reset(Xk,ENVIRONMENT,X_type)
   if(present(RESET)) return
 endif
 !
 ! Logicals
 !
 l_par_X_T          =ENVIRONMENT=="Response_T_space"
 l_par_X_G_q0       =ENVIRONMENT=="Response_G_space_Zero_Momentum"
 l_par_X_G_finite_q =ENVIRONMENT=="Response_G_space_Finite_Momentum"
 l_par_X_G_all_q    =ENVIRONMENT=="Response_G_space"
 l_par_SE           =ENVIRONMENT=="Self_Energy"
 l_par_RT           =ENVIRONMENT=="Real_Time"
#if defined _QED
 l_par_SC           =ENVIRONMENT=="Self_Energy"
#endif
 !
 !==================================
 ! USER provided PARALLEL structure
 !==================================
 !
 call PARALLEL_get_user_structure(ENVIRONMENT,.TRUE.)
 !
 !============
 ! Dimensions
 !============
 !
 call PARALLEL_global_dimensions(E,Xk,q,ENVIRONMENT,X_local)
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
 if (      ENVIRONMENT=="Self_Energy")         call PARALLEL_global_Self_Energy(E,Xk,q,ENVIRONMENT,X_type)
 !
 if (      ENVIRONMENT=="Response_T_space")    call PARALLEL_global_Response_T(E,Xk,q,ENVIRONMENT,X_type)
 !
#if defined _RT
 if (      ENVIRONMENT=="Real_Time")           call PARALLEL_global_Real_Time(E,Xk,q,ENVIRONMENT)
#endif
 !
#if defined _SCALAPACK
 if (      ENVIRONMENT=="ScaLapacK")           call PARALLEL_global_ScaLapacK(E,Xk,q,ENVIRONMENT)
#endif
 !
 !======
 ! MORE
 !======
 if (ENVIRONMENT=="Response_G_space_Zero_Momentum".or.ENVIRONMENT=="Response_G_space_Finite_Momentum".or.&
&    ENVIRONMENT=="Response_G_space".or.ENVIRONMENT=="Response_T_space".or.&
&    (ENVIRONMENT=="Self_Energy".and.l_life.and.l_elphoton_corr) ) then
   !========================================================================================================
   !
   ! Response functions conduction bands
   !
   if (l_X_terminator) then
       call PARALLEL_index(PAR_IND_CON_BANDS_X(X_type),(/n_c_bands(2)/),low_range=(/n_v_bands(1)/),&
&                          COMM=PAR_COM_CON_INDEX(X_type),CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   else
       call PARALLEL_index(PAR_IND_CON_BANDS_X(X_type),(/n_c_bands(2)/),low_range=(/n_c_bands(1)/),&
&                          COMM=PAR_COM_CON_INDEX(X_type),CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   endif
   !
   PAR_IND_CON_BANDS_X_ID(X_type)=PAR_COM_CON_INDEX(X_type)%CPU_id
   !
   if (l_X_terminator) then
       call PARALLEL_live_message("CON bands",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_CON_BANDS_X(X_type)%n_of_elements(PAR_COM_CON_INDEX(X_type)%CPU_id+1),&
&                             TOTAL=n_c_bands(2)-n_v_bands(1)+1,&
&                             NCPU=PAR_COM_CON_INDEX(X_type)%n_CPU)
   else
       call PARALLEL_live_message("CON bands",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_CON_BANDS_X(X_type)%n_of_elements(PAR_COM_CON_INDEX(X_type)%CPU_id+1),&
&                             TOTAL=n_c_bands(2)-n_c_bands(1)+1,&
&                             NCPU=PAR_COM_CON_INDEX(X_type)%n_CPU)
   endif
   !
   ! Response functions valence bands
   !
   call PARALLEL_index(PAR_IND_VAL_BANDS_X(X_type),(/n_v_bands(2)/),low_range=(/n_v_bands(1)/),&
&                      COMM=PAR_COM_VAL_INDEX(X_type),CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   PAR_IND_VAL_BANDS_X_ID(X_type)=PAR_COM_VAL_INDEX(X_type)%CPU_id
   !
   call PARALLEL_live_message("VAL bands",ENVIRONMENT=ENVIRONMENT,&
&                             LOADED=PAR_IND_VAL_BANDS_X(X_type)%n_of_elements(PAR_COM_VAL_INDEX(X_type)%CPU_id+1),&
&                             TOTAL=n_v_bands(2)-n_v_bands(1)+1,&
&                             NCPU=PAR_COM_VAL_INDEX(X_type)%n_CPU)
   !
 endif
 !
end subroutine
