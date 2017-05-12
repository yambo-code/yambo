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
subroutine PARALLEL_global_reset(ENVIRONMENT,X_type)
 !
 use R_lattice,     ONLY:bz_samp,nXkibz
 use openmp,        ONLY:OPENMP_set_threads
 use wave_func,     ONLY:states_to_load
 use parallel_m,    ONLY:PP_indexes_reset
 ! COMMUNICATORS
 use parallel_m,    ONLY:COMM_reset,&  
&                        PAR_COM_VAL_INDEX,PAR_COM_CON_INDEX,PAR_COM_Q_A2A,&
&                        PAR_COM_Xk_ibz_INDEX,PAR_COM_Xk_ibz_A2A,PAR_COM_Q_INDEX,&
&                        PAR_COM_Xk_bz_INDEX,PAR_COM_SLK_INDEX_global,PAR_COM_SLK,&
&                        PAR_COM_QP_INDEX,PAR_COM_PLASMA_INDEX,PAR_COM_freqs_INDEX,PAR_COM_freqs,&
&                        PAR_COM_QP_A2A,PAR_COM_G_b_INDEX,PAR_COM_G_b_A2A,&
&                        PAR_COM_WF_k_A2A,PAR_COM_WF_b_INDEX,PAR_COM_WF_k_INDEX,&
&                        PAR_COM_DIPOLES,PAR_COM_DIPOLES_k_subgroup,PAR_COM_Xk_bz_A2A,PAR_COM_density,&
&                        PAR_COM_eh_INDEX,PAR_COM_eh_A2A,PAR_COM_T_INDEX,PAR_COM_SLK_INDEX_local
 ! IND
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_CON_BANDS_X,PAR_IND_Xk_bz,&
&                        PAR_IND_VAL_BANDS_X,PAR_IND_Q,PAR_IND_SLK,&
&                        PAR_IND_QP,PAR_IND_G_b,PAR_IND_B_mat_ordered,PAR_IND_WF_b,PAR_IND_WF_k,&
&                        PAR_IND_B_mat,PAR_IND_Plasma,PAR_IND_WF_linear,&
&                        PAR_IND_Bp_mat,PAR_IND_G_k,PAR_IND_eh,PAR_IND_WF_b_and_k,&
&                        PAR_IND_T_groups,PAR_IND_Kk_ibz,PAR_IND_T_ordered,PAR_IND_Q_bz,PAR_IND_freqs
 ! INDEX
 use parallel_m,    ONLY:PAR_B_mat_index,PAR_Bp_mat_index,PAR_QP_index,PAR_G_bands_index,PAR_PLASMA_index, &
&                        PAR_Q_index,PAR_Q_bz_index,PAR_Xk_ibz_index,PAR_Xk_bz_index,PAR_DIPk_ibz_index,   &
&                        PAR_BS_T_grps_index,PAR_RL_index,PAR_freqs_index
 ! ID's
 use parallel_m,    ONLY:PAR_IND_CON_BANDS_X_ID,PAR_IND_VAL_BANDS_X_ID,PAR_IND_Xk_bz_ID,&
&                        PAR_IND_Xk_ibz_ID,PAR_IND_Q_ID,&
&                        PAR_IND_QP_ID,PAR_IND_G_b_ID,PAR_IND_PLASMA_ID,&
&                        PAR_IND_WF_k_ID,PAR_IND_WF_b_ID,PAR_IND_B_mat_ID,&
&                        PAR_IND_Bp_mat_ID,PAR_IND_G_k_ID,PAR_IND_freqs_ID
#if defined _SCALAPACK
 use SLK_m,         ONLY:SLK_COM_INDEX,SLK_COM_A2A
#endif
 !
#include <memory.h>
 !
 type(bz_samp)        :: Xk
 character(*)         :: ENVIRONMENT
 integer              :: X_type
 !
 ! Work Space
 !
 integer              :: i_k
 !
 call PP_indexes_reset(PAR_IND_SLK)
 call PP_indexes_reset(PAR_IND_Q)
 call PP_indexes_reset(PAR_IND_Q_bz)
 call PP_indexes_reset(PAR_IND_T_groups)
 call PP_indexes_reset(PAR_IND_Kk_ibz)
 call PP_indexes_reset(PAR_IND_Xk_ibz)
 call PP_indexes_reset(PAR_IND_Xk_bz)
 call PP_indexes_reset(PAR_IND_CON_BANDS_X(X_type))
 call PP_indexes_reset(PAR_IND_VAL_BANDS_X(X_type))
 call PP_indexes_reset(PAR_IND_QP)
 call PP_indexes_reset(PAR_IND_Plasma)
 call PP_indexes_reset(PAR_IND_B_mat)
 call PP_indexes_reset(PAR_IND_Bp_mat)
 call PP_indexes_reset(PAR_IND_B_mat_ordered)
 call PP_indexes_reset(PAR_IND_G_b)
 call PP_indexes_reset(PAR_IND_G_k)
 call PP_indexes_reset(PAR_IND_WF_b)
 call PP_indexes_reset(PAR_IND_WF_k)
 call PP_indexes_reset(PAR_IND_WF_b_and_k)
 call PP_indexes_reset(PAR_IND_WF_linear)
 call PP_indexes_reset(PAR_IND_T_ordered)
#if defined _NL
 call PP_indexes_reset(PAR_IND_freqs)
#endif
 !
 if (ENVIRONMENT=="Response_T_space") then
   if (.not.allocated(PAR_IND_eh)) then
     allocate(PAR_IND_eh(nXkibz))
     do i_k=1,nXkibz
       call PP_indexes_reset(PAR_IND_eh(i_k))
     enddo
   endif
 endif
 !
 YAMBO_FREE(PAR_B_mat_index)
 YAMBO_FREE(PAR_Bp_mat_index)
 YAMBO_FREE(PAR_QP_index)
 YAMBO_FREE(PAR_G_bands_index)
 YAMBO_FREE(PAR_PLASMA_index)
 YAMBO_FREE(PAR_Q_index)
 YAMBO_FREE(PAR_Q_bz_index)
 YAMBO_FREE(PAR_Xk_ibz_index)
 YAMBO_FREE(PAR_Xk_bz_index)
 YAMBO_FREE(states_to_load)
 YAMBO_FREE(PAR_DIPk_ibz_index)
 YAMBO_FREE(PAR_BS_T_grps_index)
 YAMBO_FREE(PAR_RL_index)
#if defined _NL
 YAMBO_FREE(PAR_freqs_index)
#endif
 !
 PAR_IND_Q_ID=0
 PAR_IND_Xk_ibz_ID=0
 PAR_IND_Xk_bz_ID=0
 PAR_IND_CON_BANDS_X_ID(X_type)=0
 PAR_IND_VAL_BANDS_X_ID(X_type)=0
 PAR_IND_QP_ID=0
 PAR_IND_PLASMA_ID=0
 PAR_IND_G_b_ID=0
 PAR_IND_G_k_ID=0
 PAR_IND_WF_b_ID=0
 PAR_IND_WF_k_ID=0
 PAR_IND_B_mat_ID=0
 PAR_IND_Bp_mat_ID=0
#if defined _NL
 PAR_IND_freqs_ID=0
#endif
 !
 call COMM_reset(PAR_COM_VAL_INDEX(X_type))
 call COMM_reset(PAR_COM_CON_INDEX(X_type))
 call COMM_reset(PAR_COM_Xk_ibz_INDEX)
 call COMM_reset(PAR_COM_Q_INDEX)
 call COMM_reset(PAR_COM_Xk_bz_INDEX)
 call COMM_reset(PAR_COM_Xk_ibz_A2A)
 call COMM_reset(PAR_COM_Xk_bz_A2A)
 call COMM_reset(PAR_COM_Q_A2A)
 call COMM_reset(PAR_COM_SLK_INDEX_global)
 call COMM_reset(PAR_COM_SLK)
 call COMM_reset(PAR_COM_PLASMA_INDEX)
 call COMM_reset(PAR_COM_QP_INDEX)
 call COMM_reset(PAR_COM_QP_A2A)
 call COMM_reset(PAR_COM_G_b_INDEX)
 call COMM_reset(PAR_COM_G_b_A2A)
 call COMM_reset(PAR_COM_WF_k_A2A)
 call COMM_reset(PAR_COM_WF_b_INDEX)
 call COMM_reset(PAR_COM_WF_k_INDEX)
 call COMM_reset(PAR_COM_DIPOLES)
 call COMM_reset(PAR_COM_DIPOLES_k_subgroup)
 call COMM_reset(PAR_COM_density)
 call COMM_reset(PAR_COM_eh_INDEX)
 call COMM_reset(PAR_COM_eh_A2A)
 call COMM_reset(PAR_COM_T_INDEX)
 call COMM_reset(PAR_COM_SLK_INDEX_local)
#if defined _SCALAPACK
 call COMM_reset(PAR_COM_SLK)
 call COMM_reset(PAR_COM_SLK_INDEX_global)
 call COMM_reset(SLK_COM_INDEX(1))
 call COMM_reset(SLK_COM_INDEX(2))
 call COMM_reset(SLK_COM_A2A(1))
 call COMM_reset(SLK_COM_A2A(2))
#endif
#if defined _NL
 call COMM_reset(PAR_COM_freqs_INDEX)
 call COMM_reset(PAR_COM_freqs)
#endif
 !
 call OPENMP_set_threads( )
 !
end subroutine PARALLEL_global_reset
