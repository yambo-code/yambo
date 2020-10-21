!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use parallel_m,    ONLY:PP_indexes_reset,n_groups
 ! SLK
 use parallel_m,    ONLY:PAR_COM_RL_A2A,PAR_COM_RL_INDEX,PAR_COM_X_WORLD,PAR_COM_X_WORLD_RL_resolved
 use matrix,        ONLY:TRANSFER_world
 ! COMMUNICATORS
 use parallel_m,    ONLY:COMM_reset,&  
&                        PAR_COM_VAL_INDEX_X,PAR_COM_VAL_INDEX_DIP,PAR_COM_CON_INDEX_X,PAR_COM_CON_INDEX_DIP,&
&                        PAR_COM_VAL_INDEX_OVLP,PAR_COM_CON_INDEX_OVLP,PAR_COM_Q_A2A,&
&                        PAR_COM_Xk_ibz_INDEX,PAR_COM_Xk_ibz_A2A,PAR_COM_Q_INDEX,&
&                        PAR_COM_Xk_bz_INDEX,PAR_COM_SLK_INDEX_global,PAR_COM_SLK,&
&                        PAR_COM_QP_INDEX,PAR_COM_PLASMA_INDEX,PAR_COM_freqs_INDEX,PAR_COM_freqs,&
&                        PAR_COM_QP_A2A,PAR_COM_G_b_INDEX,PAR_COM_G_b_A2A,&
&                        PAR_COM_WF_k_A2A,PAR_COM_WF_b_INDEX,PAR_COM_WF_k_INDEX,&
&                        PAR_COM_DIPk_bz_A2A,PAR_COM_DIPk_ibz_A2A,PAR_COM_Xk_bz_A2A,PAR_COM_density,&
&                        PAR_COM_eh_INDEX,PAR_COM_eh_A2A,PAR_COM_T_INDEX,PAR_COM_SLK_INDEX_local,&
&                        PAR_PH_COM_Q_ibz_INDEX,PAR_PH_COM_Q_ibz_A2A,PAR_PH_COM_Xk_bz_INDEX,PAR_PH_COM_Xk_bz_A2A,&
&                        PAR_PH_COM_eh_bands_INDEX,PAR_PH_COM_eh_bands_A2A
 ! IND
 use parallel_m,    ONLY:PAR_IND_Xk_ibz,PAR_IND_CON_BANDS_X,PAR_IND_Xk_bz,&
&                        PAR_IND_VAL_BANDS_X,PAR_IND_Q_ibz,PAR_IND_Q_bz,PAR_IND_SLK,&
&                        PAR_IND_QP,PAR_IND_G_b,PAR_IND_B_mat_ordered,PAR_IND_WF_b,PAR_IND_WF_k,&
&                        PAR_IND_B_mat,PAR_IND_Plasma,PAR_IND_WF_linear,&
&                        PAR_IND_Bp_mat,PAR_IND_G_k,PAR_IND_eh,PAR_IND_WF_b_and_k,&
&                        PAR_IND_T_groups,PAR_IND_Kk_ibz,PAR_IND_T_ordered,PAR_IND_freqs,&
&                        PAR_IND_CON_BANDS_DIP,PAR_IND_VAL_BANDS_DIP,PAR_IND_CON_BANDS_OVLP,PAR_IND_VAL_BANDS_OVLP,&
&                        PAR_PH_IND_Q_ibz,PAR_PH_IND_Xk_bz
 ! INDEX
 use parallel_m,    ONLY:PAR_B_mat_index,PAR_Bp_mat_index,PAR_QP_index,PAR_G_bands_index,PAR_PLASMA_index, &
&                        PAR_Q_ibz_index,PAR_Q_bz_index,PAR_Xk_ibz_index,PAR_Xk_bz_index,PAR_DIPk_ibz_index,   &
&                        PAR_BS_T_grps_index,PAR_RL_index,PAR_COM_DIPk_bz_INDEX,PAR_COM_DIPk_ibz_INDEX,    &
&                        PAR_freqs_index
 ! ID's
 use parallel_m,    ONLY:PAR_IND_CON_BANDS_X_ID,PAR_IND_VAL_BANDS_X_ID,PAR_IND_Xk_bz_ID,&
&                        PAR_IND_Xk_ibz_ID,PAR_IND_Q_ibz_ID,PAR_IND_Q_bz_ID,PAR_IND_CON_BANDS_DIP_ID,&
&                        PAR_IND_VAL_BANDS_DIP_ID,PAR_IND_VAL_BANDS_OVLP_ID,PAR_IND_CON_BANDS_OVLP_ID,&
&                        PAR_IND_QP_ID,PAR_IND_G_b_ID,PAR_IND_PLASMA_ID,&
&                        PAR_IND_WF_k_ID,PAR_IND_WF_b_ID,PAR_IND_B_mat_ID,&
&                        PAR_IND_Bp_mat_ID,PAR_IND_G_k_ID,PAR_IND_freqs_ID,&
&                        PAR_PH_IND_Q_ibz_ID,PAR_PH_IND_Xk_bz_ID,PAR_PH_IND_eh_bands_ID
#if defined _RT || defined _SC || defined _QED || defined _NL
 use collision_ext, ONLY:COH_collisions,HXC_collisions,GW_NEQ_collisions,P_collisions
#endif
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
 call PP_indexes_reset(PAR_IND_Q_ibz)
 call PP_indexes_reset(PAR_IND_Q_bz)
 call PP_indexes_reset(PAR_IND_T_groups)
 call PP_indexes_reset(PAR_IND_Kk_ibz)
 call PP_indexes_reset(PAR_IND_Xk_ibz)
 call PP_indexes_reset(PAR_IND_Xk_bz)
 call PP_indexes_reset(PAR_IND_CON_BANDS_X(X_type))
 call PP_indexes_reset(PAR_IND_VAL_BANDS_X(X_type))
 call PP_indexes_reset(PAR_IND_CON_BANDS_DIP)
 call PP_indexes_reset(PAR_IND_VAL_BANDS_DIP)
 call PP_indexes_reset(PAR_IND_CON_BANDS_OVLP)
 call PP_indexes_reset(PAR_IND_VAL_BANDS_OVLP)
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
 call PP_indexes_reset(PAR_IND_freqs)
 call PP_indexes_reset(PAR_PH_IND_Q_ibz)
 call PP_indexes_reset(PAR_PH_IND_Xk_bz)
 !
 if (ENVIRONMENT=="Response_T_space") then
   if (.not.allocated(PAR_IND_eh)) allocate(PAR_IND_eh(nXkibz))
   do i_k=1,nXkibz
     call PP_indexes_reset(PAR_IND_eh(i_k))
   enddo
 endif
 !
 YAMBO_FREE(PAR_B_mat_index)
 YAMBO_FREE(PAR_Bp_mat_index)
 YAMBO_FREE(PAR_QP_index)
 YAMBO_FREE(PAR_G_bands_index)
 YAMBO_FREE(PAR_PLASMA_index)
 YAMBO_FREE(PAR_Q_ibz_index)
 YAMBO_FREE(PAR_Q_bz_index)
 YAMBO_FREE(PAR_Xk_ibz_index)
 YAMBO_FREE(PAR_Xk_bz_index)
 YAMBO_FREE(states_to_load)
 YAMBO_FREE(PAR_DIPk_ibz_index)
 YAMBO_FREE(PAR_BS_T_grps_index)
 YAMBO_FREE(PAR_RL_index)
 YAMBO_FREE(PAR_freqs_index)
 !
 PAR_IND_Q_ibz_ID=0
 PAR_IND_Q_bz_ID=0
 PAR_IND_Xk_ibz_ID=0
 PAR_IND_Xk_bz_ID=0
 PAR_IND_CON_BANDS_X_ID(X_type)=0
 PAR_IND_VAL_BANDS_X_ID(X_type)=0
 PAR_IND_CON_BANDS_DIP_ID=0
 PAR_IND_VAL_BANDS_DIP_ID=0
 PAR_IND_CON_BANDS_OVLP_ID=0
 PAR_IND_VAL_BANDS_OVLP_ID=0
 PAR_IND_QP_ID=0
 PAR_IND_PLASMA_ID=0
 PAR_IND_G_b_ID=0
 PAR_IND_G_k_ID=0
 PAR_IND_WF_b_ID=0
 PAR_IND_WF_k_ID=0
 PAR_IND_B_mat_ID=0
 PAR_IND_Bp_mat_ID=0
 PAR_IND_freqs_ID=0
 PAR_PH_IND_Q_ibz_ID=0
 PAR_PH_IND_Xk_bz_ID=0
 PAR_PH_IND_eh_bands_ID=0
 !
 call COMM_reset(PAR_COM_VAL_INDEX_X(X_type))
 call COMM_reset(PAR_COM_CON_INDEX_X(X_type))
 call COMM_reset(PAR_COM_Xk_ibz_INDEX)
 call COMM_reset(PAR_COM_Xk_ibz_A2A)
 call COMM_reset(PAR_COM_Xk_bz_INDEX)
 call COMM_reset(PAR_COM_Xk_bz_A2A)
 call COMM_reset(PAR_COM_Q_INDEX)
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
 call COMM_reset(PAR_COM_VAL_INDEX_DIP)
 call COMM_reset(PAR_COM_CON_INDEX_DIP)
 call COMM_reset(PAR_COM_VAL_INDEX_OVLP)
 call COMM_reset(PAR_COM_CON_INDEX_OVLP)
 call COMM_reset(PAR_COM_DIPk_bz_INDEX)
 call COMM_reset(PAR_COM_DIPk_bz_A2A)
 call COMM_reset(PAR_COM_DIPk_ibz_INDEX)
 call COMM_reset(PAR_COM_DIPk_ibz_A2A)
 call COMM_reset(PAR_COM_X_WORLD_RL_resolved)
 call COMM_reset(PAR_COM_X_WORLD)
 call COMM_reset(PAR_COM_density)
 call COMM_reset(PAR_COM_eh_INDEX)
 call COMM_reset(PAR_COM_RL_INDEX)
 call COMM_reset(PAR_COM_RL_A2A)
 call COMM_reset(PAR_COM_eh_A2A)
 call COMM_reset(PAR_COM_T_INDEX)
 call COMM_reset(PAR_COM_SLK_INDEX_local)
 call COMM_reset(TRANSFER_world)
#if defined _SCALAPACK
 call COMM_reset(PAR_COM_SLK)
 call COMM_reset(PAR_COM_SLK_INDEX_global)
 call COMM_reset(SLK_COM_INDEX(1))
 call COMM_reset(SLK_COM_INDEX(2))
 call COMM_reset(SLK_COM_A2A(1))
 call COMM_reset(SLK_COM_A2A(2))
#endif
 call COMM_reset(PAR_COM_freqs_INDEX)
 call COMM_reset(PAR_COM_freqs)
#if defined _RT || defined _SC || defined _QED || defined _NL
 call PARALLEL_collisions_reset(HXC_collisions)
 call PARALLEL_collisions_reset(COH_collisions)
 call PARALLEL_collisions_reset(GW_NEQ_collisions)
 call PARALLEL_collisions_reset(P_collisions)
#endif
 call COMM_reset(PAR_PH_COM_Q_ibz_INDEX)
 call COMM_reset(PAR_PH_COM_Q_ibz_A2A)
 call COMM_reset(PAR_PH_COM_Xk_bz_INDEX)
 call COMM_reset(PAR_PH_COM_Xk_bz_A2A)
 call COMM_reset(PAR_PH_COM_eh_bands_INDEX)
 call COMM_reset(PAR_PH_COM_eh_bands_A2A)
 !
 n_groups=0
 !
 call OPENMP_set_threads( )
 !
end subroutine PARALLEL_global_reset
