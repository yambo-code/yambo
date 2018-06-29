!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine PARALLEL_global_dimensions(E,Xk,q,ENVIRONMENT)
 !
 use com,           ONLY:secnm
 use parallel_m,    ONLY:PAR_Q_range,PAR_QP_range,PAR_n_v_bands,PAR_n_c_bands,PAR_K_range,PAR_n_bands,&
&                        PAR_EH_range,PAR_X_ib,PAR_X_iq,PAR_n_freqs,PAR_n_G_vectors
 use drivers,       ONLY:l_eval_collisions,l_elph_corr,l_gw0,l_HF_and_locXC,l_life,l_sc_run
 use BS,            ONLY:BS_bands,BS_nT_at_k
 use X_m,           ONLY:X_ng
#if defined _ELPH
 use ELPH,          ONLY:elph_nDBs_used,QP_PH_n_G_bands,elph_use_q_grid
#endif
#if defined _NL
 use nl_optics,     ONLY:n_frequencies,NL_bands,eval_OVERLAPS
#endif
#if defined _RT
 use real_time,     ONLY:RT_bands
#endif
#if defined _SC || defined _RT
 use collision_ext, ONLY:COLL_bands,COH_collisions,P_collisions,HXC_collisions
#endif
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp,nqibz,nqbz
 use QP_m,          ONLY:QP_n_states,QP_nb,QP_n_G_bands
#include<memory.h>
 !
 type(levels)               ::E
 type(bz_samp)              ::Xk,q
 character(*)               ::ENVIRONMENT
 !
 ! Work Space
 !
 integer :: i_k  
 logical :: computing_Fock
 !
 computing_Fock= ( l_HF_and_locXC .and. index(secnm,"Bare local and non-local Exchange-Correlation")/=0 )
 !
 ! K-pts
 !-------
 PAR_K_range=Xk%nibz
 if (ENVIRONMENT=="Response_G_space_Zero_Momentum"  ) PAR_K_range=Xk%nibz
 if (ENVIRONMENT=="Response_T_space"                ) PAR_K_range=Xk%nibz
 if (ENVIRONMENT=="Real_Time"                       ) PAR_K_range=Xk%nibz
 if (ENVIRONMENT=="Non_Linear"                      ) PAR_K_range=Xk%nibz
 if (ENVIRONMENT=="Response_G_space"                ) PAR_K_range=Xk%nbz
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum") PAR_K_range=Xk%nbz
 !
 ! COND bands
 !------------
 if (ENVIRONMENT=="Response_G_space_Zero_Momentum"  ) PAR_n_c_bands= (/E%nbf+1,PAR_X_ib(2)/)
 if (ENVIRONMENT=="Response_G_space"                ) PAR_n_c_bands= (/E%nbf+1,PAR_X_ib(2)/)
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum") PAR_n_c_bands= (/E%nbf+1,PAR_X_ib(2)/)
 if (ENVIRONMENT=="Response_T_space"                ) PAR_n_c_bands= (/E%nbf+1,BS_bands(2)/)
 !
 ! VAL bands
 !-----------
 if (ENVIRONMENT=="Response_G_space_Zero_Momentum"  ) PAR_n_v_bands= (/PAR_X_ib(1),E%nbm/)
 if (ENVIRONMENT=="Response_G_space"                ) PAR_n_v_bands= (/PAR_X_ib(1),E%nbm/)
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum") PAR_n_v_bands= (/PAR_X_ib(1),E%nbm/)
 if (ENVIRONMENT=="Response_T_space"                ) PAR_n_v_bands= (/BS_bands(1),E%nbm/)
 !
 ! # G vectors 
 !-------------
 if (ENVIRONMENT=="Response_G_space_Zero_Momentum"  ) PAR_n_G_vectors= X_ng
 if (ENVIRONMENT=="Response_G_space"                ) PAR_n_G_vectors= X_ng
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum") PAR_n_G_vectors= X_ng
 !
 ! Q-pts
 !-------
 PAR_Q_range=(/1,nqibz/)
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum") PAR_Q_range=(/max(PAR_X_iq(1),2),PAR_X_iq(2)/)
 if (ENVIRONMENT=="Real_Time"                       ) PAR_Q_range=(/1,nqbz/)
 if (ENVIRONMENT=="Self_Energy"                     ) then
#if defined _SC || defined _RT
   if (l_eval_collisions)                       PAR_Q_range=(/1,nqbz/)
#endif
#if defined _ELPH
   if (l_elph_corr.and.elph_use_q_grid)         PAR_Q_range=(/1,nqbz/)
   if (l_elph_corr.and..not.elph_use_q_grid)    PAR_Q_range=(/1,elph_nDBs_used/)
#endif
 endif
 !
 ! EH-range
 !----------
 if (ENVIRONMENT=="Response_T_space"                ) then
   PAR_EH_range=maxval( BS_nT_at_k )
   do i_k=1,Xk%nibz
     if (BS_nT_at_k(i_k)<PAR_EH_range.and.BS_nT_at_k(i_k)>0) PAR_EH_range=BS_nT_at_k(i_k)
   enddo
 endif
 !
 ! Generic Bands
 !---------------
 if (ENVIRONMENT=="Self_Energy"                     ) then
   if (l_HF_and_locXC)                               PAR_n_bands=(/1,max(E%nbm,QP_nb)/)
   if ((l_gw0.or.l_life).and..not.computing_Fock)    PAR_n_bands=(/1,max(QP_n_G_bands(2),QP_nb)/)
   if (.not.computing_Fock)                          PAR_n_bands(1)=QP_n_G_bands(1)
#if defined _SC || defined _RT
   if (l_eval_collisions)  PAR_n_bands=COLL_bands
#endif
#if defined _ELPH
   if (l_elph_corr)        PAR_n_bands=(/1,QP_PH_n_G_bands/)
#endif
 endif
#if defined _RT
 if (ENVIRONMENT=="Real_Time"                       ) then
   PAR_n_bands=(/1,(RT_bands(2)-RT_bands(1)+1)**2/)
   PAR_QP_range=QP_n_states
 endif
#endif
#if defined _NL
 if (ENVIRONMENT=="Non_Linear") then 
   if(     eval_OVERLAPS) PAR_n_bands=(/    1,      NL_bands(2)/)
   if(.not.eval_OVERLAPS) PAR_n_bands=(/NL_bands(1),NL_bands(2)/)
 endif
#endif
 !
 ! QP states
 !-----------
 if (ENVIRONMENT=="Self_Energy"                     ) PAR_QP_range=QP_n_states
 !
#if defined _NL
 if (ENVIRONMENT=="Non_Linear"                      ) PAR_n_freqs =n_frequencies
#endif
 !
end subroutine PARALLEL_global_dimensions
