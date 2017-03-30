!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine PARALLEL_global_dimensions(E,Xk,q,ENVIRONMENT,X,Dip)
 !
 use electrons,     ONLY:levels
 use X_m,           ONLY:X_t
 use DIPOLES,       ONLY:DIPOLE_t
 use com,           ONLY:secnm
 use R_lattice,     ONLY:bz_samp,nqibz,nqbz
 use BS,            ONLY:BS_bands,BS_nT_at_k
 use parallel_m,    ONLY:Q_range,QP_range,n_v_bands,n_c_bands,K_range,n_bands,EH_range
 use drivers,       ONLY:l_gw0,l_HF_and_locXC,l_life
 use QP_m,          ONLY:QP_nb,QP_n_G_bands,QP_n_states
#if defined _ELPH
 use drivers,       ONLY:l_elph_corr
 use ELPH,          ONLY:elph_nDBs_used,QP_PH_n_G_bands,elph_use_q_grid
#endif
#if defined _SC
 use drivers,       ONLY:l_sc_run
 use SC,            ONLY:SC_bands
 use collision_ext, ONLY:P_collisions,COH_collisions,HXC_collisions
#endif
#if defined _RT
 use real_time,     ONLY:RT_bands
#endif
#if defined _SC || defined _RT
 use drivers,       ONLY:l_eval_collisions
 use collision_ext, ONLY:COLL_bands
#endif
 !
 implicit none
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 type(X_t)            :: X
 type(DIPOLE_t)       :: Dip
 character(*)         :: ENVIRONMENT
 !
 ! Workspace
 !
 logical              :: computing_Fock
 integer              :: i_k
 !
 !
 computing_Fock= ( l_HF_and_locXC .and. index(secnm,"Bare local and non-local Exchange-Correlation")/=0 )
 !
 ! K-pts
 !-------
 K_range=Xk%nibz
 if (ENVIRONMENT=="DIPOLES")                          K_range=Xk%nibz
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum") K_range=Xk%nbz
 if (ENVIRONMENT=="Response_G_space")                 K_range=Xk%nbz
 if (ENVIRONMENT=="Response_T_space")                 K_range=Xk%nibz
 if (ENVIRONMENT=="Real_Time"       )                 K_range=Xk%nibz
 !
 ! COND bands
 !------------
 if (ENVIRONMENT=="DIPOLES")                          n_c_bands= (/E%nbf+1,Dip%ib(2)/)
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum") n_c_bands= (/E%nbf+1,X%ib(2)/)
 if (ENVIRONMENT=="Response_G_space")                 n_c_bands= (/E%nbf+1,X%ib(2)/)
 if (ENVIRONMENT=="Response_T_space")                 n_c_bands= (/E%nbf+1,BS_bands(2)/)
 !
 ! VAL bands
 !-----------
 if (ENVIRONMENT=="DIPOLES")                          n_v_bands= (/Dip%ib(1),E%nbm/)
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum") n_v_bands= (/X%ib(1),E%nbm/)
 if (ENVIRONMENT=="Response_G_space")                 n_v_bands= (/X%ib(1),E%nbm/)
 if (ENVIRONMENT=="Response_T_space")                 n_v_bands= (/BS_bands(1),E%nbm/)
 !
 ! Q-pts
 !-------
 Q_range=(/1,nqibz/)
 if (ENVIRONMENT=="Response_G_space_Finite_Momentum") Q_range=(/X%iq(1),X%iq(2)/)
 if (ENVIRONMENT=="Real_Time"       )                 Q_range=(/1,nqbz/)
 if (ENVIRONMENT=="Self_Energy"     ) then
#if defined _SC
   if (l_sc_run.and.&
&      any( (/P_collisions%N,COH_collisions%N,HXC_collisions%N/)> 0))  &
&                                                     Q_range=(/1,nqbz/)
#endif
#if defined _SC || defined _RT
   if (l_eval_collisions)                             Q_range=(/1,nqbz/)
#endif
#if defined _ELPH
   if (l_elph_corr.and.elph_use_q_grid)               Q_range=(/1,nqbz/)
   if (l_elph_corr.and..not.elph_use_q_grid)          Q_range=(/1,elph_nDBs_used/)
#endif
 endif
 !
 !
 ! EH-range
 !----------
 if (ENVIRONMENT=="Response_T_space"                ) then
   EH_range=maxval( BS_nT_at_k )
   do i_k=1,Xk%nibz
     if (BS_nT_at_k(i_k)<EH_range.and.BS_nT_at_k(i_k)>0) EH_range=BS_nT_at_k(i_k)
   enddo
 endif
 !
 ! Generic Bands
 !---------------
 if (ENVIRONMENT=="Self_Energy"                     ) then
   if (l_HF_and_locXC)                               n_bands=(/1,max(E%nbm,QP_nb)/)
   if ((l_gw0.or.l_life).and..not.computing_Fock)    n_bands=(/1,max(QP_n_G_bands(2),QP_nb)/)
   if (.not.computing_Fock)                          n_bands(1)=QP_n_G_bands(1)
#if defined _SC || defined _RT
   if (l_eval_collisions)                            n_bands=COLL_bands
#endif
#if defined _SC
   if (l_sc_run)                                     n_bands=SC_bands
#endif
#if defined _ELPH
   if (l_elph_corr)                                  n_bands=(/1,QP_PH_n_G_bands/)
#endif
 endif
#if defined _RT
 if (ENVIRONMENT=="Real_Time"                       ) n_bands=(/1,(RT_bands(2)-RT_bands(1)+1)**2/)
#endif
 !
 ! QP states
 !-----------
 if (ENVIRONMENT=="Self_Energy"                     ) QP_range=QP_n_states
 !
end subroutine PARALLEL_global_dimensions
