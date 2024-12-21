!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine PARALLEL_global_dimensions_nl(E,Xk,q,ENVIRONMENT)
 !
 use com,           ONLY:secnm
 use parallel_m,    ONLY:PAR_Q_bz_range,PAR_Q_ibz_range,PAR_QP_range,PAR_n_v_bands,PAR_n_c_bands,&
&                        PAR_EH_range,PAR_Dip_ib,PAR_Dip_ib_lim,PAR_X_ib,PAR_X_iq,PAR_n_G_vectors,&
&                        PAR_n_freqs,PAR_K_range,PAR_n_bands
 use drivers,       ONLY:l_eval_collisions,l_elph_corr,l_gw0,l_HF_and_locXC,l_life,l_sc_run,l_nl_optics
 use BS,            ONLY:BS_bands,BS_nT_at_k,BS_n_eh_spaces
 use X_m,           ONLY:X_ng
 use collision_ext, ONLY:COLL_bands
 use nl_optics,     ONLY:n_runs
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp,nqibz,nqbz
 use QP_m,          ONLY:QP_n_states,QP_nb,QP_n_G_bands
 !
#include<y_memory.h>
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 character(*)         :: ENVIRONMENT
 !
 if (.not.(ENVIRONMENT=="Non_Linear".or.ENVIRONMENT=="Oscillators")) call errore("PARALLEL_global_dimensions_rt: wrong environment")
 !
 ! K-pts
 !-------
 PAR_K_range=Xk%nibz
 !
 ! Q-pts
 !-------
 PAR_Q_ibz_range=(/1,nqibz/)
 PAR_Q_bz_range=(/1,nqbz/)
 !
 ! Generic Bands
 !---------------
 !
 ! Oscillators
 if (ENVIRONMENT=="Oscillators") PAR_n_bands =COLL_bands
 !
 if (ENVIRONMENT=="Non_Linear" ) PAR_n_freqs =n_runs
 !
end subroutine PARALLEL_global_dimensions_nl
