!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine PARALLEL_global_dimensions_sc(E,Xk,q,ENVIRONMENT)
 !
 use com,           ONLY:secnm
 use parallel_m,    ONLY:PAR_Q_bz_range,PAR_Q_ibz_range,PAR_QP_range,PAR_n_v_bands,PAR_n_c_bands,&
&                        PAR_EH_range,PAR_Dip_ib,PAR_Dip_ib_lim,PAR_X_ib,PAR_X_iq,PAR_n_G_vectors,&
&                        PAR_n_freqs,PAR_K_range,PAR_n_bands
 use drivers,       ONLY:l_elph_corr,l_gw0,l_HF_and_locXC,l_life,l_sc_run,l_nl_optics
 use BS,            ONLY:BS_bands,BS_nT_at_k,BS_n_eh_spaces
 use X_m,           ONLY:X_ng
 use SC,            ONLY:SC_bands
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp,nqibz,nqbz
 use QP_m,          ONLY:QP_n_states,QP_nb,QP_n_G_bands
#include<y_memory.h>
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 character(*)         :: ENVIRONMENT
 !
 ! Work Space
 !
 integer              :: i_k
 logical :: computing_Fock
 !
 computing_Fock= ( l_HF_and_locXC .and. index(secnm,"Non-Local Fock")/=0 )
 !
 ! K-pts
 !-------
 PAR_K_range=Xk%nibz
 !
 ! VAL bands
 !-----------
 if (l_sc_run .or. l_nl_optics) then
   ! DS 2023/07/24 : to check if this is really needed or not
   if (index(ENVIRONMENT, "Response_G_space")>0     ) PAR_n_v_bands= (/1,PAR_X_ib(2)/)
 endif
 !
 if (ENVIRONMENT=="Self_Energy"       ) then
   PAR_n_G_vectors= X_ng
   if (l_sc_run.and.any( (/P_collisions%N,COH_collisions%N,HXC_collisions%N/)> 0))  &
&                                               PAR_Q_bz_range=(/1,nqbz/)
 endif
 !
 !
 ! Generic Bands
 !---------------
 if (ENVIRONMENT=="Self_Energy"                     ) then
   if (l_HF_and_locXC)                               PAR_n_bands=(/1,maxval((/E%nbm(:),QP_nb/))/)
   if ((l_gw0.or.l_life).and..not.computing_Fock)    PAR_n_bands=(/1,max(QP_n_G_bands(2),QP_nb)/)
   if (.not.computing_Fock)                          PAR_n_bands(1)=QP_n_G_bands(1)
   if (l_sc_run)           PAR_n_bands=SC_bands
 endif
 !
 ! QP states
 !-----------
 if (ENVIRONMENT=="Self_Energy"                     ) PAR_QP_range=QP_n_states
 !
end subroutine PARALLEL_global_dimensions_sc
