!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine PARALLEL_global_dimensions(E,Xk,q,ENVIRONMENT)
 !
 use com,           ONLY:secnm
 use parallel_m,    ONLY:PAR_Q_bz_range,PAR_Q_ibz_range,PAR_QP_range,PAR_n_v_bands,PAR_n_c_bands,&
&                        PAR_EH_range,PAR_Dip_ib,PAR_Dip_ib_lim,PAR_X_ib,PAR_X_iq,PAR_n_G_vectors,&
&                        PAR_n_freqs,PAR_K_range,PAR_n_bands
 use drivers,       ONLY:l_eval_collisions,l_elph_corr,l_gw0,l_HF_and_locXC,l_life,l_sc_run,l_nl_optics
 use BS,            ONLY:BS_bands,BS_nT_at_k,BS_n_eh_spaces
 use X_m,           ONLY:X_ng
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
 if (ENVIRONMENT=="BZ_Indexes"               ) PAR_K_range=Xk%nibz
 if (ENVIRONMENT=="DIPOLES"                  ) PAR_K_range=Xk%nibz
 if (index(ENVIRONMENT, "Response_G_space")>0) PAR_K_range=Xk%nbz
 if (ENVIRONMENT=="Response_T_space"         ) PAR_K_range=Xk%nibz
 !
 ! COND bands
 !------------
 if (ENVIRONMENT=="DIPOLES"                  ) PAR_n_c_bands= (/PAR_Dip_ib_lim(2),PAR_Dip_ib(2)/)
 if (index(ENVIRONMENT, "Response_G_space")>0) PAR_n_c_bands= (/minval(E%nbf)+1,PAR_X_ib(2)/)
 if (ENVIRONMENT=="Response_G_space"         ) PAR_n_c_bands= (/minval(E%nbf)+1,PAR_X_ib(2)/)
 if (ENVIRONMENT=="Response_T_space"         ) PAR_n_c_bands= (/minval(E%nbf)+1,BS_bands(2)/)
 !
 ! VAL bands
 !-----------
 if (ENVIRONMENT=="DIPOLES")                          PAR_n_v_bands= (/PAR_Dip_ib(1),PAR_Dip_ib_lim(1)/)
 if (index(ENVIRONMENT, "Response_G_space")>0       ) PAR_n_v_bands= (/PAR_X_ib(1),maxval(E%nbm)/)
 if (ENVIRONMENT=="Response_T_space"                ) PAR_n_v_bands= (/BS_bands(1),maxval(E%nbm)/)
 !
 ! # G vectors
 !-------------
 if (index(ENVIRONMENT, "Response_G_space")>0) PAR_n_G_vectors= X_ng
 !
 ! Q-pts
 !-------
 PAR_Q_ibz_range=(/1,nqibz/)
 PAR_Q_bz_range=(/1,nqbz/)
 if (ENVIRONMENT=="Response_G_space"  )        PAR_Q_ibz_range=(/PAR_X_iq(1),PAR_X_iq(2)/)
 !
 if (ENVIRONMENT=="Self_Energy"       ) then
   PAR_n_G_vectors= X_ng
 endif
 !
 !
 ! EH-range
 !----------
 if (ENVIRONMENT=="Response_T_space") then
   ! Set this value just to properly define a parallel range
   PAR_EH_range=(PAR_n_c_bands(2)-PAR_n_c_bands(1)+1)*PAR_n_v_bands(2)-PAR_n_v_bands(1)+1
 endif
 if (ENVIRONMENT=="Response_T_space_transitions") then
   PAR_EH_range=maxval( BS_nT_at_k )
   do i_k=1,Xk%nibz*BS_n_eh_spaces
     if (BS_nT_at_k(i_k)<PAR_EH_range.and.BS_nT_at_k(i_k)>0) PAR_EH_range=BS_nT_at_k(i_k)
   enddo
 endif
 !
 ! Generic Bands
 !---------------
 if (ENVIRONMENT=="Self_Energy"                     ) then
   if (l_HF_and_locXC)                               PAR_n_bands=(/1,maxval((/E%nbm(:),QP_nb/))/)
   if ((l_gw0.or.l_life).and..not.computing_Fock)    PAR_n_bands=(/1,max(QP_n_G_bands(2),QP_nb)/)
   if (.not.computing_Fock)                          PAR_n_bands(1)=QP_n_G_bands(1)
 endif
 !
 ! QP states
 !-----------
 if (ENVIRONMENT=="Self_Energy"                     ) PAR_QP_range=QP_n_states
 !
end subroutine PARALLEL_global_dimensions
