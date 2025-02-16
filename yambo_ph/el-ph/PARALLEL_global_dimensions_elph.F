!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine PARALLEL_global_dimensions_elph(E,Xk,q,ENVIRONMENT)
 !
#if defined _PHEL
 use PHEL,          ONLY:PH_Q
#endif
 use com,           ONLY:secnm
 use ELPH,          ONLY:elph_nQ_used,QP_PH_n_G_bands,elph_use_q_grid
 use parallel_m,    ONLY:PAR_Q_bz_range,PAR_n_bands
 use drivers,       ONLY:l_elph_corr,l_gw0,l_HF_and_locXC,l_life
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp,nqibz,nqbz
 !
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
#if defined _PHEL
 if (ENVIRONMENT=="Phonon_Self_Energy")        PAR_Q_ibz_range=PH_Q
#endif
 !
 if (ENVIRONMENT=="Self_Energy"       ) then
   if (l_elph_corr.and.elph_use_q_grid)         PAR_Q_bz_range=(/1,nqbz/)
   if (l_elph_corr.and..not.elph_use_q_grid)    PAR_Q_bz_range=(/1,elph_nQ_used/)
 endif
 !
 ! EH Bands 
 !----------
 if (ENVIRONMENT=="Phonon_Self_Energy"              ) then
    PAR_n_bands=(/1,(QP_PH_n_G_bands(2)-QP_PH_n_G_bands(1)+1)**2/)
 endif
 !
 ! Generic Bands
 !---------------
 if (ENVIRONMENT=="Self_Energy"                     ) then
   if (l_elph_corr)        PAR_n_bands=(/QP_PH_n_G_bands(1),QP_PH_n_G_bands(2)/)
 endif
 !
end subroutine PARALLEL_global_dimensions_elph
