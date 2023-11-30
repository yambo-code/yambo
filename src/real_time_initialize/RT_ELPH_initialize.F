!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_ELPH_initialize(E,k,q)
 ! 
 use parser_m,       ONLY:parser
 use real_time,      ONLY:RT_bands
 use electrons,      ONLY:levels
 use drivers,        ONLY:l_elph_scatt,l_phel_scatt
 use R_lattice,      ONLY:bz_samp
 use ELPH,           ONLY:PH_freqs_sq,PH_W_debye,GKKP_ep_sq,GKKP_pe_sq,l_GKKP_expanded_DB_exists,elph_nQ
 !
#include<memory.h>
 !
 type(levels)             :: E
 type(bz_samp),intent(in) :: k,q
 !
 ! Work Space
 !
 logical             ::l_use_Debye_E
 !
 call section('=','El-Ph matrix elements')
 !                 ---------------------
 ! Logicals
 !==========
 if (.not.l_elph_scatt.and..not.l_phel_scatt) return
 !
 call parser('UseDebyeE'   , l_use_Debye_E)
 !
 ! El-Ph databases check
 !=======================
 call ELPH_databases_check(RT_bands)
 if (.not.l_GKKP_expanded_DB_exists) then
   call warning('ELPH (expanded) database not found and/or incompatible OR QP widths not provided. E-P scattering switched off')
   l_elph_scatt   =.FALSE.
   return
 endif
 !
 ! Q/K grid remapping
 !====================
 call ELPH_databases_grids_map(k,q)
 !
 ! El-Ph databases I/O
 !=====================
 if (l_elph_scatt) call ELPH_databases_load(GKKP_ep_sq,E,k,q,RT_bands,(/1,elph_nQ/),"Q-BZ K-IBZ K-sym","dressed-dressed")
 !
 ! Pj-El databases I/O
 !=====================
 if (l_phel_scatt) call ELPH_databases_load(GKKP_pe_sq,E,k,q,RT_bands,(/1,elph_nQ/),"Q-IBZ K-BZ Q-sym","dressed-dressed")
 !
 ! Test of the effect of using a single optical mode
 !                               ===================
 !
 if (l_use_Debye_E) PH_freqs_sq=PH_W_debye**2
 !
 ! Q weights 
 !===========
 !
 call section('=','El-Ph BZ integrals')
 !                 -------------------
 call ELPH_acoustic_phonon_properties(q)
 !
 call RT_ELPH_Q_weights(q)
 !
 ! THETA matrix indexes
 !======================
#if defined _ELPH_ITERATIVE
 call RT_ELPH_iterative_init( )
#endif
 !
end subroutine RT_ELPH_initialize
