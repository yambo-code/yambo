!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_free( )
 !
 use R_lattice,      ONLY:bz_samp_FineGd_reset
 use DIPOLES,        ONLY:DIP_alloc
#if defined _ELPH
 use interfaces,     ONLY:ELPH_alloc
 use ELPH,           ONLY:Q_io_map,GKKP_ep_sq,GKKP_pe_sq
#endif
 use hamiltonian,    ONLY:H_free
 use xc_functionals, ONLY:E_xc
 use real_time,      ONLY:DbGd_EE_scatt_table,RT_TIME_status,REF_V_xc_sc,&
&                         Ho_plus_Sigma,H_EQ,H_pseudo_eq,RT_Vnl_xc,HARTREE0,XC0,G_lesser_reference,&
&                         dG_lesser,G_lesser,MEM_pointer,REF_V_xc_sc,REF_V_hartree_sc,rho_reference,&
&                         magn_reference,DbGd_EE_scatt_table,RT_TIME_status,REF_V_xc_sc,&
&                         RT_levels,I1_matrix,RT_Deph_Matrix,RTibz
 use RT_occupations, ONLY:RT_ALL_occupations_free
#if defined _ELPH_ITERATIVE
 use RT_iterative,   ONLY:THETA_matrix,THETA_matrix_index
#endif
 use RT_lifetimes,   ONLY:RT_ALL_lifetimes_free,q_weight_RT
 !
#include<memory.h>
 !
 call H_free()
 !
 call DIP_alloc('DIP_iR')
 call DIP_alloc('DIP_P')
 call DIP_alloc('DIP_v')
 call DIP_alloc('DIP_spin')
 call DIP_alloc('DIP_orbital')
 call DIP_alloc('P_square')
 call DIP_alloc('DIP_P_spinor')
 !
 call RT_ALL_lifetimes_free()
 call RT_ALL_occupations_free()
 !
 YAMBO_FREE(DbGd_EE_scatt_table)
 YAMBO_FREE(E_xc)
 YAMBO_FREE(RT_TIME_status)
 YAMBO_FREE(REF_V_xc_sc)
 !
 YAMBO_FREE(REF_V_hartree_sc)
 YAMBO_FREE(RT_levels%E)
 YAMBO_FREE(RT_levels%W)
 !
 YAMBO_FREE(Ho_plus_Sigma)
 YAMBO_FREE(H_EQ)
 YAMBO_FREE(H_pseudo_eq)
 YAMBO_FREE(RT_Vnl_xc)
 !
 YAMBO_FREE(HARTREE0)
 YAMBO_FREE(XC0)
 !
 YAMBO_FREE(G_lesser)
 YAMBO_FREE(G_lesser_reference)
 YAMBO_FREE(dG_lesser)
 YAMBO_FREE(MEM_pointer)
 !
 YAMBO_FREE(q_weight_RT)
#if defined _ELPH_ITERATIVE
 YAMBO_FREE(THETA_matrix)
 YAMBO_FREE(THETA_matrix_index)
#endif
 YAMBO_FREE(I1_matrix)
 YAMBO_FREE(rho_reference)
 YAMBO_FREE(magn_reference)
 YAMBO_FREE(RT_Deph_Matrix)
 !
#if defined _ELPH
 !
 YAMBO_FREE(Q_io_map)
 !
 call ELPH_alloc('FREE',GKKP=GKKP_ep_sq)
 call ELPH_alloc('FREE',GKKP=GKKP_pe_sq)
#endif
 !
 call bz_samp_FineGd_reset(RTibz)
 !
end subroutine RT_free
