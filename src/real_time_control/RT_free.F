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
subroutine RT_free( )
 !
 use R_lattice,      ONLY:bz_samp_FineGd_reset
 use DIPOLES,        ONLY:DIP_alloc
 use ELPH,           ONLY:ELPH_alloc('FREE'),Q_io_map,K_io_map,elph_gkkp_expanded_sq
 use hamiltonian,    ONLY:H_free
 use xc_functionals, ONLY:E_xc
 use real_time,      ONLY:DbGd_EE_scatt_table,RT_TIME_status,REF_V_xc_sc,&
&                         Ho_plus_Sigma,H_pseudo_eq,RT_Vnl_xc,HARTREE0,XC0,G_lesser_reference,&
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
 YAMBO_FREE(elph_gkkp_expanded_sq)
 YAMBO_FREE(I1_matrix)
 YAMBO_FREE(rho_reference)
 YAMBO_FREE(magn_reference)
 YAMBO_FREE(RT_Deph_Matrix)
 YAMBO_FREE(Q_io_map)
 YAMBO_FREE(K_io_map)
 call ELPH_alloc('FREE')( )
 !
 call bz_samp_FineGd_reset(RTibz)
 !
end subroutine RT_free
