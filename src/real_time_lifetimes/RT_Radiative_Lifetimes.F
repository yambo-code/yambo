!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_Radiative_Lifetimes(i_k,E,k,q)
 !
 use units,               ONLY:HA2EV,SPEED_OF_LIGHT
 use pars,                ONLY:SP
 use functions,           ONLY:theta_function
 use electrons,           ONLY:levels,spin,spin_occ
 use R_lattice,           ONLY:bz_samp
 use parallel_m,          ONLY:PAR_DIPk_ibz_index,PAR_IND_B_mat
 use DIPOLES,             ONLY:DIP_P
 use RT_occupations,      ONLY:RT_life_occ,RT_ho_occ,RT_el_occ
 use RT_lifetimes,        ONLY:RT_EH_life,l_initial_equilibrium
 use real_time,           ONLY:RT_bands,RTibz,RAD_magnification
 use hamiltonian,         ONLY:B_mat_index
 !
 implicit none
 !
 integer             :: i_k
 type(levels)        :: E
 type(bz_samp)       :: k,q
 !
 ! Work Space
 !
 integer     :: i_n,i_spin,i_m,i_k_mem,i_k_rim
 real(SP)    :: E_diff,f_m,P_square,pre_factor,Hole_f_m
 !
 i_k_mem=PAR_DIPk_ibz_index(i_k)
 i_k_rim=RTibz%k_range(i_k,1)
 i_spin=1
 !
 B_loop: do i_n=RT_bands(1),RT_bands(2)
   !
   Bp_loop: do i_m=RT_bands(1),RT_bands(2)
     !
     if (.not.PAR_IND_B_mat%element_1D( B_mat_index(i_n,i_m,RT_bands) ) ) cycle
     !
     E_diff =E%E(i_n,i_k,i_spin)-E%E(i_m,i_k,i_spin)
     !
     if(l_initial_equilibrium) then
       f_m      = RT_el_occ%N_ref(i_m,i_k_rim)/spin_occ
       Hole_f_m = RT_ho_occ%N_ref(i_m,i_k_rim)/spin_occ
     else
       f_m      =  RT_life_occ%dN(i_m,i_k_rim)/spin_occ
       Hole_f_m = -RT_life_occ%dN(i_m,i_k_rim)/spin_occ
     endif
     !
     P_square=2.*(abs(DIP_P(1,i_m,i_n,i_k_mem,i_spin))**2.+&
&                 abs(DIP_P(2,i_m,i_n,i_k_mem,i_spin))**2.+&
&                 abs(DIP_P(3,i_m,i_n,i_k_mem,i_spin))**2)
     !
     pre_factor=P_square/3._SP/SPEED_OF_LIGHT**2*abs(E_diff)
     !
     RT_EH_life%emit(i_n,i_k)=RT_EH_life%emit(i_n,i_k)+&
&                                   pre_factor*Hole_f_m*theta_function( E_diff,0.01/HA2EV,0)*RAD_magnification
     RT_EH_life%abs(i_n,i_k)=RT_EH_life%abs(i_n,i_k)+&
&                                   pre_factor*     f_m*theta_function(-E_diff,0.01/HA2EV,0)*RAD_magnification
     !
   enddo Bp_loop
   !
 enddo B_loop
 !
end subroutine RT_Radiative_Lifetimes

