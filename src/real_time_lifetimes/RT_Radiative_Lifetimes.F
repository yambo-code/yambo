!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_Radiative_Lifetimes(i_k,E,k,q)
 !
 use units,               ONLY:HA2EV,SPEED_OF_LIGHT
 use pars,                ONLY:SP
 use functions,           ONLY:theta_function
 use electrons,           ONLY:levels,spin,spin_occ
 use R_lattice,           ONLY:bz_samp
 use parallel_m,          ONLY:PAR_DIPk_ibz_index,PAR_IND_B_mat
 use X_m,                 ONLY:DIP_P
 use real_time,           ONLY:RT_bands,E_PHOT_Hole_lifetime,E_PHOT_Electron_lifetime,l_initial_equilibrium,&
&                              RT_delta_occupations,RT_ibz_coo,RT_H_occupations_reference,&
&                              RT_E_occupations_reference,RAD_magnification
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
 i_k_rim=RT_ibz_coo(i_k,1)
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
       f_m      = RT_E_occupations_reference(i_m,i_k_rim)/spin_occ
       Hole_f_m = RT_H_occupations_reference(i_m,i_k_rim)/spin_occ
     else
       f_m      =  RT_delta_occupations(i_m,i_k_rim)/spin_occ
       Hole_f_m = -RT_delta_occupations(i_m,i_k_rim)/spin_occ
     endif
     !
     P_square=2.*(abs(DIP_P(1,i_m,i_n,i_k_mem,i_spin))**2.+&
&                 abs(DIP_P(2,i_m,i_n,i_k_mem,i_spin))**2.+&
&                 abs(DIP_P(3,i_m,i_n,i_k_mem,i_spin))**2)
     !
     pre_factor=P_square/3._SP/SPEED_OF_LIGHT**2*abs(E_diff)
     !
     E_PHOT_Electron_lifetime(i_n,i_k)=E_PHOT_Electron_lifetime(i_n,i_k)+&
&                                   pre_factor*Hole_f_m*theta_function( E_diff,0.01/HA2EV,0)*RAD_magnification
     E_PHOT_Hole_lifetime(i_n,i_k)=E_PHOT_Hole_lifetime(i_n,i_k)+&
&                                   pre_factor*     f_m*theta_function(-E_diff,0.01/HA2EV,0)*RAD_magnification
     !
   enddo Bp_loop
   !
 enddo B_loop
 !
end subroutine RT_Radiative_Lifetimes

