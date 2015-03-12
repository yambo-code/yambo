!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DV
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
subroutine SC_bare_Hamiltonian(E,Xk,k)
 !
 use pars,           ONLY:SP,cZERO
 use electrons,      ONLY:levels,n_sp_pol,spin
 use QP_m,           ONLY:QP_nk,QP_nb,QP_Vxc,QP_n_states
 use SC,             ONLY:Ho,E_bare,V_hartree_sc,rho_n,V_xc_sc
 use memory_m,       ONLY:mem_est
 use parallel_m,     ONLY:PAR_IND_WF_k,PP_redux_wait,PAR_IND_WF_linear
 use wave_func,      ONLY:WF
 use R_lattice,      ONLY:bz_samp
 use xc_functionals, ONLY:V_xc
#if defined _RT
 use real_time,      ONLY:REF_V_xc_sc,REF_V_hartree_sc,G_lesser,RT_ibz_coo
#endif
#if defined _MAGNETIC
 use drivers,        ONLY:l_sc_magnetic
#endif
 implicit none
 !
 type(levels)             :: E
 type(bz_samp)            :: Xk,k
 !
 ! Work space
 !
 integer     :: ik,ib,i_spin
 !
 allocate(QP_Vxc(QP_n_states))
 call mem_est("QP_Vxc",(/QP_n_states/))    
 !
 QP_Vxc=cZERO
 !
#if defined _RT
 !
 ! Note that G_lesser here is the non-interacting one (occupations on the diagonal) only
 !
 call el_density_matrix(G_lesser,E,Xk,rho_n,1)
 !call el_magnetization_matrix(G_lesser,E,Xk,magn_n,1)
 !
#else
 !
 call el_density(E,Xk,rho_n,.false.)
 !call el_magnetization(E,Xk,magn_n)
 V_xc_sc=V_xc 
 !
#endif
 !
 call XCo_local(E,k)
 !
 ! The Ho = -nabla**2. + V_ext is deduced from the input energies en0:
 !
 ! H = Ho + v_hxc[rho_in] and
 ! H psi_0(n) = en0(n) psi_0(n)
 !
 ! then Ho_n,m = en0(n) delta_n,m - v_hxc[rho_0]_n,m
 !
 ! where the matrix elements are calculated from the  psi_0(n)
 !
 ! ... so first store -Vhartree in Ho ...
 !========================================
 !
#if defined _MAGNETIC
 !
 ! If we are using yambo_magnetic now Ho already
 ! contains H_magnetic
 !
 if (.not.l_sc_magnetic) Ho=(0._SP,0._SP)
 !
#else
 !
 Ho=(0._SP,0._SP)
 !
#endif
 !
 call V_Hartree(rho_n,V_hartree_sc)
 !
 do ik=1,QP_nk
   !
   ! Add bare energies ... 
   !=======================
   !
   do ib=1,QP_nb
     if (.not.PAR_IND_WF_linear%element_2D(ib,ik)) cycle
     do i_spin=1,n_sp_pol
       Ho(ib,ib,ik,i_spin) = Ho(ib,ib,ik,i_spin) + E_bare(ib,ik,i_spin)  
     enddo
   enddo
   !
#if defined _RT
   if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
#endif
   !
   ! ... then remove Vh[rho_0] 
   !===========================
   !
   call V_real_space_to_H(ik,-V_hartree_sc,Ho(:,:,ik,:),WF,'def')
   !
   ! ... then remove Vxc[rho_0] also
   !=================================
   !
   call V_qp_basis_to_H(ik,-QP_Vxc,Ho(:,:,ik,:))
   !
 enddo
 !
#if defined _RT
 !
 ! Save the reference xc and Hartree potentials 
 !==============================================
 !
 REF_V_xc_sc=V_xc
 REF_V_hartree_sc=V_hartree_sc
 !
#endif
 !
 deallocate(QP_Vxc)
 call mem_est("QP_Vxc")
 !
 call PP_redux_wait(Ho)
 !
end subroutine SC_bare_Hamiltonian
