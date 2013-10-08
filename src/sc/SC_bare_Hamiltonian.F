!
! Copyright (C) 2000-2013 D. Varsano, M. Palummo and the YAMBO team
!              http://www.yambo-code.org
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
 use SC,             ONLY:Ho,E_bare,V_xc_sc,V_hartree_sc,rho_n,G_lesser,RT_ibz_coo
 use memory_m,       ONLY:mem_est
 use parallel_m,     ONLY:PAR_IND_WF_k,PP_redux_wait,PAR_COM_WF_k_INDEX
 use wave_func,      ONLY:wf
 use R_lattice,      ONLY:bz_samp
 use xc_functionals, ONLY:V_xc
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
 call el_density_matrix(G_lesser(:,:,RT_ibz_coo(:,1),1),E,Xk,rho_n,1,.false.)
 !
#else
 !
 call el_density(E,Xk,rho_n,.false.)
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
 V_hartree_sc(:,n_sp_pol)=V_hartree_sc(:,1)
 ! 
 do ik=1,QP_nk
   !
#if defined _RT
   if (.not.PAR_IND_WF_k%element_1D(ik)) then
     Ho(:,:,ik,:)  =cZERO
     cycle
   endif
#endif
   !
   call V_real_space_to_H(ik,-V_hartree_sc,Ho(:,:,ik,:),wf)
   !
   forall (ib=1:QP_nb,i_spin=1:n_sp_pol) &
&           Ho(ib,ib,ik,i_spin) = Ho(ib,ib,ik,i_spin) + E_bare(ib,ik,i_spin)  
   !
   ! ... then remove Vxc[rho_0] also
   !=================================
   !
   call V_qp_basis_to_H(ik,-QP_Vxc,Ho(:,:,ik,:))
   !
 enddo
 !
 deallocate(QP_Vxc)
 call mem_est("QP_Vxc")
 !
#if defined _RT
 call PP_redux_wait(Ho,COMM=PAR_COM_WF_k_INDEX%COMM)
#endif
 !
end subroutine SC_bare_Hamiltonian
