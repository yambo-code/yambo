!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine Bare_Hamiltonian(E,Xk,k)
 !
 use pars,           ONLY:cZERO
 use electrons,      ONLY:levels,n_sp_pol,spin,n_spin
 use QP_m,           ONLY:QP_nk,QP_nb,QP_Vxc,QP_n_states
 use hamiltonian,    ONLY:Ho,E_bare,V_hartree_sc,rho_n,magn_n,V_xc_sc,&
&                         H_ref_bands,WF_G_max,WF_Go_indx
 use parallel_m,     ONLY:PAR_IND_WF_k,PP_redux_wait,PAR_IND_WF_linear
 use wave_func,      ONLY:WF
 use R_lattice,      ONLY:bz_samp,nkibz
 use xc_functionals, ONLY:V_xc
 use interfaces,     ONLY:el_density_and_current,WF_load
 use wave_func,      ONLY:WF
#if defined _RT
 use drivers,        ONLY:l_real_time
 use real_time,      ONLY:REF_V_xc_sc,REF_V_hartree_sc
#endif
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
#include<memory.h>
 !
 type(levels)             :: E
 type(bz_samp)            :: Xk,k
 !
 ! Work space
 !
 integer     :: ik,ib,i_sp_pol
 !
 Ho=cZERO
 !
#if defined _TIMING
 call timing('Bare_Hamiltonian',OPR='start')
#endif
 !
 !
 ! WF Loading, density and magnetization
 !=======================================
 call WF_load(WF,WF_G_max,WF_Go_indx,(/1,max(H_ref_bands(2),E%nbm)/),(/1,nkibz/),title='-SC')
 !
 call el_density_and_current(E,Xk,rho=rho_n)
 !
 if(n_spin>1) call el_magnetization(E,Xk,magn_n)
 !
 V_xc_sc=V_xc
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
 ! V_hartree
 call V_Hartree(rho_n,V_hartree_sc)
 !
 ! QP_Vxc
 YAMBO_ALLOC(QP_Vxc,(QP_n_states))
 QP_Vxc=cZERO
 !
 call XCo_local(E,k)
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,QP_nk
     !
     ! Add bare energies ... 
     !=======================
     !
     do ib=1,QP_nb
       if (.not.PAR_IND_WF_linear%element_2D(ib,ik)) cycle
       Ho(ib,ib,ik,i_sp_pol) = Ho(ib,ib,ik,i_sp_pol) + E_bare(ib,ik,i_sp_pol)  
     enddo
     !
#if defined _RT
     if(l_real_time) then
       if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
     endif
#endif
     !
     ! ... then remove Vh[rho_0] 
     !===========================
     !
     call V_real_space_to_H(ik,i_sp_pol,-V_hartree_sc,Ho(:,:,ik,i_sp_pol),WF,'def')
     !
     ! ... then remove Vxc[rho_0] also
     !=================================
     !
     call V_qp_basis_to_H(ik,i_sp_pol,-QP_Vxc,Ho(:,:,ik,i_sp_pol))
     !
   enddo
   !
 enddo
 !
 !
 YAMBO_FREE(QP_Vxc)
 !
 call PP_redux_wait(Ho)
 !
#if defined _TIMING
 call timing('Bare_Hamiltonian',OPR='stop')
#endif
 !
end subroutine Bare_Hamiltonian
