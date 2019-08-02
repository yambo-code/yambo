!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use hamiltonian,    ONLY:Hzero,E_reference,V_hartree_sc,rho_n,magn_n,V_xc_sc,&
&                         H_ref_bands,WF_G_max,WF_Go_indx
 use parallel_m,     ONLY:PAR_IND_WF_k,PP_redux_wait,PAR_IND_WF_linear
 use wave_func,      ONLY:WF
 use R_lattice,      ONLY:bz_samp,nkibz
 use xc_functionals, ONLY:V_xc
 use interfaces,     ONLY:el_density_and_current,WF_load
 use wave_func,      ONLY:WF
 use timing_m,       ONLY:timing
#if defined _RT
 use drivers,        ONLY:l_real_time
 use real_time,      ONLY:REF_V_xc_sc,REF_V_hartree_sc,rho_reference,magn_reference
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
 Hzero=cZERO
 !
 call timing('Bare_Hamiltonian',OPR='start')
 !
 !
 ! WF Loading, density and magnetization
 !=======================================
 call WF_load(WF,WF_G_max,WF_Go_indx,(/H_ref_bands(1),max(H_ref_bands(2),E%nbm)/),(/1,nkibz/),title='-SC')
 !
#if defined _RT
 if(l_real_time) then
   rho_n=rho_reference
   if(n_spin>1) magn_n=magn_reference
 else
#endif
 call el_density_and_current(E,Xk,rho=rho_n)
 !
 if(n_spin>1) call el_magnetization(E,Xk,magn_n)
#if defined _RT
 endif
#endif
 !
 V_xc_sc=V_xc
 !
 ! The Hzero = -nabla**2. + V_ext is deduced from the input energies en0:
 !
 ! H = Hzero + v_hxc[rho_in] and
 ! H psi_0(n) = en0(n) psi_0(n)
 !
 ! then Hzero_n,m = en0(n) delta_n,m - v_hxc[rho_0]_n,m
 !
 ! where the matrix elements are calculated from the  psi_0(n)
 !
 ! ... so first store -Vhartree in Hzero ...
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
       if( ib<H_ref_bands(1).or.ib>H_ref_bands(2) ) cycle
       Hzero(ib,ib,ik,i_sp_pol) = Hzero(ib,ib,ik,i_sp_pol) + E_reference%E(ib,ik,i_sp_pol)  
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
     call V_real_space_to_H(ik,i_sp_pol,-V_hartree_sc,Hzero(:,:,ik,i_sp_pol),WF,'def')
     !
     ! ... then remove Vxc[rho_0] also
     !=================================
     !
     call V_qp_basis_to_H(ik,i_sp_pol,-QP_Vxc,Hzero(:,:,ik,i_sp_pol))
     !
   enddo
   !
 enddo
 !
#if defined _RT
 !
 ! Save the reference xc and Hartree potentials 
 !==============================================
 !
 if (l_real_time) then
   REF_V_xc_sc=V_xc
   REF_V_hartree_sc=V_hartree_sc
 endif
 !
#endif
 !
 YAMBO_FREE(QP_Vxc)
 !
 call PP_redux_wait(Hzero)
 !
 call timing('Bare_Hamiltonian',OPR='stop')
 !
end subroutine Bare_Hamiltonian
