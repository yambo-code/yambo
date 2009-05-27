!
! Copyright (C) 2000-2008 D. Varsano, M. Palummo and the YAMBO team
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
subroutine SC_common_build_Ho(E,Xk,k)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels,n_sp_pol,el_density,spin
 use QP_m,           ONLY:QP_nk,QP_nb,QP_Vxc,QP_n_states
 use SC,             ONLY:Ho,E_bare,V_xc_sc,V_hartree_sc,rho_n
 use memory_m,       ONLY:mem_est
 use real_time,      ONLY:Hartree_plus_Vxc_sc,l_RT_no_updates,G_lesser,V_xc_0,V_hartree_0
 use wave_func,      ONLY:wf
 use R_lattice,      ONLY:bz_samp
 use xc_functionals, ONLY:V_xc
 implicit none
 !
 type(levels)             :: E
 type(bz_samp)            :: Xk,k
 !
 ! Work space
 !
 integer   :: ik,ib,i_spin
 !
 allocate(QP_Vxc(QP_n_states))
 call mem_est("QP_Vxc",(/QP_n_states/))    
 !
 call DFT_Vxc(E,k)
 !
#if defined _RT
 call el_density(E,Xk,rho_n,G_lesser=G_lesser)
#else
 call el_density(E,Xk,rho_n)
#endif
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
 !
 Ho=(0.,0.)
 call V_Hartree(rho_n,V_hartree_sc)
 V_hartree_sc(:,n_sp_pol)=V_hartree_sc(:,1)
 !
 do ik=1,QP_nk
   call V_real_space_to_H(ik,-V_hartree_sc,Ho(:,:,ik,:),wf)
 enddo
 !
 forall (ib=1:QP_nb,ik=1:QP_nk,i_spin=1:n_sp_pol) Ho(ib,ib,ik,i_spin) = Ho(ib,ib,ik,i_spin) + E_bare(ib,ik,i_spin)  
 !
 ! ... then remove Vxc[rho_0] also
 !=================================
 !
 do ik=1,QP_nk
   call V_qp_basis_to_H(ik,-QP_Vxc,Ho(:,:,ik,:))
 enddo
 !
 deallocate(QP_Vxc)
 call mem_est("QP_Vxc")
 !
#if defined _RT
 !
 ! Save Hartree and V_xc at T=0
 !
 V_xc_0                  =  V_xc 
 V_hartree_0(:,n_sp_pol) =  V_hartree_sc(:,1)
 !
 ! Define Hartree+xc potential Hamiltonian
 !
 if(l_RT_no_updates) then 
   !
   Hartree_plus_Vxc_sc=(0._SP,0._SP)
   !
   do ik=1,QP_nk
     call V_real_space_to_H(ik,V_hartree_sc+V_xc_sc,Hartree_plus_Vxc_sc(:,:,ik,:),wf)
   enddo
   !
 endif
 !
#endif
 !
 deallocate(V_xc)
 !
end subroutine SC_common_build_Ho
