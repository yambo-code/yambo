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
#if defined _RT
 use pars,           ONLY:SP
 use electrons,      ONLY:levels,n_spin,el_density
 use QP_m,           ONLY:QP_n_states,QP_nk,QP_nb,QP_state,QP_Vxc,QP_table
 use SC,             ONLY:Ho,SC_bands,H_nl_sc
 use memory_m,       ONLY:mem_est
 use real_time,      ONLY:V_hartree_0,V_xc_0,Hartree_plus_Vxc_0,l_RT_no_updates,rho,G_lesser
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
 integer   :: ik,ib,ibp,i1
 !
 ! Build-up QP table
 !
 call mem_est("QP_state",(/QP_n_states*QP_nk/))
 allocate(QP_state(QP_nb,QP_nk))
 !
 QP_state=.TRUE.
 !
 call QP_state_table_setup(E)
 !
 allocate(QP_Vxc(QP_n_states))
 call mem_est("QP_Vxc",(/QP_n_states/))
 !
 allocate(Ho(SC_bands,SC_bands,E%nk,n_spin),H_nl_sc(SC_bands,SC_bands,E%nk))
 call mem_est("Ho",(/size(Ho)/),(/2*2*SP/))
 !
 call el_density(E,Xk,rho,G_lesser=G_lesser)
 !
 call DFT_Vxc(E,k)
 !
 V_xc_0=V_xc 
 !
 ! Fill the initial Hamiltonian Ho
 ! Ho is the non-interacting kinetic energy 
 ! as defined in SC_driver.F
 !
 Ho=(0.,0.)
 !
 call V_Hartree(rho,V_hartree_0)
 !
 do ik=1,QP_nk
   call V_to_H(ik,-V_hartree_0,Ho(:,:,ik,1),wf)
 enddo
 !
 do i1 = 1, QP_n_states
   !      
   ib =QP_table(i1,1)
   ibp=QP_table(i1,2)
   ik =QP_table(i1,3)
   !
   if (ib==ibp) Ho(ib,ibp,ik,1) = Ho(ib,ibp,ik,1) + E%E(ib,ik,1)
   !
   Ho(ib,ibp,ik,1) = Ho(ib,ibp,ik,1) - QP_Vxc(i1)
   !
   if (ib==ibp) Ho(ib,ibp,ik,1) = real(Ho(ib,ibp,ik,1))
   !
   Ho(ibp,ib,ik,1) = conjg(Ho(ib,ibp,ik,1))
   !
 enddo
 !
 ! Define Hartree+xc potential Hamiltonian
 !
 if(l_RT_no_updates) then 
   !
   Hartree_plus_Vxc_0=(0._SP,0._SP)
   !
   do ik=1,QP_nk
     call V_to_H(ik,V_hartree_0+V_xc_0,Hartree_plus_Vxc_0(:,:,ik),wf)
   enddo
  !
 endif
#endif
 !
end subroutine SC_common_build_Ho
