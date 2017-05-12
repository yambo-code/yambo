!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
subroutine RT_Hamiltonian(dG_in,A_input,E,k,q,Time)
 !
 ! 1) The KBE is coded in terms of :
 !
 !    i d_t G(t) - [ H^RT_s , G(t) ]  =  S^xc(t)
 !
 !   - G(t) is the time diagonal G_lesser
 !   - H^RT_s contains the equilibrium hamiltonian H^EQ
 !     plus the variation of the static Hxc self-energy
 !     plus the external field(s)
 !   - S_xc contains instead the dynamical part of the self-energy
 !     which describes relaxation and dissipation
 !
 ! 2) This subroutine computes the RT Hamiltonian H^RT
 !
 !     H^RT= H^EQ + ( \Sigma^Hxc[G(t)] - \Sigma^Hxc[G_ref] ) + U^ext
 !
 !    The term A+B can be computed with two different strategies:
 !    a) computing the equilibrium kernel K (named here collisions) and then 
 !      \Sigma_Hxc = K*(G(t)-G_ref) 
 !      This approach is exact only for linear self-energies
 !    b) directly computing the \Sigma_Hxc[G(t)] functional
 !
 ! 3) Available approximations for H^RT
 !    IP          ==>  \Sigma^Hxc = 0
 !    TD-HARTREE  ==>  \Sigma^Hxc=V^H
 !    TD-LDA      ==>  \Sigma^Hxc=V^H+V^xc
 !    TD-SEX      ==>  \Sigma^Hxc=V^H+\Simga^{SEX} (only strategy 2a available)
 !
 use pars,           ONLY:SP,cONE,cZERO
 use wrapper_omp,    ONLY:V_by_V_plus_V_omp
 use electrons,      ONLY:levels,n_spinor,n_spin
 use FFT_m,          ONLY:fft_size
 use drivers,        ONLY:l_use_Hxc_collisions
 use R_lattice,      ONLY:bz_samp,nXkibz
 use hamiltonian,    ONLY:V_hartree_sc,H_nl_sc,rho_n,magn_n,V_xc_sc
 use real_time,      ONLY:Ho_plus_Sigma,l_NE_with_fields,l_RT_uses_E_DbGd,REF_V_xc_sc,&
&                         DbGd_ibz_coo,rho_reference,magn_reference,RT_Vnl_xc,&
&                         l_RT_RWA,eval_HARTREE,eval_DFT,RT_ibz_coo,&
&                         RT_bands,RT_nbands,RT_nk
 use fields,         ONLY:gauge_field
 use wave_func,      ONLY:WF
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_WF_k,HEAD_k_cpu,master_cpu,HEAD_QP_cpu
 use xc_functionals, ONLY:V_xc,XC_LDA_C_PZ,XC_potential_driver
 use global_XC,      ONLY:WF_kind,WF_xc_functional 
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k,q
 real(SP),      intent(in)    :: Time
 complex(SP),   intent(in)    :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 type(gauge_field), intent(in):: A_input
 !
 ! Work Space
 !
 integer          :: ik,ik_RT,ik_DbGd,ib,ibp
 real(SP)         :: delta_rho(fft_size),delta_magn(fft_size,3)
 !
#if defined _TIMING
 call timing('RT Hamiltonian',OPR='start')
#endif
 !
 ! Contribution from \delta\Sigma_coherent
 !=========================================
 !
 ! All the potentials are written in terms of the variations
 ! * The Hartree term: V_hartree(rho) = V_hartree(rho_reference) + V_hartree(delta_rho)
 ! * The V_xc    term: V_xc(rho)      = V_xc(rho_reference)      + {V_xc(rho) - V_xc(rho_reference)} 
 ! * Sigma_xc    term: S_xc(G)        ~ S_xc(G_reference)        + S_xc(delta_G)
 !   For Sigma_xc this is in general an approximation. It is exact only for self-energies 
 !                linear in G_lesser as exchange and SEX[W_eq] 
 !
 !  V_hartree(rho_reference) is in REF_V_hartree_sc
 !  V_xc(rho_reference)      is in REF_V_xc_sc
 !  S_xc(G_reference)        is introduced in the QP corrections to the eigenvalues
 !
 !
 ! In the following loop there are operations spread among bands that 
 ! can be done by all cpu's that have PAR_IND_WF_k%element_1D(ik)=T.
 ! Some operations, instead, must be carried on only by the head CPU in this group
 ! identified by PAR_COM_Xk_ibz_A2A%CPU_id==0
 !
 Ho_plus_Sigma=cZERO
 !
 ! The bare Hamiltonian
 !=========================
 if (.not.l_RT_RWA) then
   !
   do ik=1,nXkibz
     !
     if (.not.PAR_IND_WF_k%element_1D(ik) .or. .not.HEAD_k_cpu ) cycle
     !
     ! DAVIDE:
     ! Ho+Sigma[eq]= E^{QP} 
     !
     do ik_RT=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       ik_DbGd=ik_RT-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
       do ib=RT_bands(1),RT_bands(2)
         if(     l_RT_uses_E_DbGd) Ho_plus_Sigma(ib,ib,ik_RT,1)=E%E_DbGd(ib,ik_DbGd,1)
         if(.not.l_RT_uses_E_DbGd) Ho_plus_Sigma(ib,ib,ik_RT,1)=E%E(ib,ik,1)
       enddo
     enddo
     !
   enddo
   !
 endif
 !
 ! Local part computed in real space (V_xc_sc and V_Hartree_sc)
 !=============================================================
 !
 if (.not.l_use_Hxc_collisions) then
   !
   ! Density & Magnetization (needed by Hartree and local XCs)
   !--------------------------
   if (eval_HARTREE .or. eval_DFT) then
     call el_density_matrix(dG_in,E,k,delta_rho,RT_bands(1))
     rho_n=rho_reference+delta_rho
   endif
   !
   if(n_spin>1 .and. eval_DFT) then
     call el_magnetization_matrix(dG_in,E,k,delta_magn,RT_bands(1))
     magn_n=magn_reference+delta_magn
   endif
   !
   ! The Hartree term
   !-------------------
   if (eval_HARTREE) call V_Hartree(delta_rho,V_hartree_sc)
   !
   ! The DFT-like
   !-------------
   if (eval_DFT) then
     !
     call XC_potential_driver(E,k,WF_KIND,WF_xc_functional,1,&
&                              MUTE="-report -log -io_in -io_out",unMUTE=master_cpu)
     !
     V_xc_sc=V_xc-REF_V_xc_sc
     ! 
   endif
   !
   ! Add H_nl_sc to Ho_plus_Sigma 
   !---------------------------------
   do ik=1,nXkibz 
     !
     H_nl_sc=cZERO
     !
     if (n_spinor==1 .and. (eval_DFT.or.eval_HARTREE)) call V_real_space_to_H(ik,1,V_hartree_sc+V_xc_sc,H_nl_sc,WF,'def')
     !
     if (n_spinor==2) then
       if(eval_HARTREE) call V_real_space_to_H(ik,1,V_hartree_sc,H_nl_sc,WF,'def')
       if(eval_DFT)     call V_real_space_to_H(ik,1,V_xc_sc     ,H_nl_sc,WF,'xc ')
     endif
     !
     if (.not.PAR_IND_WF_k%element_1D(ik).or. .not.HEAD_QP_cpu) cycle
     !
     do ik_RT =RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       if (l_RT_RWA) then
         Ho_plus_Sigma(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),ik_RT,1)=&
&                                              H_nl_sc(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
       else
         call V_by_V_plus_V_omp(RT_nbands,cONE,H_nl_sc(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2)),&
&                               Ho_plus_Sigma(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),ik_RT,1))
       endif
     enddo
     !
   enddo
   !
 endif
 !
 ! Non local part computed with collisions (RT_Vnl_xc)
 !======================================================
 if (l_use_Hxc_collisions) then
   !
   call COLLISIONS_compose_rt(dG_in)
   !
   ! Add RT_Vnl_xc to Ho_plus_Sigma 
   !---------------------------------
   if(HEAD_QP_cpu.and.HEAD_k_cpu) then
     do ik=1,nXkibz 
       if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
       do ik_RT =RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
         if (l_RT_RWA) then
           Ho_plus_Sigma(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),ik_RT,1)=&
&                                                RT_Vnl_xc(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),ik_RT,1)
         else
           call V_by_V_plus_V_omp(RT_nbands,cONE,RT_Vnl_xc(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),ik_RT,1),&
&                               Ho_plus_Sigma(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),ik_RT,1))
         endif
       enddo
     enddo
   endif
   !
 endif
 !
 ! The External Field
 !====================
 if (l_NE_with_fields.and.HEAD_k_cpu) then
   do ik=1,nXkibz 
     !
     if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
     !
     H_nl_sc=cZERO
     !
     call RT_apply_field(ik,1,H_nl_sc,A_input)
     !
     forall( ib=RT_bands(1):RT_bands(2) , ibp=RT_bands(1):RT_bands(2) , ik_RT=RT_ibz_coo(ik,1):RT_ibz_coo(ik,2) ) &
&      Ho_plus_Sigma(ib,ibp,ik_RT,1)= Ho_plus_Sigma(ib,ibp,ik_RT,1)+H_nl_sc(ib,ibp)
     !  
   enddo
 endif
 !
 ! All2All
 !=========
 call PP_redux_wait(Ho_plus_Sigma)
 !
#if defined _TIMING
 call timing('RT Hamiltonian',OPR='stop')
#endif
 !
end subroutine RT_Hamiltonian
