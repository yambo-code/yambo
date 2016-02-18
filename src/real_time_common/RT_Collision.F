!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine RT_Collision(dG_in,A_input,E,k,q,Time)
 !
 ! The BKE is coded in terms of Ho + V + Sigma
 ! This subroutine construct Ho + V plus the time-diagonal part of Sigma(T=(t1 + t2)/2)
 ! In the TD-COHSEX we have that
 !
 !   H_k=Ho_k+U_k+ V_k^H-V_k^H[G_ref]+\Sigma_k^{SEX}-\Sigma_k^{SEX}[G_ref]
 !                 ----- A --------- -------- B -------------------------
 !
 ! Note that IP\equiv A=0 & B=0
 ! Note that TDLDA\equiv A=0 &  \Sigma=V_xc
 ! Note that TDH\equiv B=0
 !
 use pars,           ONLY:SP,cONE,cZERO
 use wrapper_omp,    ONLY:V_by_V_plus_V_omp
 use electrons,      ONLY:levels,n_spinor,n_spin
 use FFT_m,          ONLY:fft_size
 use drivers,        ONLY:l_sc_hf,l_sc_cohsex
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:V_hartree_sc,H_nl_sc,rho_n,magn_n,V_xc_sc
 use QP_m,           ONLY:QP_nk
 use real_time,      ONLY:Ho_plus_Sigma,l_NE_with_fields,l_RT_uses_E_DbGd,REF_V_xc_sc,&
&                         DbGd_ibz_coo,rho_reference,magn_reference,RT_Vnl_xc,&
&                         l_RT_RWA,eval_DENSITY,eval_HARTREE,eval_DFT,RT_ibz_coo,&
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
 call timing('RT Collision',OPR='start')
#endif
 !
 ! Contribution from \delta\Sigma_coherent
 !=========================================
 !
 ! Density (needed by Hartree and local XCs)
 !=========
 !
 if (eval_DENSITY) then
   !
   ! Contribution only from the changed occupations about RT_bands(1)
   !
   call el_density_matrix(dG_in,E,k,delta_rho,RT_bands(1))
   !
   rho_n=rho_reference+delta_rho
   if(n_spin>1) then
     call el_magnetization_matrix(dG_in,E,k,delta_magn,RT_bands(1))
     magn_n=magn_reference+delta_magn
   endif
   !
 endif
 !
 ! All the potentials are written in terms of the variations
 ! * The Hartree term: V_hartree(rho) = V_hartree(rho_reference) + V_hartree(delta_rho)
 ! * The V_xc    term: V_xc(rho)      = V_xc(rho_reference)      + {V_xc(rho) - V_xc(rho_reference)} 
 ! * Sigma_xc    term: S_xc(G)        ~ S_xc(G_reference)        + S_xc(delta_G)
 !   For Sigma_xc this is in general an approximation. It is exact only for self-energies 
 !                linear in G_lesser as exchange and COHSEX[W_eq] 
 !
 !  V_hartree(rho_reference) is in REF_V_hartree_sc
 !  V_xc(rho_reference)      is in REF_V_xc_sc
 !  S_xc(G_reference)        is introduced in the QP corrections to the eigenvalues
 !
 if (eval_HARTREE) call V_Hartree(delta_rho,V_hartree_sc)
 !
 ! The xc-part
 !-------------
 !
 ! The DFT-like
 !-------------
 if (eval_DFT) then
   !
   call XC_potential_driver(E,k,WF_KIND,WF_xc_functional,1,&
&                           MUTE="-report -log -io_in -io_out",unMUTE=master_cpu)
   !
   V_xc_sc=V_xc-REF_V_xc_sc
   !
 endif
 !
 ! Filling the BKE Kernel with the HXC part
 !==========================================
 Ho_plus_Sigma=cZERO
 !
 ! In the following loop there are operations spread among bands that 
 ! can be done by all cpu's that have PAR_IND_WF_k%element_1D(ik)=T.
 ! Some operations, instead, must be carried on only by the head CPU in this group
 ! identified by PAR_COM_Xk_ibz_A2A%CPU_id==0
 !
 ! The bare Hamiltonian
 !=========================
 if (.not.l_RT_RWA) then
   !
   do ik=1,QP_nk 
     !
     if (.not.PAR_IND_WF_k%element_1D(ik) .or. .not.HEAD_k_cpu ) cycle
     !
     ! DAVIDE:
     ! Ho+Sigma[eq]= E^{QP} 
     !
     do ik_RT=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       ik_DbGd=ik_RT-RT_ibz_coo(ik,1)+DbGd_ibz_coo(ik,1)
       do ib=RT_bands(1),RT_bands(2)
         if(     l_RT_uses_E_DbGd) Ho_plus_sigma(ib,ib,ik_RT,1)=E%E_DbGd(ib,ik_DbGd,1)
         if(.not.l_RT_uses_E_DbGd) Ho_plus_sigma(ib,ib,ik_RT,1)=E%E(ib,ik,1)
       enddo
     enddo
     !
   enddo
   !
 endif
 !
 ! The MB self-energy (compute RT_Vnl_xc)
 !========================================
 if (l_sc_hf.or.l_sc_cohsex) call COLLISIONS_compose_rt(dG_in)
 !
 ! HXC part 
 !===========
 do ik=1,QP_nk 
   !
   H_nl_sc=cZERO
   !
   ! Hartree and local XC potentials 
   !=================================
   !
   if (n_spinor==1 .and. (eval_DFT.or.eval_HARTREE)) call V_real_space_to_H(ik,V_hartree_sc+V_xc_sc,H_nl_sc,WF,'def')
   !
   if (n_spinor==2) then
     if(eval_HARTREE) call V_real_space_to_H(ik,V_hartree_sc,H_nl_sc,WF,'def')
     if(eval_DFT)     call V_real_space_to_H(ik,V_xc_sc     ,H_nl_sc,WF,'xc ')
   endif
   !
   if (.not.PAR_IND_WF_k%element_1D(ik).or. .not.HEAD_QP_cpu) cycle
   !
   do ik_RT =RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
     if (l_RT_RWA) then
       Ho_plus_sigma(RT_bands(1):RT_bands(2),&
&                    RT_bands(1):RT_bands(2),ik_RT,1)=H_nl_sc(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),1)
     else
       call V_by_V_plus_V_omp(RT_nbands,&
&                             cONE,H_nl_sc(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),1),&
&                             Ho_plus_sigma(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),ik_RT,1))
     endif
     !
     if (.not.HEAD_k_cpu) cycle
     !
     ! Non-local XC potentials (like COHSEX)
     !=======================================
     if(l_sc_hf.or.l_sc_cohsex) then
       call V_by_V_plus_V_omp(RT_nbands,cONE,RT_Vnl_xc(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),ik_RT,1),&
&                             Ho_plus_sigma(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),ik_RT,1))
     endif
   enddo
   !
 enddo
 !
 ! The External Field
 !====================
 do ik=1,QP_nk 
   !
   if (.not.PAR_IND_WF_k%element_1D(ik) .or. .not.HEAD_k_cpu) cycle
   !
   H_nl_sc=cZERO
   !
   ! The gauge field
   !=================
   if (l_NE_with_fields) call RT_apply_field(ik,H_nl_sc,A_input)
   !
   forall( ib=RT_bands(1):RT_bands(2) , ibp=RT_bands(1):RT_bands(2) , &
&          ik_RT=RT_ibz_coo(ik,1):RT_ibz_coo(ik,2) ) &
&            Ho_plus_Sigma(ib,ibp,ik_RT,1)= Ho_plus_Sigma(ib,ibp,ik_RT,1)+H_nl_sc(ib,ibp,1)
   !  
 enddo
 !
 ! All2All
 !=========
 call PP_redux_wait(Ho_plus_Sigma)
 !
#if defined _TIMING
 call timing('RT Collision',OPR='stop')
#endif
 !
 ! Calculate all relaxation terms
 !================================
 call RT_relaxation(dG_in,E,k,q,Time,.false.)
 !
end subroutine RT_Collision
