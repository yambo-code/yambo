!
!        Copyright (C) 2000-2014 the YAMBO team
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
 ! Note that G_lesser_reference and rho_reference are zero during
 ! the thermalization
 !
 ! This subroutine also fills SC_R with the full Hamiltonian if this is allocated
 !
 use pars,           ONLY:SP,cONE,cZERO
 use wrapper,        ONLY:V_by_V_plus_V
#if defined _OPENMP
 use wrapper,        ONLY:V_by_V_plus_V_omp
#endif
 use electrons,      ONLY:levels
 use FFT_m,          ONLY:fft_size
 use drivers,        ONLY:l_sc_ip,l_sc_is_libDFT,l_sc_hf,l_sc_cohsex
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,V_hartree_sc,Ho,H_nl_sc,rho_n,V_xc_sc,it_now,SC_R,&
&                         RT_ibz_coo,RT_nk,it_now
 use QP_m,           ONLY:QP_nk,QP_Vnl_xc
 use real_time,      ONLY:Ho_plus_Sigma,l_NE_with_fields,rho_full,l_RT_uses_E_RIM,&
&                         XC0_and_HARTREE0,RIM_ibz_coo,&
&                         l_NE_dynamics_started,l_RT_RWA,l_NE_simulate_Irredux,l_BGR
 use fields,         ONLY:gauge_field
 use wave_func,      ONLY:WF
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_WF_k,HEAD_k_cpu,master_cpu,HEAD_QP_cpu
 use xc_functionals, ONLY:V_xc,XC_LDA_C_PZ,XC_potential_driver
 use global_XC,      ONLY:WF_kind,WF_xc_functional 
 use SC,             ONLY:SC_nbands
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k,q
 real(SP),      intent(in)    :: Time
 complex(SP),   intent(in)    :: dG_in(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 type(gauge_field), intent(in):: A_input
 !
 ! Work Space
 !
 logical          :: eval_DENSITY,eval_HARTREE,eval_DFT
 integer          :: i1,ik,ik_RT,ikp_RT,ik_RIM,ib,ibp
 real(SP)         :: delta_rho(fft_size)
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
 ! l_NE_dynamics_started=.FALSE.
 ! corresponds to Thermalization where the density must be updated
 ! This part of the code is presently not used so I comment it
 !  
 eval_DFT     = l_sc_is_libDFT            .and..not.l_sc_ip
 eval_HARTREE = .not.l_NE_simulate_Irredux.and..not.l_sc_ip
 eval_DENSITY = eval_HARTREE .or. eval_DFT
 !
 if (eval_DENSITY) then
   !
   ! Contribution only from the changed occupations about SC_bands(1)
   !
   call el_density_matrix(dG_in(:,:,RT_ibz_coo(:,1)),E,k,delta_rho,SC_bands(1))
   !
   rho_n=rho_full+delta_rho
   !
 endif
 !
 ! The Hartree term is written as
 !------------------
 ! V_hartree(G) = V_hartree(rho-rho_0) + V_hartree(rho_0)
 !
 ! where V_hartree(rho_0) is stored in XC0_and_HARTREE0
 !
 ! Here I calculate V_hartree(rho-rho_0) => V_hartree_sc
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
   ! For V_xc I cannot write
   !
   !  V_xc(G) =/= V_xc(rho-rho_0) + V_xc(rho_0)
   !
   ! rather
   !
   !  V_xc(G) = V_xc(rho) 
   !
   ! note that XC0_and_HARTREE0 DOES NOT contains V_xc(rho_0) when V_xc is updated
   !
   call XC_potential_driver(E,k,WF_KIND,WF_xc_functional,1,&
&                           MUTE="-report -log -io_in -io_out",unMUTE=master_cpu)
   V_xc_sc=V_xc
   !
 endif
 !
 ! The MB-like
 !-------------
 !
 ! The exchange/cohsex term is written as Sigma(G - G_0) + Sigma(G_0)
 ! NOTICE THAT: this approximation is valid only for linear self-energies 
 !              in G_lesser as exchange or COHSEX(with W fixed) 
 !
 if (l_sc_hf.or.l_sc_cohsex) call COLLISIONS_compose(dG_in(:,:,RT_ibz_coo(:,1)),dG_in(:,:,RT_ibz_coo(:,1)))
 !
 ! Filling the BKE Kernel with the HXC part
 !==========================================
 !
 Ho_plus_Sigma=cZERO
 !
 ! In the following loop there are operations spread among bands that 
 ! can be done by all cpu's that have PAR_IND_WF_k%element_1D(ik)=T.
 ! Some operations, instead, must be carried on only by the head CPU in this group
 ! identified by PAR_COM_Xk_ibz_A2A%CPU_id==0
 !
 do ik=1,QP_nk 
   !
   if (.not.PAR_IND_WF_k%element_1D(ik).or..not.HEAD_QP_cpu) cycle
   !
   ik_RT =RT_ibz_coo(ik,1)
   ikp_RT=RT_ibz_coo(ik,2)
   !
   H_nl_sc=cZERO
   !
   ! Here I fill H_nl_sc with...
   !
   ! Non-local XC potentials (like COHSEX)
   !=======================================
   if (l_sc_hf.or.l_sc_cohsex) then
     ! 
     ! HF/COHSEX
     call V_qp_basis_to_H(ik,QP_Vnl_xc,H_nl_sc) 
     ! 
   endif
   !
   ! The bare, frozed VHxc 
   !-----------------------
   if (HEAD_k_cpu) then
#if defined _OPENMP
     call V_by_V_plus_V_omp(&
#else
     call V_by_V_plus_V(&
#endif
&                 SC_nbands,cONE,XC0_and_HARTREE0(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1), &
&                 H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1))
   endif
   !
   ! Local XC potentials 
   !=====================
   if (eval_DFT.or.eval_HARTREE) call V_real_space_to_H(ik,V_hartree_sc+V_xc_sc,H_nl_sc,WF)
   !  
   ! Add Ho = T+V_ion
   !==================
   if (HEAD_k_cpu) then
#if defined _OPENMP
     call V_by_V_plus_V_omp(&
#else
     call V_by_V_plus_V(&
#endif
&                       SC_nbands,cONE,Ho(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1),&
&                       H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1))
   endif
   !
   forall(ib=SC_bands(1):SC_bands(2),ibp=SC_bands(1):SC_bands(2)) Ho_plus_Sigma(ib,ibp,ik_RT:ikp_RT,1)=H_nl_sc(ib,ibp,1)

   !
 enddo  !---- loop on k-points
 !
 call PP_redux_wait(Ho_plus_Sigma(:,:,:,1))
 !
 ! External Field and RIM support
 !================================
 !
 do ik=1,QP_nk 
   !
   if (.not.(PAR_IND_WF_k%element_1D(ik).and.HEAD_k_cpu)) then
     Ho_plus_Sigma(:,:,RT_ibz_coo(ik,1):RT_ibz_coo(ik,2),:)=(0._SP,0._SP)
     cycle
   endif
   !
   do ik_RT=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
     ik_RIM=ik_RT-RT_ibz_coo(ik,1)+RIM_ibz_coo(ik,1)
     !
     H_nl_sc=cZERO
     !
     ! And the gauge field
     !=====================
     if (l_NE_with_fields.and.l_NE_dynamics_started) call RT_apply_field(ik,ik_RIM,E,H_nl_sc,A_input)
     !
     forall(ib=SC_bands(1):SC_bands(2),ibp=SC_bands(1):SC_bands(2)) &
&           Ho_plus_Sigma(ib,ibp,ik_RT,1)= Ho_plus_Sigma(ib,ibp,ik_RT,1)+H_nl_sc(ib,ibp,1)
     !
     if (l_RT_RWA) then
       !
       ! Apply the RWA
       !===============
       forall(ib=SC_bands(1):SC_bands(2)) Ho_plus_Sigma(ib,ib,ik_RT,1)=Ho_plus_Sigma(ib,ib,ik_RT,1)-E%E(ib,ik,1)
       !
     else if (l_RT_uses_E_RIM) then
       !
       ! RIM support
       !============
       forall(ib=SC_bands(1):SC_bands(2)) Ho_plus_Sigma(ib,ib,ik_RT,1)=Ho_plus_Sigma(ib,ib,ik_RT,1)-&
&                                         E%E(ib,ik,1)+E%E_RIM(ib,ik_RIM,1)
     endif
     !
   enddo
   !  
 enddo  !---- loop on k-points
 !
 ! All2All
 !=========
 call PP_redux_wait(Ho_plus_Sigma(:,:,:,1))
 !
 ! Rotation Matrix
 !=================
 if(allocated(SC_R)) then
   SC_R(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),:,:) = &
&      Ho_plus_Sigma(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_ibz_coo(:,1),:)
   do i1=1,SC_bands(1)-1
     SC_R(i1,i1,:,:)=cONE
   enddo
 endif
 !
#if defined _TIMING
 call timing('RT Collision',OPR='stop')
#endif
 !
 ! BGR
 !=====
 if (l_BGR) call RT_BGR(dG_in,E,k)
 !
 ! Calculate all relaxation terms
 !================================
 call RT_relaxation(dG_in,E,k,q,Time,.false.)
 !
end subroutine RT_Collision
