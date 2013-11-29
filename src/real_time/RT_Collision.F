!
! Copyright (C) 2000-2013 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Collision(G_input,A_input,E,k,q,Time)
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
 use drivers,        ONLY:l_sc_contains_lda,l_sc_hf,l_sc_cohsex
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,V_hartree_sc,Ho,H_nl_sc,rho_n,V_xc_sc,local_V,it_now,SC_R,&
&                         RT_ibz_coo,RT_nk,it_now
 use QP_m,           ONLY:QP_nk,QP_Vnl_xc
 use real_time,      ONLY:Ho_plus_Sigma,l_NE_with_fields,full_rho,                             &
&                         Thermal_steps,SwitchOnTime,RT_step,XC0_and_HARTREE0,rho_reference,   & 
&                         G_lesser_reference,                                                  &
&                         l_NE_simulate_IP,Hfrozen,XCfrozen,NE_dynamics,                       &
&                         l_RT_needs_XC0,Ho_plus_Sigma
 use fields,         ONLY:gauge_field
 use wave_func,      ONLY:WF
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_WF_k,HEAD_k_cpu,master_cpu,HEAD_QP_cpu,myid
 use xc_functionals, ONLY:V_xc,XC_LDA_C_PZ
 use global_XC,      ONLY:WF_kind,WF_xc_functional 
 use SC,             ONLY:SC_xc_functional,SC_nbands
 use IO_m,           ONLY:IO_and_Messaging_switch
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k,q
 real(SP),      intent(in)    :: Time
 complex(SP),   intent(in)    :: G_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 type(gauge_field), intent(in):: A_input
 !
 ! Work Space
 !
 logical          :: XC_needs_density
 integer          :: i1,ik
 !
#if defined _TIMING
 call timing('RT Collisions',OPR='start')
#endif
 !
 ! Density (needed by Hartree and local XCs)
 !=========
 !
 ! NE_dynamics=.FALSE. corresponds to Thermalization where the density must be updated
 !
 XC_needs_density=.not.XCfrozen.and.l_sc_contains_lda.and.SC_xc_functional/=0
 !
 if (.not.Hfrozen.or.XC_needs_density.or..not.NE_dynamics) &
&   call el_density_matrix(G_input(:,:,RT_ibz_coo(:,1)),E,k,rho_n,SC_bands(1))
 !
 ! The Hartree term is written as
 !
 ! V_hartree(G) = V_hartree(rho-rho_0) + V_hartree(rho_0)
 !
 ! where V_hartree(rho_0) is stored in XC0_and_HARTREE0
 !
 if (.not.Hfrozen.or..not.NE_dynamics) call V_Hartree(rho_n-rho_reference,V_hartree_sc)
 !
 if (.not.XCfrozen.or..not.NE_dynamics) then
   !
   if(l_sc_contains_lda.and.SC_xc_functional/=0) then
     !
     ! Reconstruct the full density
     !
     if (SC_bands(1)>1) rho_n=full_rho+rho_n-rho_reference
     !
     call IO_and_Messaging_switch("-report -log -io_in -io_out")
     call XC_potential_driver(E,k,WF_KIND,WF_xc_functional,1)
     call IO_and_Messaging_switch("+report +log +io_in +io_out",CONDITION=master_cpu)
     V_xc_sc=V_xc
     !
   endif
   !
   ! The exchange/cohsex term is written as Sigma(G - G_0) - Sigma(G)
   ! NOTICE THAT: this approximation is valid only for linear self-energies 
   !              in G_lesser as exchange or COHSEX(with W fixed) 
   !
   if (l_sc_hf.or.l_sc_cohsex) then
     call COLLISIONS_compose(G_input(:,:,RT_ibz_coo(:,1))-G_lesser_reference(:,:,RT_ibz_coo(:,1)),&
&                            G_input(:,:,RT_ibz_coo(:,1))-G_lesser_reference(:,:,RT_ibz_coo(:,1)))
   endif
   !
 endif
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
   H_nl_sc=cZERO
   !
   ! Here I fill H_nl_sc with...
   !
   ! Non-local XC potentials (like COHSEX)
   !=======================================
   if(.not.local_V.and..not.XCfrozen) then
     ! 
     ! HF/COHSEX
     call V_qp_basis_to_H(ik,QP_Vnl_xc,H_nl_sc) 
     ! 
   endif
   !
   ! Adiabatic Switching
   !=====================
   ! (for all the parts of self-energy different from V_h + V_xc)
   ! I have to subtract XC0_and_HARTREE0 that contains <mk|Vxc(rho0) |nk>
   !
   if(Thermal_steps/=0.and.SwitchOnTime/=0.0.and..not.NE_dynamics.and.HEAD_k_cpu) then 
     !      
#if defined _OPENMP
     call V_by_V_plus_V_omp(SC_nbands,cONE,-XC0_and_HARTREE0(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1), &
&                           H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1))
#else
     call V_by_V_plus_V(SC_nbands,cONE,-XC0_and_HARTREE0(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1), &
&                       H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1))
#endif
     !
     H_nl_sc=H_nl_sc*(1._SP+tanh((Time+(RT_step*real(Thermal_steps))/2._SP)/ &
&                    (RT_step*real(Thermal_steps))/SwitchOnTime))/2._SP
     !
   endif
   !
   ! The bare, frozed VHxc 
   !=======================
   if (l_RT_needs_XC0.and.HEAD_k_cpu) then
#if defined _OPENMP
     call V_by_V_plus_V_omp(SC_nbands,cONE,XC0_and_HARTREE0(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1), &
&                           H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1))
#else
     call V_by_V_plus_V(SC_nbands,cONE,XC0_and_HARTREE0(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1), &
&                       H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1))
#endif
   endif
   !
   ! Local XC potentials 
   !=====================
   if (.not.l_NE_simulate_IP) call V_real_space_to_H(ik,V_hartree_sc+V_xc_sc,H_nl_sc,WF)
   !  
   ! Add Ho = T+V_ion
   !==================
   if (HEAD_k_cpu) then
#if defined _OPENMP
     call V_by_V_plus_V_omp(SC_nbands,cONE,Ho(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1),&
&                           H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1))
#else
     call V_by_V_plus_V(SC_nbands,cONE,Ho(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1),&
&                       H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1))
#endif
   endif
   !
   Ho_plus_Sigma(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1)= &
&        H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1)
   !
 enddo  !---- loop on k-points
 !
 call PP_redux_wait(Ho_plus_Sigma(:,:,:,1))
 !
 do ik=1,QP_nk 
   !
   if (.not.(PAR_IND_WF_k%element_1D(ik).and.HEAD_k_cpu)) then
     Ho_plus_Sigma(:,:,ik,:)=(0._SP,0._SP)
     cycle
   endif
   !
   H_nl_sc=cZERO
   !
   ! And the gauge field
   !=====================
   if (l_NE_with_fields.and.NE_dynamics) call RT_apply_field(ik,H_nl_sc,A_input)
   !
   Ho_plus_Sigma(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1)= &
&                Ho_plus_Sigma(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1)+&
&                H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1)
   !  
 enddo  !---- loop on k-points
 !
 call PP_redux_wait(Ho_plus_Sigma(:,:,:,1))
 !
 if(allocated(SC_R)) then
   SC_R(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),:,:) = Ho_plus_Sigma
   do i1=1,SC_bands(1)-1
     SC_R(i1,i1,:,:)=cONE
   enddo
 endif
 !
#if defined _TIMING
 call timing('RT Collisions',OPR='stop')
#endif
 !
 ! Calculate all relaxation terms
 ! ==============================
 call RT_relaxation(G_input,E,k,q,Time)
 !
end subroutine RT_Collision

