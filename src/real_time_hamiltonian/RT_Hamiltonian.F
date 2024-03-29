!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
subroutine RT_Hamiltonian(dG_in,A_input,E,k)
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
 use wrapper_omp,    ONLY:M_plus_alpha_M_omp
 use electrons,      ONLY:levels,n_spinor,n_spin
 use FFT_m,          ONLY:fft_size
 use drivers,        ONLY:l_use_Hxc_collisions
 use R_lattice,      ONLY:bz_samp,nXkibz
 use hamiltonian,    ONLY:V_hartree_sc,H_nl_sc,rho_n,magn_n,V_xc_sc
 use real_time,      ONLY:Ho_plus_Sigma,l_NE_with_fields,REF_V_xc_sc, &
&                         rho_reference,magn_reference,RT_Vnl_xc,H_EQ,         &
&                         l_RT_RWA,eval_HARTREE,eval_DFT,RTibz,RT_potential,      &
&                         RT_bands,RT_nbands,    &
&                         l_update_SOC,l_velocity_gauge_corr
 use fields,         ONLY:gauge_field
 use wave_func,      ONLY:WF
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_A2A,PAR_G_k_range
 use parallel_int,   ONLY:PP_redux_wait
 use xc_functionals, ONLY:V_xc,XC_potential_driver
 use global_XC,      ONLY:WF_xc_functional,WF_kind
 use collision_ext,  ONLY:COLLISIONS_have_HARTREE
 use timing_m,       ONLY:timing
 use H_interfaces,   ONLY:V_real_space_to_H
 !
 implicit none
 !
 type(levels),  intent(inout) :: E
 type(bz_samp), intent(in)    :: k
 complex(SP),   intent(in)    :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),PAR_G_k_range(1):PAR_G_k_range(2))
 type(gauge_field), intent(in):: A_input
 !
 ! Work Space
 !
 integer                  :: ik,ik_RT,ib
 logical                  :: l_H_contains_Hartree
 real(SP),    allocatable :: delta_rho(:),delta_magn(:,:)
 complex(SP), allocatable :: V_scf(:),V_scf_grad(:,:),V_xc_mat(:,:,:)
 !
 call timing('RT Hamiltonian',OPR='start')
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
 l_H_contains_Hartree=.FALSE.
 Ho_plus_Sigma=cZERO
 !
 ! Local part computed in real space (V_xc_sc and V_Hartree_sc)
 !=============================================================
 !
 if ( (.not.l_use_Hxc_collisions .and. .not.trim(RT_Potential)=='IP') .or. &
&     (.not.COLLISIONS_have_HARTREE .and. index(trim(RT_potential),"HARTREE")>0) ) then
   !
   ! Density & Magnetization (needed for VHartree and Vxc)
   !--------------------------
   allocate(delta_rho(fft_size))
   call el_density_matrix(dG_in,E,k,delta_rho,RT_bands(1))
   rho_n=rho_reference+delta_rho
   !
   ! The VHartree term
   !-------------------
   if (eval_HARTREE) call V_Hartree(delta_rho,V_hartree_sc)
   !
   deallocate(delta_rho)
   !
   if(n_spin>1 .and. eval_DFT) then
     allocate(delta_magn(fft_size,3))
     call el_magnetization_matrix(dG_in,E,k,delta_magn,RT_bands(1))
     magn_n=magn_reference+delta_magn
     deallocate(delta_magn)
   endif
   !
   ! The Vxc term
   !--------------
   if (eval_DFT) then
     call XC_potential_driver(E,k,WF_kind,WF_xc_functional,1)
     V_xc_sc=V_xc-REF_V_xc_sc
   endif
   !
   ! Update of SOC term
   !
   if(n_spinor==2 .and. l_update_SOC ) then
     !
     allocate(V_scf(fft_size))
     V_scf=cZERO
     if(eval_HARTREE) V_scf=V_scf+V_Hartree_sc(:,1)
     if(eval_DFT)     then
       allocate(V_xc_mat(fft_size,n_spinor,n_spinor))
       call Build_V_xc_mat(V_xc_sc,V_xc_mat)
       V_scf=V_scf+V_xc_mat(:,1,1)+V_xc_mat(:,2,2)
       deallocate(V_xc_mat)
     endif
     allocate(V_scf_grad(fft_size,3))
     call eval_Gradient(V_scf,V_scf_grad,1,"potential")
     deallocate(V_scf)
     !
   endif
   !
   ! Add H_nl_sc to Ho_plus_Sigma 
   !---------------------------------
   do ik=1,nXkibz 
     !
     H_nl_sc=cZERO
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik) ) cycle
     !
     call V_real_space_to_H(ik,1,H_nl_sc,WF,'def+xc',V=V_hartree_sc,Vxc=V_xc_sc)
     if(n_spinor==2.and.l_update_soc) call Vgrad_real_space_to_H(ik,H_nl_sc,WF,V_scf_grad)
     !
     call PP_redux_wait(H_nl_sc,COMM=PAR_COM_Xk_ibz_A2A%COMM)
     !
     forall(ik_RT=RTibz%k_range(ik,1):RTibz%k_range(ik,2)) Ho_plus_Sigma(:,:,ik_RT,1)=H_nl_sc
     !
   enddo
   !
   l_H_contains_Hartree=index(trim(RT_potential),"HARTREE")>0
   if(allocated(V_scf_grad)) deallocate(V_scf_grad)
   !
 endif
 !
 ! Local and/or non local part computed with collisions (RT_Vnl_xc)
 !======================================================
 if (l_use_Hxc_collisions) then
   !
   call COLLISIONS_compose_rt(dG_in)
   !
   ! Add RT_Vnl_xc to Ho_plus_Sigma 
   !---------------------------------
   do ik=1,nXkibz 
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     if (l_H_contains_HARTREE) then
       do ik_RT =RTibz%k_range(ik,1),RTibz%k_range(ik,2)
         Ho_plus_Sigma(:,:,ik_RT,1)=Ho_plus_Sigma(:,:,ik_RT,1)+RT_Vnl_xc(:,:,ik,1)
       enddo
     else
       do ik_RT =RTibz%k_range(ik,1),RTibz%k_range(ik,2)
         Ho_plus_Sigma(:,:,ik_RT,1)=RT_Vnl_xc(:,:,ik,1)
       enddo
     endif
   enddo
   !
 endif
 !
 ! The External Field
 !====================
 if (l_NE_with_fields) then
   !
   call timing('RT Apply Field',OPR='start')
   !
   do ik=1,nXkibz 
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     H_nl_sc=cZERO
     call RT_apply_field(ik,1,H_nl_sc,A_input)
     do ik_RT =RTibz%k_range(ik,1),RTibz%k_range(ik,2)
       call M_plus_alpha_M_omp(RT_nbands,cONE,H_nl_sc,Ho_plus_sigma(:,:,ik_RT,1))
     enddo
   enddo
   !
   call timing('RT Apply Field',OPR='stop')
   !
 endif
 !
 ! The KS Hamiltonian
 !====================
 if (.not.l_RT_RWA) Ho_plus_Sigma=Ho_plus_Sigma+H_EQ
 !
 ! Pseudo potential terms needed for velocity gauge
 !==================================================
 if (l_velocity_gauge_corr) call Pseudo_Hamiltonian(k,E,.false.)
 !
 call timing('RT Hamiltonian',OPR='stop')
 !
end subroutine RT_Hamiltonian
