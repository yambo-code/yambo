!
! Copyright (C) 2000-2012 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Collision(G_input,A_input,Xen,k,q,Time)
 !
 ! The BKE is coded in terms of I1(T) (left time) and I2(T) (right time) kernels.
 ! In the TD-COHSEX we have that
 !
 !  i\partial_T G_k^<(T) = I1(T)-I2(T)
 !
 ! where
 !
 !  I1(T)=O_k G_^k + S_matrix_k(T)
 !  I2(T)=-G_^k O_k
 !
 ! and 
 !
 !   O_k=h_k+U_k+ V_k^H-V_k^H[G_ref]+\Sigma_k^{SEX}-\Sigma_k^{SEX}[G_ref]
 !                ------ A --------- -------- B -------------------------
 !
 ! Note that IP\equiv A=0 & B=0
 ! Note that TDLDA\equiv A=0 &  \Sigma=V_xc
 ! Note that TDH\equiv B=0
 !
 ! S_matrix_k(T) is the el-ph term that includes memory.
 !
 ! Note that G_lesser_reference and rho_reference are zero during
 ! the thermalization
 !
 ! This subroutine also fills SC_R with the full Hamiltonian if this is allocated
 !
 use pars,           ONLY:SP,cI,cONE
 use wrapper,        ONLY:M_by_M,V_by_V_plus_V
 use electrons,      ONLY:levels
 use drivers,        ONLY:l_sc_contains_lda,l_sc_hf,l_sc_cohsex,l_ph_corr
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,V_hartree_sc,Ho,H_nl_sc,rho_n,V_xc_sc,local_V,it_now,SC_R
 use QP_m,           ONLY:QP_nk,QP_Vnl_xc,QP_Sc
 use real_time,      ONLY:I1_lesser,I2_lesser,l_NE_with_fields,full_rho,          &
&                         Thermal_steps,SwitchOnTime,RT_step,XC0_and_HARTREE0,rho_reference,   & 
&                         G_lesser_reference,NE_Hxc_jumps,                      &
&                         RT_is_dumped,l_NE_simulate_IP,Hfrozen,XCfrozen,NE_dynamics,it_start, &
&                         RT_Deph_Matrix,l_RT_needs_XC0,RT_k_px
 use fields,         ONLY:gauge_field
 use wave_func,      ONLY:wf
 use xc_functionals, ONLY:V_xc
 use global_XC,      ONLY:WF_kind,WF_xc_functional 
 use SC,             ONLY:SC_xc_functional,SC_nbands
 use xc_functionals, ONLY:XC_LDA_C_PZ
 use par_proc_m,     ONLY:pp_redux_wait 
 !
 implicit none
 !
 type(levels),  intent(in)    :: Xen
 type(bz_samp), intent(in)    :: k,q
 real(SP),      intent(in)    :: Time
 complex(SP),   intent(in)    :: G_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 type(gauge_field), intent(in):: A_input
 !
 ! Work Space
 !
 logical          :: XC_needs_density
 integer          :: i1,i2,ik,ikp
 !
 ! Density (needed bu Hartree and local XCs)
 !=========
 !
 ! NE_dynamics=.FALSE. corresponds to Thermalization where the density must be updated
 !
 XC_needs_density=.not.XCfrozen.and.l_sc_contains_lda.and.SC_xc_functional/=0
 !
 if (it_now==it_start.or.mod(it_now-it_start+1,NE_Hxc_jumps)==0) then
   !
   if(.not.Hfrozen.or.XC_needs_density.or..not.NE_dynamics) call el_density(G_input,Xen,k,rho_n,SC_bands(1),.false.)
   !
   ! The Hartree term is written as
   !
   ! V_hartree(G) = V_hartree(rho-rho_0) + V_hartree(rho_0)
   !
   ! where V_hartree(rho_0) is stored in XC0_and_HARTREE0
   !
   if (.not.Hfrozen.or..not.NE_dynamics) call V_Hartree(rho_n-rho_reference,V_hartree_sc)
   !
   if(.not.XCfrozen.or..not.NE_dynamics) then
     !
     if(l_sc_contains_lda.and.SC_xc_functional/=0) then
       !
       ! Reconstruct the full density
       !
       if (SC_bands(1)>1) rho_n=full_rho+rho_n-rho_reference
       !
       call IO_and_Messaging_switch("-report -log -io_in -io_out")
       call XC_potential_driver(Xen,k,WF_KIND,WF_xc_functional,1)
       call IO_and_Messaging_switch("+report +log +io_in +io_out")
       V_xc_sc=V_xc
       !
     endif
     !
     ! The exchange/cohsex term is written as Sigma(G - G_0) - Sigma(G)
     ! NOTICE THAT: this approximation is valid only for linear self-energies 
     !              in G_lesser as exchange or COHSEX(with W fixed) 
     !
     if(l_sc_hf.or.l_sc_cohsex) call COLLISIONS_IO_compose(G_input-G_lesser_reference)
     !
   endif
   !
 endif
 !
 I1_lesser=(0._SP,0._SP)
 I2_lesser=(0._SP,0._SP)
 if(allocated(SC_R)) then
    SC_R =  (0._SP,0._SP) 
    do i1=1,SC_bands(1)-1
      SC_R(i1,i1,:,:)=cONE
    enddo
 endif
 !
 do ik=1,QP_nk 
   !
   if (.not.RT_k_px%element_1D(ik)) cycle
   !
   ! Here I fill H_nl_sc (which defined I1 and I2) with...
   !
   H_nl_sc=(0._SP,0._SP)
   !
   ! Non-local XC potentials (like CHOSEX)
   !=======================================
   if(.not.local_V.and..not.XCfrozen) then
     ! HF
     call V_qp_basis_to_H(ik,QP_Vnl_xc,H_nl_sc) 
     ! CHOSEX
     if (l_sc_cohsex) call V_qp_basis_to_H(ik,QP_Sc,H_nl_sc) 
   endif
   !
   ! Adiabatic Switching
   !=====================
   ! (for all the parts of self-energy different from V_h + V_xc)
   ! I have to subtruct XC0_and_HARTREE0 that contains <mk|Vxc(rho0) |nk>
   !
   if(Thermal_steps/=0.and.SwitchOnTime/=0.0.and..not.NE_dynamics) then 
     !      
     call V_by_V_plus_V(SC_nbands,cONE,-XC0_and_HARTREE0(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1), &
&                      H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1))
     !
     H_nl_sc=H_nl_sc*(1._SP+tanh((Time+(RT_step*real(Thermal_steps))/2._SP)/ &
&            (RT_step*real(Thermal_steps))/SwitchOnTime))/2._SP
     !
   endif
   !
   ! The bare, frozed VHxc 
   !=======================
   if (l_RT_needs_XC0) & 
&     call V_by_V_plus_V(SC_nbands,cONE,XC0_and_HARTREE0(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1), &
&                        H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1))
   !
   ! Local XC potentials 
   !=====================
   if(.not.l_NE_simulate_IP) call V_real_space_to_H(ik,V_hartree_sc+V_xc_sc,H_nl_sc,wf)
   !  
   ! Add Ho = T+V_ion
   !==================
   call V_by_V_plus_V(SC_nbands,cONE,Ho(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1),&
&                     H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1))
   !
   ! And the gauge field
   !=====================
   if(l_NE_with_fields.and.NE_dynamics) call RT_apply_field(ik,H_nl_sc,A_input)
   !
   if(allocated(SC_R))  SC_R(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1)= &
&                            H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1)
   !
   ! Now I1 and I2 
   !===============
   !
   call M_by_M('n','n',SC_nbands,H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1),G_input(:,:,ik),I1_lesser(:,:,ik))
   call M_by_M('n','n',SC_nbands,G_input(:,:,ik),H_nl_sc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),1),I2_lesser(:,:,ik))
   !
#if defined _ELPH
   !
   ! El-Ph
   !=======
   if (l_ph_corr) call RT_ELPH_total_Kernel(ik,Xen,k,q,Time)
   !
#endif
   !
   ! Damping
   !=========
   !
   ! Fictitious lifetime equivalent to the broadening
   ! in linear optics
   !   
   ! This term kill the induced polarization, namely off-diagonal term in the
   ! Green function
   !
   ! This term send the electron distribution into the initial one
   ! G_lesser_refence(:,:,:) 
   !
   if(RT_is_dumped.and.NE_dynamics) then
     !
     do i1=SC_bands(1),SC_bands(2)
       do i2=SC_bands(1),SC_bands(2)
         I1_lesser(i1,i2,ik)=I1_lesser(i1,i2,ik)-cI/RT_Deph_Matrix(i1,i2,ik)*(G_input(i1,i2,ik)-G_lesser_reference(i1,i2,ik))
       enddo
     enddo
     !
   endif
   !
 enddo  !---- loop on k-points
 !
 if(allocated(SC_R)) call pp_redux_wait(SC_R(:,:,:,1))
 ! 
 ! When there is the el-ph Kernel the parallelization is done on the q-points 
 ! 
 if (.not.l_ph_corr) then
   call pp_redux_wait(I1_lesser)
   call pp_redux_wait(I2_lesser)
 endif
 !
end subroutine RT_Collision

