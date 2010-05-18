!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Collision(G_input,A_input,Xen,k,q,RT_time)
 !
 ! This subroutine prepare I1(t_1,t_2) and I2(t_1,t_2) give a G^<(t_1,t_1) 
 ! Actualy it works only for the diagonal I1(t_1,t_1), I2(t_1,t_1)
 !
 use pars,           ONLY:SP
 use wrapper,        ONLY:M_by_M,V_by_V_plus_V
 use electrons,      ONLY:levels
 use drivers,        ONLY:l_sc_contains_lda,l_sc_hf,l_sc_cohsex
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,V_hartree_sc,Ho,H_nl_sc,rho_n,V_xc_sc,local_V
 use QP_m,           ONLY:QP_nk,QP_Vnl_xc,QP_Sc
 use real_time,      ONLY:I1_lesser,I2_lesser,l_NE_with_fields,LifeTime,H_t,      &
 &                        Thermal_steps,Thermal_time,RT_step,XC0,Hfrozen,XCfrozen,&
 &                        G_lesser_reference,l_RT_needs_XC0,i_time,gauge_field
 use wave_func,      ONLY:wf
 use xc_functionals, ONLY:V_xc
 use global_XC,      ONLY:WF_kind,WF_xc_functional 
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes  
 use SC,             ONLY:SC_xc_functional
 use xc_functionals, ONLY:XC_LDA_C_PZ
 !
 implicit none
 !
 real(SP),      intent(in)    :: RT_time
 type(levels),  intent(in)    :: Xen
 type(bz_samp), intent(in)    :: k,q
 complex(SP),   intent(in)    :: G_input(SC_bands,SC_bands,QP_nk)
 type(gauge_field), intent(in):: A_input
 !
 ! Work Space
 !
 integer          :: ik,iv
 type(pp_indexes) :: px
 !
 if(.not.Hfrozen.or..not.XCfrozen.or.i_time<0) call el_density(G_input,k,rho_n,.false.)
 !
 if(.not.Hfrozen.or.i_time<0) call V_Hartree(rho_n,V_hartree_sc)
 !
 if(.not.XCfrozen.or.i_time<0) then
   !
   if(l_sc_contains_lda.and.SC_xc_functional/=0) then
     !
     call IO_and_Messaging_switch("-report -log -io_in -io_out")
     call xc_lda_driver(Xen,k,WF_KIND,WF_xc_functional,1,G_input)
     call IO_and_Messaging_switch("+report +log +io_in +io_out")
     V_xc_sc=V_xc
     !
   endif
   !
   if(l_sc_hf.or.l_sc_cohsex) call COLLISIONS_IO_compose(G_input)
   !
 else
   !  
   if(.not.local_V) QP_Vnl_xc=(0._SP,0._SP)
   if(l_sc_cohsex)      QP_Sc=(0._SP,0._SP)
   !
 endif
 !
 I1_lesser=(0._SP,0._SP)
 I2_lesser=(0._SP,0._SP)
 if(allocated(H_t)) H_t =  (0._SP,0._SP) 
 !
 if(l_NE_with_fields) call RT_Ext_Current(RT_time)
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/QP_nk/))
 !
 do ik=1,QP_nk 
   !
   if (.not.px%element_1D(ik)) cycle
   !
   H_nl_sc=(0._SP,0._SP)
   !
   if(.not.local_V) then
     call V_qp_basis_to_H(ik,QP_Vnl_xc,H_nl_sc) 
     if(l_sc_cohsex) call V_qp_basis_to_H(ik,QP_Sc,H_nl_sc) 
   endif
   !  
   ! Adiabatic Switching
   ! (for all the parts of self-energy different from V_h + V_xc)
   ! I have to subtruct XC0
   !
   if(Thermal_steps/=0.and.Thermal_time/=0.0) then 
     !      
     call V_by_V_plus_V(SC_bands,(1._SP,0._SP),-XC0(:,:,ik,1),H_nl_sc(:,:,1))
     !
     H_nl_sc=H_nl_sc*(1._SP+tanh((RT_time+(RT_step*real(Thermal_steps))/2._SP) &
    &        /(RT_step*real(Thermal_steps))/Thermal_time))/2._SP
     !
   endif
   !
   call V_real_space_to_H(ik,V_hartree_sc+V_xc_sc,H_nl_sc,wf)
   !
   if(l_RT_needs_XC0) call V_by_V_plus_V(SC_bands,(1._SP,0._SP),XC0(:,:,ik,1),H_nl_sc(:,:,1))
   !
   ! Add Ho = T+V_ion
   !
   call V_by_V_plus_V(SC_bands,(1._SP,0._SP),Ho(:,:,ik,1),H_nl_sc(:,:,1))
   !
   ! And the gauge field
   !
   if(l_NE_with_fields.and.i_time>0) call RT_apply_gauge_field(ik,H_nl_sc,A_input)
   !
   if(allocated(H_t))  H_t(:,:,ik,1)=H_nl_sc(:,:,1)
   !
   ! Now prepare I1 and I2 before the integration notice that 
   ! they contain also the external field
   !
   call M_by_M('n','n',SC_bands,H_nl_sc(:,:,1),G_input(:,:,ik),I1_lesser(:,:,ik))
   call M_by_M('n','n',SC_bands,G_input(:,:,ik),H_nl_sc(:,:,1),I2_lesser(:,:,ik))
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
   if(LifeTime/=0._SP) call V_by_V_plus_V(SC_bands,cmplx(0._SP,-1._SP/LifeTime,SP),&
&                                         G_input(:,:,ik)-G_lesser_reference(:,:,ik),I1_lesser(:,:,ik))
   !
 enddo  !---- loop on k-points
 !
 if(allocated(H_t)) call pp_redux_wait(H_t(:,:,:,1))
 call pp_redux_wait(I1_lesser)
 call pp_redux_wait(I2_lesser)
 call pp_indexes_reset(px) 
 !
end subroutine RT_Collision

