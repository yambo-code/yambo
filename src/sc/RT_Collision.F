!
! Copyright (C) 2000-2008 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Collision(G_input,Xen,k,q,RT_time)
 !
 ! This subroutine prepare I1(t_1,t_2) and I2(t_1,t_2) give a G^<(t_1,t_1) 
 ! Actualy it works only for the diagonal I1(t_1,t_1), I2(t_1,t_1)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels,n_sp_pol
 use drivers,        ONLY:l_sc_contains_lda,l_sc_hf
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,V_hartree_sc,Ho,H_nl_sc,rho_n
 use QP_m,           ONLY:QP_nk,QP_Sx
 use real_time,      ONLY:I1_lesser,I2_lesser,l_RT_no_updates,l_qp_analyze,         &
 &                        l_rt_efield,LifeTime,H_t,l_RPA,H_Hartree_Vxc_0,V_xc_0,    &
 &                        Thermal_time,Integrator_name,EXACT,RK2EXACT,A_norm,       &
 &                        Thermal_eta, i_time,Efield, &
 &                        G_lesser_reference,n_ext_fields
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
 real(SP),      intent(in)   :: RT_time
 type(levels),  intent(in)   :: Xen
 type(bz_samp), intent(in)   :: k,q
 complex(SP),   intent(in)   :: G_input(SC_bands,SC_bands,QP_nk)
 !
 ! Work Space
 !
 integer     :: ik,iv,i1,SC_bands2
 type(pp_indexes) ::px
 !
 SC_bands2=SC_bands**2
 !
 if(.not.l_RT_no_updates) then
   !
   call el_density(G_input,k,rho_n,.false.)
   !
   call V_Hartree(rho_n,V_hartree_sc)
   V_hartree_sc(:,n_sp_pol)=V_hartree_sc(:,1)
   !
   if(l_sc_contains_lda.and.SC_xc_functional/=0) then
     !
     call IO_mute("report log io_in io_out")
     call xc_lda_driver(Xen,k,WF_KIND,WF_xc_functional,1,G_input)
     call IO_mute("  ")
     !
   endif
   !
 endif
 !
 ! l_RT_no_updates=.true.
 !
 ! Simple case without any correlation just transition between bands
 ! equivalent to RPA without local field effects
 !
 I1_lesser=(0.,0.)
 I2_lesser=(0.,0.)
 if(allocated(H_t)) H_t =  (0.,0.) 
 !
 call RT_make_QP_Sx(G_input)
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/QP_nk/))
 !
 do ik=1,QP_nk 
   !
   if (.not.px%i1p(ik)) cycle
   !
   H_nl_sc=(0.,0.)
   !
   if(.not.l_RT_no_updates) then
     !
     if(l_sc_hf) then
       !
       ! Add the exchange term
       !
       call V_qp_basis_to_H(ik,QP_Sx,H_nl_sc) 
       !
     endif
     !
     !  I subtract the V_xc term if there is a self-energy /=0 (at least V_h)
     !  and not l_RPA=.true. (used to include local field effects only)
     !
     if(.not.l_RPA) call V_real_space_to_H(ik,-V_xc_0,H_nl_sc,wf) 
     !
   endif
   !
   ! Adiabatic Switching
   ! (for all the parts of self-energy different from V_h + V_xc)
   !
   if(Thermal_time/=0.0) then 
     !      
     H_nl_sc=H_nl_sc*(1._SP+tanh((RT_time+Thermal_time/2._SP)/Thermal_time*Thermal_eta))/2._SP
     !
   endif
   !
   ! Put the V_Hartree and V_xc
   !
   if(.not.l_RT_no_updates) then
     !      
     if(l_sc_contains_lda.and.SC_xc_functional/=0) then
       !
       call V_real_space_to_H(ik,V_hartree_sc+V_xc,H_nl_sc,wf)
       !
     else
       !      
       call V_real_space_to_H(ik,V_hartree_sc+V_xc_0,H_nl_sc,wf)
       !
     endif
     !
   else
     !   
#if defined _DOUBLE
     call zaxpy(SC_bands2,(1.,0.),H_Hartree_Vxc_0(:,:,ik,1),1,H_nl_sc(:,:,1),1)
#else
     call caxpy(SC_bands2,(1.,0.),H_Hartree_Vxc_0(:,:,ik,1),1,H_nl_sc(:,:,1),1)
#endif
     !
   endif
   !
   ! Ho is part of the Hamiltonian (or singular self-energy H_nl_sc) 
   !
#if defined _DOUBLE
   call zaxpy(SC_bands2,(1.,0.),Ho(:,:,ik,1),1,H_nl_sc(:,:,1),1)
#else
   call caxpy(SC_bands2,(1.,0.),Ho(:,:,ik,1),1,H_nl_sc(:,:,1),1)
#endif
   !
   ! And now add the external field
   !
   if(l_rt_efield.and.i_time>0) then
     !
     do i1=1,n_ext_fields
       if(Efield(i1)%ef_name/='none') call RT_Ext_Field(ik,H_nl_sc,G_input,RT_time,Efield(i1),A_norm(i1))
     enddo
     !
   endif
   !
   if(l_qp_analyze.or.trim(Integrator_name)==EXACT.or.trim(Integrator_name)==RK2EXACT) then
     H_t(:,:,ik,1)=H_nl_sc(:,:,1)
   endif               
   !
   ! Now prepare I1 and I2 before the integration notice that 
   ! they contain also the external field
   !
#if defined _DOUBLE
   call zgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),H_nl_sc(:,:,1),&
&             SC_bands,G_input(:,:,ik),SC_bands,(0._SP,0._SP),I1_lesser(:,:,ik),SC_bands)
   call zgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),G_input(:,:,ik),&
&             SC_bands,H_nl_sc(:,:,1),SC_bands,(0._SP,0._SP),I2_lesser(:,:,ik),SC_bands)
#else
   call cgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),H_nl_sc(:,:,1),&
&             SC_bands,G_input(:,:,ik),SC_bands,(0._SP,0._SP),I1_lesser(:,:,ik),SC_bands)
   call cgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),G_input(:,:,ik),&
&             SC_bands,H_nl_sc(:,:,1),SC_bands,(0._SP,0._SP),I2_lesser(:,:,ik),SC_bands)
#endif
   !
   ! Life-time always for optics 
   !
   if(i_time<0) cycle
   !
   ! Fictitious lifetime equivalent to the broadening
   ! in linear optics
   !
   if(LifeTime/=0._SP) then
     !   
     ! This term kill the induced polarization, namley off-diagonal term in the
     ! Green function
     !
     ! This term send the electron distribution into the initial one
     ! G_lesser_refence(:,:,:) 
     !
#if defined _DOUBLE
     call zaxpy(SC_bands2,dcmplx(0.,-1./LifeTime), &
             &  G_input(:,:,ik)-G_lesser_reference(:,:,ik),1,I1_lesser(:,:,ik),1)
#else
     call caxpy(SC_bands2, cmplx(0.,-1./LifeTime), &
             &  G_input(:,:,ik)-G_lesser_reference(:,:,ik),1,I1_lesser(:,:,ik),1)
#endif
     !
   endif
   !
 enddo  !---- loop on k-points
 !
 if(allocated(H_t)) call pp_redux_wait(H_t(:,:,:,1))
 call pp_redux_wait(I1_lesser)
 call pp_redux_wait(I2_lesser)
 call pp_indexes_reset(px) 
 !
end subroutine RT_Collision

