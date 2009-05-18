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
subroutine RT_Collision(G_input,en,Xen,k,q,RT_time)
 !
 ! This subroutine prepare I1(t_1,t_2) and I2(t_1,t_2) give a G^<(t_1,t_1) 
 ! Actualy it works only for the diagonal I1(t_1,t_1), I2(t_1,t_1)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels,el_density,n_sp_pol
 use drivers,        ONLY:l_sc_exx,l_sc_chosex,l_sc_hartree,l_sc_pz,l_sc_lda_x,&
&                         l_sc_hf,l_sc_exxc
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,V_hartree_sc,Ho
 use QP_m,           ONLY:QP_nk,QP_Vxc,QP_n_states,QP_table 
 use real_time,      ONLY:I1_lesser,I2_lesser,V_xc_0,l_RT_no_updates,rho,V_hartree_0, &
 &                        Thermal_Time,l_rt_efield,TransRelax,LongRelax,T_initial, &
 &                        H_t,Hartree_plus_Vxc_0,l_RPA
 use wave_func,      ONLY:wf
 use xc_functionals, ONLY:V_xc
 use global_XC,      ONLY:WF_kind,WF_xc_functional 
 !
 implicit none
 !
 real(SP),      intent(in)   :: RT_time
 type(levels),  intent(in)   :: en,Xen
 type(bz_samp), intent(in)   :: k,q
 complex(SP),   intent(in)   :: G_input(SC_bands,SC_bands,en%nk)
 !
 ! Work Space
 !
 integer     :: ik,iv,i1,i2,i3,ik1,ib,ibp,i_spin,SC_bands2
 complex(SP) :: Sigma_s(SC_bands,SC_bands,n_sp_pol),tmp,tmp1,tmp2
 real(SP)    :: RelaxDiff
 !
 SC_bands2=SC_bands**2
 !
 ! Update occupations
 !
 forall(iv=1:SC_bands,ik=1:Xen%nk) 
   Xen%f(iv,ik,1)=-(0._SP,1._SP)*G_input(iv,iv,ik)
 end forall
 !
 if(.not.l_RT_no_updates) call el_density(Xen,k,rho,G_lesser=G_input)
 !
 if(.not.l_RT_no_updates) then
   !
   call V_Hartree(rho,V_hartree_sc)
   !
   if(l_sc_pz) then
     !
     call xc_lda_driver(Xen,k,WF_KIND,WF_xc_functional,1)
     !
   else
     !
     V_xc=V_xc_0
     !
   endif
   !
 else
   !
   ! Simple case without any correlation just transition between bands
   ! equivalent to RPA without local field effects
   !
   V_hartree_sc     =V_hartree_0
   V_xc             =V_xc_0
   !
 endif
 !
 ! l_rt_exc=.false.
 !
 ! Simple case without any correlation just transition between bands
 ! equivalent to RPA without local field effects
 !
 !
 do ik=1,QP_nk 
   !      
   Sigma_s=(0._SP,0._SP)
   !
   !  I subtract the V_xc_0 term if there is a self-energy /=0 (at least V_h)
   !  and not l_keep_vxc=.true. (used to include local field effects only)
   !
   if(.not.l_RT_no_updates.and..not.l_RPA) then 
     !  
     call V_to_H(ik,-V_xc_0,Sigma_s,wf)
     !
   endif
   !
   ! Adiabatic Switching
   !
   if(Thermal_time/=0.0) then 
     !      
     Sigma_s=Sigma_s*(1._SP+tanh((RT_time+Thermal_time/2._SP)/Thermal_time*8._SP))/2._SP
     !
   endif
   !
   ! Fill the Singular (in time) Self-Energy Sigma_s(:,:,:)
   !
   if(.not.l_RT_no_updates) then
     call V_to_H(ik,V_hartree_sc+V_xc,Sigma_s,wf)
   else
#if defined _DOUBLE
     call zaxpy(SC_bands2,(1.,0.),Hartree_plus_Vxc_0(:,:,ik),1,Sigma_s(:,:,1),1)
#else
     call caxpy(SC_bands2,(1.,0.),Hartree_plus_Vxc_0(:,:,ik),1,Sigma_s(:,:,1),1)
#endif
   endif
   !
   ! Ho is part of the Hamiltonian (or singular self-energy Sigma_s) 
   !
#if defined _DOUBLE
   call zaxpy(SC_bands2,(1.,0.),Ho(:,:,ik,1),1,Sigma_s(:,:,1),1)
#else
   call caxpy(SC_bands2,(1.,0.),Ho(:,:,ik,1),1,Sigma_s(:,:,1),1)
#endif
   !
!   
! RT phase section   
!  
!   else
!     !
!     ! I use H_o + V_h(rho_o) + V_xc(rho_o) as phase factor 
!     ! so I subtract them from the Self-Energy Sigma_S(:,:,:)
!     !      
!#if defined _DOUBLE
!     call zaxpy(SC_bands2,(-1.,0.),Hartree_plus_Vxc_0(:,:,ik),1,Sigma_s(:,:,1),1)
!#else
!     call caxpy(SC_bands2,(-1.,0.),Hartree_plus_Vxc_0(:,:,ik),1,Sigma_s(:,:,1),1)
!#endif
!     !
!   endif
   !
   ! And now add the external field
   !
   if(l_rt_efield.and.RT_time>0._SP) then
     !      
     call RT_Ext_Field(ik,Sigma_s,RT_time-T_initial)
     !
   endif
   !
!   H_t(:,:,ik)=Sigma_s(:,:,1)
   !
!   if(RT_phase_int) H_t(:,:,ik)=H_t(:,:,ik)+Ho(:,:,ik,1)+Hartree_plus_Vxc_0(:,:,ik)
   !
   ! Now prepare I1 and I2 before the integration notice that 
   ! they contain also the external field
   !
#if defined _DOUBLE
   call zgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),Sigma_s(:,:,1),&
&             SC_bands,G_input(:,:,ik),SC_bands,(0._SP,0._SP),I1_lesser(:,:,ik),SC_bands)
   call zgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),G_input(:,:,ik),&
&             SC_bands,Sigma_s(:,:,1),SC_bands,(0._SP,0._SP),I2_lesser(:,:,ik),SC_bands)
#else
   call cgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),Sigma_s(:,:,1),&
&             SC_bands,G_input(:,:,ik),SC_bands,(0._SP,0._SP),I1_lesser(:,:,ik),SC_bands)
   call cgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),G_input(:,:,ik),&
&             SC_bands,Sigma_s(:,:,1),SC_bands,(0._SP,0._SP),I2_lesser(:,:,ik),SC_bands)
#endif
   !
   !
   ! Fictitious lifetime equivalent to the broadening
   ! in linear optics
   !
   if(TransRelax/=0._SP) then
     !   
     ! This term kill the induced polarization, namley off-diagonal term in the
     ! Green function
     !
#if defined _DOUBLE
     call zaxpy(SC_bands2,dcmplx(0.,-1./TransRelax),G_input(:,:,ik),1,I1_lesser(:,:,ik),1)
#else
     call caxpy(SC_bands2, cmplx(0.,-1./TransRelax),G_input(:,:,ik),1,I1_lesser(:,:,ik),1)
#endif
     !
     ! This term send the electron distribution into the initial one en%f()
     !   
     forall(i1=1:SC_bands)
       I1_lesser(i1,i1,ik)=I1_lesser(i1,i1,ik)-(1.,0.)*en%f(i1,ik,1)/TransRelax
     end forall 
     !
     ! Use a different relaxation time for the diagonal part
     !
     if(LongRelax/=TransRelax.and.LongRelax/=0.) then
       !      
       RelaxDiff=1./LongRelax-1./TransRelax
       !
       forall(i1=1:SC_bands)
         I1_lesser(i1,i1,ik)=I1_lesser(i1,i1,ik)-(0.,1.)*(G_input(i1,i1,ik)-(0.,1.)*en%f(i1,ik,1))*RelaxDiff
       end forall
       !
     endif
     !
   endif
   !
 enddo  !---- loop on k-points
 !
end subroutine RT_Collision

