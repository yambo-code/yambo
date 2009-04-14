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
!
! This subroutine prepare I1(t_1,t_2) and I2(t_1,t_2) give a G^<(t_1,t_1) 
! Actualy it works only for the diagonal I1(t_1,t_1), I2(t_1,t_1)
!
!!!!!!
!WARNING: The variables Sigma_s  have been generalized for spin
!polarized calculation due to changes in V_to_H subroutine. 
!Here the spin index is set to 1 ...(Daniele e Maurizia).
!!!!!!

subroutine RT_Collision(G_input,en,k,RT_time)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels,el_density,n_sp_pol
 use drivers,        ONLY:l_sc_exx,l_sc_chosex,l_sc_hartree,l_sc_pz,l_sc_lda_x,&
&                         l_sc_hf,l_sc_exxc
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,V_hartree_sc
 use QP_m,           ONLY:QP_nk,QP_Vxc,QP_n_states,QP_table 
 use real_time,      ONLY:I1_lesser,I2_lesser,V_xc_0,l_rt_exc,rho,V_hartree_0,        &
 &                         Thermal_Time,l_rt_efield,TransRelax,LongRelax, &
 &                         T_initial,RT_phase_int,E_bare,H_t,Ho
 use wave_func,      ONLY:wf
 use xc_functionals, ONLY:V_xc
 use global_XC,      ONLY:WF_kind,WF_xc_functional 
 !
 implicit none
 !
 real(SP),      intent(in)   :: RT_time
 type(levels),  intent(in)   :: en
 type(bz_samp), intent(in)   :: k
 complex(SP),   intent(in)   :: G_input(SC_bands,SC_bands,en%nk)
 !
 ! Work Space
 !
 integer     :: ik,iv,i1,i2,i3,ik1,ib,ibp
 complex(SP) :: Sigma_s(SC_bands,SC_bands,n_sp_pol)
 !
 ! Update occupations
 !
 forall(iv=1:SC_bands,ik=1:en%nk) 
   en%f(iv,ik,1)=-(0._SP,2._SP)*G_input(iv,iv,ik)
 end forall
 !
 call el_density(en,k,rho,G_lesser=G_input)
 !
 if(l_rt_exc) then
   !
   call V_Hartree(rho,V_hartree_sc)
   !
   if(l_sc_pz) then
     !
     call xc_lda_driver(en,k,WF_KIND,WF_xc_functional,1)
     !
   endif
   !
 else
   !
   ! Simple case without anycorrelation just transition between bands
   ! equivalent to RPA without local field effects
   !
   V_hartree_sc(:,1)=V_hartree_0
   V_xc        =V_xc_0
   !
 endif
 !
 do ik=1,QP_nk 
   !      
   Sigma_s=(0._SP,0._SP)
   !
   !  I subtract the V_xc_0 term if there is a self-energy /=0 (at least V_h)
   !  and not TD-LDA
   !
   if(l_rt_exc.and..not.l_sc_pz) then 
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
   if(.not.RT_phase_int) then
     !
     ! If I do not integrate analyticaly the H_o(rho_o) part
     ! I have to add the rest of the Hamiltonian Ho + V_H + V_xc 
     !
     call V_to_H(ik,V_hartree_sc+V_xc,Sigma_s,wf)
     !
     Sigma_s(:,:,1)=Sigma_s(:,:,1)+Ho(:,:,ik)
     !
   else
     !
     ! I use H_o + V_h(rho_o) + V_xc(rho_o) as phase factor 
     ! so I put only the differences in the self-energy
     ! 
     if(l_rt_exc) then
       !      
       call V_to_H(ik,V_hartree_sc(:,1)-V_hartree_0+V_xc(:,1)-V_xc_0(:,1),Sigma_s,wf)
       !
     endif
     !
   endif
   !
   ! And now add the external field
   !
   if(l_rt_efield.and.RT_time>0._SP) then
     !      
     call Ext_Field(ik,Sigma_s,RT_time-T_initial)
     !
   endif
   !
   ! Fictitious lifetime equivalent to the broadening
   ! in linear optics
   !
   if(TransRelax/=0._SP) then
     !   
     do i1=1,SC_bands
       do i2=i1+1,SC_bands
         Sigma_s(i1,i2,1)=Sigma_s(i1,i2,1)-G_input(i1,i2,ik)/TransRelax
         Sigma_s(i2,i1,1)=Sigma_s(i2,i1,1)-G_input(i2,i1,ik)/TransRelax
       enddo
     enddo
     !
   endif
   !
   if(LongRelax/=0._SP) then
     !   
     forall(i1=1:SC_bands)
       Sigma_s(i1,i1,1)=Sigma_s(i1,i1,1)-G_input(i1,i1,ik)/LongRelax
     end forall
     !
   endif
   !
   H_t(:,:,ik)=Sigma_s(:,:,1)
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
 enddo  !---- loop on k-points
 !
end subroutine RT_Collision

