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
subroutine RT_Collision(G_lesser,en,k,RT_time)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels,el_density
 use drivers,        ONLY:l_sc_exx,l_sc_chosex,l_sc_hartree,l_sc_pz,l_sc_lda_x,&
&                         l_sc_hf,l_sc_exxc
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,V_hartree_sc
 use QP_m,           ONLY:QP_nk
 use real_time,      ONLY:I1_lesser,I2_lesser,V_xc_0,l_rt_exc,rho,V_hartree_0,        &
 &                         Thermal_eta,Thermal_Time,l_rt_efield,TransRelax,LongRelax, &
 &                         T_initial,RT_no_phase_int,E_bare
 use wave_func,      ONLY:wf
 !
 implicit none
 !
 real(SP),      intent(in)   :: RT_time
 type(levels),  intent(in)   :: en
 type(bz_samp), intent(in)   :: k
 complex(SP),   intent(in)   :: G_lesser(SC_bands,SC_bands,en%nk)
 !
 ! Work Space
 !
 integer     :: ik,iv,i1,i2
 complex(SP) :: Sigma_s(SC_bands,SC_bands)
 !
 ! Update occupations
 !
 forall(iv=1:SC_bands,ik=1:en%nk) 
   en%f(iv,ik,1)=-(0._SP,1._SP)*G_lesser(iv,iv,ik)
 end forall
 !
 if(l_rt_exc) then
   !
   call el_density(en,k,rho,G_lesser=G_lesser)
   !
   call V_Hartree(rho,V_hartree_sc)
   !
 endif
 !
 do ik=1,QP_nk 
   !      
   Sigma_s=(0._SP,0._SP)
   !
   if(l_rt_efield.and.RT_time>0._SP) then
     !      
     call Ext_Field(ik,Sigma_s,RT_time-T_initial)
     !
   endif
   !
   if(l_rt_exc) then
     !
     call V_to_H(ik,V_hartree_sc-V_hartree_0-V_xc_0(:,1),Sigma_s,wf)
     !
   endif
   !
   ! Adiabatic Switching
   !
   if(Thermal_eta/=0.0.and.RT_time<0._SP) then
     !      
     Sigma_s=Sigma_s*(1._SP-exp(-(RT_time+Thermal_time)/Thermal_eta))
     !
   endif
   !
   ! Add H(rho_o) in the right part of the equation if it is not
   ! cancelled by the phase factors
   !
   if(RT_no_phase_int) then
     !
     forall(i1=1:SC_bands)
       Sigma_s(i1,i1)=Sigma_s(i1,i1)+E_bare(i1,ik)
     end forall
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
         Sigma_s(i1,i2)=Sigma_s(i1,i2)-G_lesser(i1,i2,ik)/TransRelax
         Sigma_s(i2,i1)=Sigma_s(i2,i1)-G_lesser(i2,i1,ik)/TransRelax
       enddo
     enddo
     !
   endif
   !
   if(LongRelax/=0._SP) then
     !   
     forall(i1=1:SC_bands)
       Sigma_s(i1,i1)=Sigma_s(i1,i1)-G_lesser(i1,i1,ik)/LongRelax
     end forall
     !
   endif
   !
   ! Now prepare I1 and I2 before the integration notice that 
   ! they contain also the external field
   !
#if defined _DOUBLE
   call zgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),Sigma_s,&
&             SC_bands,G_lesser(:,:,ik),SC_bands,(0._SP,0._SP),I1_lesser(:,:,ik),SC_bands)
   call zgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),G_lesser(:,:,ik),&
&             SC_bands,Sigma_s,SC_bands,(0._SP,0._SP),I2_lesser(:,:,ik),SC_bands)
#else
   call cgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),Sigma_s,&
&             SC_bands,G_lesser(:,:,ik),SC_bands,(0._SP,0._SP),I1_lesser(:,:,ik),SC_bands)
   call cgemm('n','n',SC_bands,SC_bands,SC_bands,(1._SP,0._SP),G_lesser(:,:,ik),&
&             SC_bands,Sigma_s,SC_bands,(0._SP,0._SP),I2_lesser(:,:,ik),SC_bands)
#endif
   !
 enddo
 !
end subroutine RT_Collision

