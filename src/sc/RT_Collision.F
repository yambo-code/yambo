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
 use electrons,      ONLY:levels,el_density,n_sp_pol
 use drivers,        ONLY:l_sc_pz
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands,V_hartree_sc,Ho,H_nl_sc,V_xc_sc,rho_n
 use QP_m,           ONLY:QP_nk
 use real_time,      ONLY:I1_lesser,I2_lesser,l_RT_no_updates,l_qp_analyze, &
 &                        l_rt_efield,TransRelax,LongRelax,T_initial, &
 &                        Hartree_plus_Vxc_sc,l_RPA,V_hartree_0,V_xc_0,Thermal_time, &
 &                        Integrator_name,EXACT,RK2EXACT,H_t
 use wave_func,      ONLY:wf
 use xc_functionals, ONLY:V_xc
 use global_XC,      ONLY:WF_kind,WF_xc_functional 
 !
 implicit none
 !
 real(SP),      intent(in)   :: RT_time
 type(levels),  intent(in)   :: Xen
 type(bz_samp), intent(in)   :: k,q
 complex(SP),   intent(in)   :: G_input(SC_bands,SC_bands,Xen%nk)
 !
 ! Work Space
 !
 integer     :: ik,iv,i1,SC_bands2
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
 call el_density(Xen,k,rho_n,G_lesser=G_input)
 !
 if(.not.l_RT_no_updates) then
   !
   call V_Hartree(rho_n,V_hartree_sc)
   V_hartree_sc(:,n_sp_pol)=V_hartree_sc(:,1)
   !
   if(l_sc_pz) then
     !
     call IO_mute("report log io_in io_out")
     call xc_lda_driver(Xen,k,WF_KIND,WF_xc_functional,1,G_input)
     call IO_mute("  ")
     !
     V_xc_sc        =V_xc
     !
   else
     !
     V_xc_sc        =V_xc_0
     !
   endif
   !
 else
   !
   ! Simple case without any correlation just transition between bands
   ! equivalent to RPA without local field effects
   !
   V_hartree_sc     =V_hartree_0
   V_xc_sc          =V_xc_0
   !
 endif
 !
 ! l_RT_no_updates=.true.
 !
 ! Simple case without any correlation just transition between bands
 ! equivalent to RPA without local field effects
 !
 !
 do ik=1,QP_nk 
   !      
   H_nl_sc=(0.,0.)
   !
   !  I subtract the V_xc_sc term if there is a self-energy /=0 (at least V_h)
   !  and not l_RPA=.true. (used to include local field effects only)
   !
   if(.not.l_RT_no_updates.and..not.l_RPA) then 
     !  
     call V_real_space_to_H(ik,-V_xc_0,H_nl_sc,wf)
     !
   endif
   !
   ! Adiabatic Switching
   !
   if(Thermal_time/=0.0) then 
     !      
     H_nl_sc=H_nl_sc*(1._SP+tanh((RT_time+Thermal_time/2._SP)/Thermal_time*8._SP))/2._SP
     !
   endif
   !
   ! Fill the Singular (in time) Self-Energy H_nl_sc(:,:,:)
   !
   if(.not.l_RT_no_updates) then
     call V_real_space_to_H(ik,V_hartree_sc+V_xc_sc,H_nl_sc,wf)
   else
#if defined _DOUBLE
     call zaxpy(SC_bands2,(1.,0.),Hartree_plus_Vxc_sc(:,:,ik,1),1,H_nl_sc(:,:,1),1)
#else
     call caxpy(SC_bands2,(1.,0.),Hartree_plus_Vxc_sc(:,:,ik,1),1,H_nl_sc(:,:,1),1)
#endif
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
   if(l_rt_efield.and.RT_time>0._SP) then
     !      
     call RT_Ext_Field(ik,H_nl_sc,Xen,RT_time-T_initial)
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
       I1_lesser(i1,i1,ik)=I1_lesser(i1,i1,ik)-(1.,0.)*Xen%f(i1,ik,1)/TransRelax
     end forall 
     !
     ! Use a different relaxation time for the diagonal part
     !
     if(LongRelax/=TransRelax.and.LongRelax/=0.) then
       !      
       RelaxDiff=1./LongRelax-1./TransRelax
       !
       forall(i1=1:SC_bands)
         I1_lesser(i1,i1,ik)=I1_lesser(i1,i1,ik)-(0.,1.)*(G_input(i1,i1,ik)-(0.,1.)*Xen%f(i1,ik,1))*RelaxDiff
       end forall
       !
     endif
     !
   endif
   !
 enddo  !---- loop on k-points
 !
end subroutine RT_Collision

