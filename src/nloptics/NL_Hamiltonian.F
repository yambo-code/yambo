!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA
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
subroutine NL_Hamiltonian(E,k,q,X,Time,i_time,V_bands)
 !
 ! The subroutine calculate Ho + V + Sigma and the fields E_tot, E_ext, E_ks
 !
 !   H_k=Ho_k+U_k+ V_k^H-V_k^H[rho_ref]+V_xc[rho]
 !                                        
 use pars,           ONLY:SP,cZERO,pi,cONE
 use units,          ONLY:SPEED_OF_LIGHT
 use electrons,      ONLY:levels,n_sp_pol,n_spinor
 use R_lattice,      ONLY:bz_samp
 use zeros,          ONLY:zero_dfl
 use drivers,        ONLY:l_use_Hxc_collisions
 use hamiltonian,    ONLY:V_hartree_SC,V_xc_SC,rho_n,H_nl_sc
 use QP_m,           ONLY:QP_nk
 use X_m,            ONLY:X_t
 use fields,         ONLY:A_ext,A_tot,Efield
 use real_time,      ONLY:eval_DFT,eval_HARTREE,l_RT_induced_field,rho_reference,&
&                         Ho_plus_Sigma,RT_P,Phase_LifeTime,RT_Vnl_xc,RT_nbands         
 use wave_func,      ONLY:WF
 use xc_functionals, ONLY:V_xc,XC_potential_driver
 use nl_optics,      ONLY:Correlation,V_xc_0,full_rho,IPA,E_full,LRC,JGM,LRCW,l_use_DIPOLES,NL_LRC_alpha,   &
&                         NL_initial_P,E_tot,E_ext,E_ks,E_xc_0,I_relax,Alpha_ED,dG,eval_dG,NL_bands
 use global_XC,      ONLY:WF_kind,WF_xc_functional 
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX
 use wrapper_omp,    ONLY:V_by_V_plus_V_omp
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k,q
 real(SP),      intent(in)    :: Time
 type(X_t),     intent(in)    :: X
 complex(SP),   intent(in)    :: V_bands(NL_bands(2),E%nbf,QP_nk,n_sp_pol)
 integer,       intent(in)    :: i_time
 !
 ! Working Space
 !
 integer     :: ik,i_sp_pol,is,i1
 complex(SP) :: E_vec_pot(3),E_xc_pot(3)
 real :: alpha_
 !
#if defined _TIMING
 call timing('NL Hamiltonian',OPR='start')
#endif
 !
 if(.not.l_use_Hxc_collisions) then
   !
   if(eval_HARTREE .or.  eval_DFT) call el_density_vbands(E,k,rho_n,V_bands,NL_bands(1))
   !
   if(eval_HARTREE) call V_Hartree(rho_n-rho_reference,V_hartree_sc)
   !
   ! I reconstruct the full density if necessary because both TDDFT 
   ! and JGM are not linear in rho_n and require the full density
   ! 
   if(eval_DFT) then
     !
     ! Reconstruct the full density
     !
     if(NL_bands(1)>1) rho_n=full_rho+rho_n-rho_reference
     !
     call XC_potential_driver(E,k,WF_KIND,WF_xc_functional,1)
     V_xc_sc=V_xc-V_xc_0
     !
   endif
   !
 endif
 !
 ! Evaluate the Polarization 
 ! ===================================================================
 !
 if(l_use_DIPOLES) then
   call DIP_polarization(E,k,V_bands,RT_P)
 else
   ! This subroutine update also the Sm1_plus,Sm1_minus and All_S_det matrices
#if defined _TIMING
 call timing('NL Hamiltonian',OPR='stop')
#endif
   call Berry_Polarization_NEQ(E,k,V_bands,RT_P)
#if defined _TIMING
 call timing('NL Hamiltonian',OPR='start')
#endif
   !
 endif
 !
 RT_P=RT_P-NL_initial_P ! Subtract the initial polarization
 !
 ! Evaluate the external/total field
 !=================================
 !
 call RT_Ext_fields(A_ext,Time)
 !
 A_tot=A_ext
 !
 E_ext=-A_ext%vecpot_vel/SPEED_OF_LIGHT
 !
 E_tot=E_ext
 E_ks =E_ext
 !
 if( (Correlation==LRC.or.Correlation==JGM.or.l_RT_induced_Field.or.Correlation==LRCW) .and. .not.l_use_Hxc_collisions) then
    !
    ! Evaluate the induced field and/or the LRC correction
    !
    E_vec_pot=-A_ext%vecpot_vel/SPEED_OF_LIGHT
    !
    if(l_RT_induced_Field) E_vec_pot=E_vec_pot - 4.0*pi*RT_P
    !
    E_tot=E_vec_pot
    !
    if(Correlation==LRCW) then !This can be calculated once for each frequency, but I did not want to mess up NL_driver (MG)
      alpha_ = NL_LRC_alpha 
      if (Efield(1)%ef_name=='SOFTSIN'.or.Efield(1)%ef_name=='SIN') &
      &     alpha_  = Alpha_ED(E,Efield(1)%frequency(1))
           E_vec_pot=E_vec_pot + alpha_*RT_P
    endif
    !
    if(Correlation==LRC)   E_vec_pot=E_vec_pot + NL_LRC_alpha*RT_P
    if(Correlation==JGM) then
      call EXC_macroscopic_JGM(E,RT_P,rho_n,E_xc_pot,V_xc_sc)
      E_vec_pot=E_vec_pot +  E_xc_pot - E_xc_0
    end if
    !
    E_ks=E_vec_pot
    !
    A_tot%vecpot_vel=-E_vec_pot*SPEED_OF_LIGHT
    !
 endif
 !
 ! Evaluate the G_lesser if using TD-SEX or TD-HF and than add COLLISIONS
 ! ======================================================
 if(eval_dG)              call NL_build_dG_lesser(E,V_bands,dG)
 if(l_use_Hxc_collisions) call COLLISIONS_compose_nl(dG)
 !
 ! Build the Hamiltonian
 ! =====================
 !
 Ho_plus_Sigma=cZERO
 I_relax      =cZERO
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,QP_nk
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     ! Here I fill H_nl_sc with...
     !
     H_nl_sc=cZERO
     !
     ! Correlation part Hartree + TDDFT
     ! ================================
     !
     if(Correlation/=IPA .and. .not.l_use_Hxc_collisions) then
       !  
       if(n_spinor==1) then
          if (eval_Hartree.and.eval_DFT) then
            call V_real_space_to_H(ik,i_sp_pol,V_hartree_sc+V_xc_sc,H_nl_sc,WF,'def')
          elseif(eval_Hartree.and..not.eval_DFT) then
            call V_real_space_to_H(ik,i_sp_pol,V_hartree_sc,H_nl_sc,WF,'def')
          endif
       elseif (n_spinor==2) then
          if (eval_Hartree) call V_real_space_to_H(ik,i_sp_pol,V_hartree_sc,H_nl_sc,WF,'def')
          if (eval_DFT) call V_real_space_to_H(ik,i_sp_pol,V_xc_sc,H_nl_sc,WF,'xc ')
       endif
       !
     endif
     !
     ! Add Ho = T+V_ion+V_h+V_xc
     !==================
     !
     forall(i1=NL_bands(1):NL_bands(2)) H_nl_sc(i1,i1)=H_nl_sc(i1,i1)+E_full(i1,ik,i_sp_pol) 
     !
     if(l_use_DIPOLES) then
       !    
       call RT_apply_field(ik,i_sp_pol,H_nl_sc,A_tot)
       !
     else
       !
       ! Use Sm1_plus, Sm1_minus and V_bands to build the W operator
       !
       call  Build_W_operator(V_bands,E,k,A_tot,ik,i_sp_pol,H_nl_sc)
       !
     endif                                                         
     !
     call  Build_W_operator(V_bands,E,k,A_tot,ik,i_sp_pol,H_nl_sc)
     !
     ! GPL_EXCLUDE_START
     !
     if(l_use_Hxc_collisions) then
       call V_by_V_plus_V_omp(RT_nbands,cONE,RT_Vnl_xc(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),ik,i_sp_pol),&
&                                              H_nl_sc(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2)))
  
     endif
     !
     ! GPL_EXCLUDE_END
     !
     ! Damping: NL_damping fills the I_relax matrix 
     ! ================================================
     call NL_damping(E,V_bands,ik,i_sp_pol)
     !
     ! Store the Hamiltonian
     ! =====================
     Ho_plus_Sigma(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),ik,i_sp_pol)= &
&          H_nl_sc(NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2))
     !
   enddo  !---- loop on k-point
   !
 enddo
 !
 call PP_redux_wait(Ho_plus_Sigma,COMM=PAR_COM_Xk_ibz_INDEX%COMM ) 
 !
 if(Phase_LifeTime<zero_dfl) call PP_redux_wait(I_relax,COMM=PAR_COM_Xk_ibz_INDEX%COMM ) 
 !
#if defined _TIMING
 call timing('NL Hamiltonian',OPR='stop')
#endif
 !
end subroutine NL_Hamiltonian

