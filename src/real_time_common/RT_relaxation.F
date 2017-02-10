!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine RT_relaxation(dG_in,E,k,q,Time,l_equilibrium)
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
 ! 2) This subroutine calculates S^xc
 !    S^xc is here divided in two terms
 !    a) (S^xc)_ij with i/=j describes de-phasing and is approximated as C_ij * G_ij(t)
 !       with C_ij a constant term. C_ij is here called RT_Deph_Matrix
 !    b) (S^xc)_ii  describes scattering processes between electrons and holes
 !       It is computing starting from the full functional S^xc[G(t)].
 !       However only the terms G_ii(t) are used in the functional, discarding
 !       G_ij(t) with i\=j
 !
 ! 3) Available approximations for S^xc_ii are
 !    - The FAN Self-energy which describes the electron--phonon scattering
 !    - The NEQ-GW self-energy constructed with a static retarded and advanced screened interaction
 !      which describes electron--electron scattering
 !    -  ... electron-photon scattering
 !
 ! 4) S^xc_ii is not computed at each time step.
 !    Here a scheme of the EXTRAPOLATION scheme used
 !
 !    i:  1 1      2  2        3 3 4 4 5 5 6 6 7 7 ...
 !    T:  T T+dT/2 T' T'+dT/2  ....
 ! 
 !    After Life_MEM_steps steps... I(extrapolation) E(valuation) dF
 ! 
 !    I     X      X  X          X X X  
 !    E  X                     X       X
 !    dF                       X       X
 !
 use pars,           ONLY:SP,cI,cZERO
 use D_lattice,      ONLY:Bose_Temp
 use electrons,      ONLY:levels
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elphoton_scatt
 use R_lattice,      ONLY:bz_samp,nkibz
 use parallel_m,     ONLY:PAR_IND_Xk_ibz
 use RT_control,     ONLY:NE_i_start_time
 use real_time,      ONLY:I_relax_static,RT_is_statically_dephased,RT_Deph_Matrix,RT_bands,&
&                         E_P_Electron_lifetime,E_P_Hole_lifetime,E_P_El_abs_lifetime,E_P_Ho_abs_lifetime,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,l_RT_CCA_Kernel,Life_MEM_steps,&
&                         RT_TIME_status,REF_lifetime,RT_E_occupations,RT_H_occupations,FIRST_step,&
&                         l_initial_equilibrium,RT_delta0_occupations,RT_is_statically_dephased,&
&                         RT_nk,RT_all2ibz,EE_prefactor,EP_prefactor,EP_abs_prefactor,NE_i_time,&
&                         E_PHOT_Hole_lifetime,E_PHOT_Electron_lifetime,SAVED_times,ADD_to_saved_values,&
&                         RT_is_dynamically_dephased
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(levels),  intent(inout) :: E
 type(bz_samp), intent(in)    :: k,q
 logical,       intent(in)    :: l_equilibrium
 real(SP),      intent(in)    :: Time
 complex(SP),   intent(inout) :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 !
 ! Work Space
 !
 integer          :: ib1,ik
 real(SP)         :: f_E_k,f_H_k,df_k,dGAMMA_h,dGAMMA_e
 logical          :: l_eval
 !
 l_initial_equilibrium=l_equilibrium
 !
 if (.not.RT_is_statically_dephased.and..not.l_elphoton_scatt) return
 !
 I_relax_static=cZERO
 !
 ! Dephasing matrix
 !==================
 if (RT_is_statically_dephased.and. .not.l_initial_equilibrium) then
   do ik=1,RT_nk
     do ib1=RT_bands(1),RT_bands(2)
       I_relax_static(ib1,ib1,ik)=-cI*RT_Deph_Matrix(ib1,ib1,RT_all2ibz(ik))*dG_in(ib1,ib1,ik)
     enddo
   enddo
 endif
 !
 ! Eval or not ?
 !===============
 !
 if (.not.RT_is_dynamically_dephased ) return
 !
 l_eval=l_initial_equilibrium 
 !
 if (.not.l_initial_equilibrium) then
   l_eval= ( RT_TIME_status(NE_i_time)>0 .and. &
&            (FIRST_step( ) .or. NE_i_time-NE_i_start_time+1<=Life_MEM_steps ) ).or. &
&          Life_MEM_steps==1
 endif
 !
 ! Lifetimes History and updates
 !================================
 call RT_Lifetimes_use_delta_Occupations(l_eval)
 !
 ! El-El
 !=======
 if(l_elel_scatt.and.l_eval ) then
#if defined _TIMING
   call timing('RT relaxation (e-e)',OPR='start')
#endif
   !
   do ik=1,nkibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     call RT_Coulomb_relaxation(ik,E,k,q)
     !
   enddo
   !
#if defined _TIMING
   call timing('RT relaxation (e-e)',OPR='stop')
#endif
   !
 endif
 !
 ! El-Ph
 !=======
 if(l_elph_scatt.and.l_eval) then
   !
#if defined _TIMING
   call timing('RT relaxation (e-p)',OPR='start')
#endif
   !
   do ik=1,nkibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     call RT_ELPH_total_Kernel(ik,E,k,q,Time) 
     !
   enddo
   !
#if defined _TIMING
   call timing('RT relaxation (e-p)',OPR='stop')
#endif
   !
 endif
 !
 ! El-Photon
 !============
 if(l_elphoton_scatt.and.l_eval) then
   !
#if defined _TIMING
   call timing('RT relaxation (e-photon)',OPR='start')
#endif
   !
   do ik=1,nkibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     call RT_Radiative_Lifetimes(ik,E,k,q)
     !
   enddo
   !
#if defined _TIMING
   call timing('RT relaxation (e-photon)',OPR='stop')
#endif
   !
 endif
 !
 ! PARALLEL Mergering & N/dN conservation (of the evaluated Lifetimes)
 !=====================================================================
 if (l_eval) then
   if (l_elel_scatt) then
     call RT_Lifetimes_merge(E,k,E_E_Hole_lifetime,1)
     call RT_Lifetimes_merge(E,k,E_E_Electron_lifetime,2)
   endif
   if (l_elph_scatt) then
     call RT_Lifetimes_merge(E,k,E_P_Hole_lifetime,3)
     call RT_Lifetimes_merge(E,k,E_P_Electron_lifetime,4)
     if(Bose_Temp>0._SP) then
       call RT_Lifetimes_merge(E,k,E_P_Ho_abs_lifetime,5)
       call RT_Lifetimes_merge(E,k,E_P_El_abs_lifetime,6)
     endif
   endif
   if (l_elphoton_scatt) then
     call RT_Lifetimes_merge(E,k,E_PHOT_Hole_lifetime,7)
     call RT_Lifetimes_merge(E,k,E_PHOT_Electron_lifetime,8)
   endif
 endif
 !
 if (l_eval) call Impose_Conservations( )
 !
 if (l_initial_equilibrium) return
 !
 ! SAVE, extrapolate & N/dN conservation (of the extrapolated Lifetimes)
 !======================================================================
 !
 ! Time points saved for later extrapolation
 !
 if (l_eval) call ADD_to_saved_values(Time,SAVED_times,Life_MEM_steps)
 !
 if (l_elel_scatt) then
   call RT_Lifetimes_save_and_extrap(Time,l_eval,E,k,E_E_Hole_lifetime,1)
   call RT_Lifetimes_save_and_extrap(Time,l_eval,E,k,E_E_Electron_lifetime,2)
 endif
 if (l_elph_scatt) then
   call RT_Lifetimes_save_and_extrap(Time,l_eval,E,k,E_P_Hole_lifetime,3)
   call RT_Lifetimes_save_and_extrap(Time,l_eval,E,k,E_P_Electron_lifetime,4)
   if(Bose_Temp>0._SP) then
     call RT_Lifetimes_save_and_extrap(Time,l_eval,E,k,E_P_Ho_abs_lifetime,5)
     call RT_Lifetimes_save_and_extrap(Time,l_eval,E,k,E_P_El_abs_lifetime,6)
   endif
 endif
 if (l_elphoton_scatt) then
   call RT_Lifetimes_save_and_extrap(Time,l_eval,E,k,E_PHOT_Hole_lifetime,7)
   call RT_Lifetimes_save_and_extrap(Time,l_eval,E,k,E_PHOT_Electron_lifetime,8)
 endif
 !
 if (.not.l_eval) call Impose_Conservations( )
 !
 ! Dephasing Matrix & Occupations reference
 !==========================================
 !
 do ik=1,RT_nk
   do ib1=RT_bands(1),RT_bands(2)
     !
     f_E_k=RT_E_occupations(ib1,ik)
     f_H_k=RT_H_occupations(ib1,ik)
     !
     df_k=RT_delta0_occupations(ib1,ik)
     !
     dGAMMA_e=0._SP
     dGAMMA_h=0._SP
     if (l_elel_scatt) then
       dGAMMA_h=E_E_Hole_lifetime(ib1,ik)
       dGAMMA_e=E_E_Electron_lifetime(ib1,ik)
     endif
     if (l_elphoton_scatt) then
       dGAMMA_h=dGAMMA_h+E_PHOT_Hole_lifetime(ib1,ik)
       dGAMMA_e=dGAMMA_e+E_PHOT_Electron_lifetime(ib1,ik)
     endif
     if (l_RT_CCA_Kernel.and.l_elph_scatt) then
       dGAMMA_h=dGAMMA_h+ E_P_Hole_lifetime(ib1,ik)    +E_P_Ho_abs_lifetime(ib1,ik)
       dGAMMA_e=dGAMMA_e+ E_P_Electron_lifetime(ib1,ik)+E_P_El_abs_lifetime(ib1,ik)
     endif
     !
     ! This term could be moved inside RT_apply_zero_dyn.F
     ! However it's better to keep is together with the rest of the 
     ! relaxation term
     !
     I_relax_static(ib1,ib1,ik)=I_relax_static(ib1,ib1,ik) + sum(REF_lifetime(ib1,ik,:))*df_k + ( dGAMMA_e*f_E_k - dGAMMA_h*f_H_k )
     !
   enddo
 enddo
 !
 contains
   !
   subroutine Impose_Conservations( )
     !
     EE_prefactor    =0._SP
     EP_prefactor    =0._SP
     EP_abs_prefactor=0._SP
     !
     ! Zero dN/dT & dE/dT & d(DeltaN)/dT
     !===================================
     if (l_elel_scatt) then
       call RT_impose_zero_dE_and_dN(E,k,E_E_Hole_lifetime,E_E_Electron_lifetime,'elel ') 
     endif
     if (l_elph_scatt) then
       call RT_impose_zero_dE_and_dN(E,k,E_P_Hole_lifetime,E_P_Electron_lifetime,'elph1')
       if(Bose_Temp>0._SP) then
         call RT_impose_zero_dE_and_dN(E,k,E_P_Ho_abs_lifetime,E_P_El_abs_lifetime,'elph2')
       endif
     endif
     if (l_elphoton_scatt) then
       call RT_impose_zero_dE_and_dN(E,k,E_PHOT_Hole_lifetime,E_PHOT_Electron_lifetime,'radiative') 
     endif
     !
   end subroutine
   !
end subroutine RT_relaxation
