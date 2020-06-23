!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_relaxation(E,k,q,Time)
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
 ! 2) This subroutine calculates the lifetimes needed to build S^xc
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
 use pars,           ONLY:SP
 use D_lattice,      ONLY:Bose_Temp
 use electrons,      ONLY:levels
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elphoton_scatt
 use R_lattice,      ONLY:bz_samp,nkibz
#if defined _PHEL
 use drivers,        ONLY:l_phel_scatt
 use R_lattice,      ONLY:nqibz
 !DEBUG>
 use units,        ONLY:HA2EV
 use PH_real_time, ONLY:EQ_P_E_sp_res_lifetime,EQ_P_E_sp_ares_lifetime,EQ_P_E_eh_lifetime
 !DEBUG<
#endif
 use parallel_m,     ONLY:PAR_IND_Xk_ibz
 use real_time,      ONLY:RT_is_statically_dephased,RT_Deph_Matrix,RT_bands,&
&                         E_P_Electron_lifetime,E_P_Hole_lifetime,E_P_El_abs_lifetime,E_P_Ho_abs_lifetime,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,l_RT_CCA_Kernel,Life_MEM_steps,&
&                         REF_lifetime,FIRST_step,l_initial_equilibrium,RT_is_statically_dephased,&
&                         RT_nk,RT_all2ibz,EE_prefactor,EP_prefactor,EP_abs_prefactor,&
&                         E_PHOT_Hole_lifetime,E_PHOT_Electron_lifetime,SAVED_times,ADD_to_saved_values,&
&                         RT_is_dynamically_dephased,l_RT_relax_time_approx,l_RT_eval_lifetimes
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(levels),  intent(inout) :: E
 type(bz_samp), intent(in)    :: k,q
 real(SP),      intent(in)    :: Time
 !
 ! Work Space
 !
 integer          :: ik,iq
 !
 if ( (.not.RT_is_statically_dephased.and..not.l_elphoton_scatt) .or. &
&     (.not.RT_is_dynamically_dephased)                          .or. &
&     (.not.l_initial_equilibrium.and.l_RT_relax_time_approx)         ) return
 !
 call RT_relaxation_logicals( )
 !
 ! Lifetimes History and updates
 !================================
 if (l_RT_eval_lifetimes) call RT_Lifetimes_use_delta_Occupations( )
 !
 ! El-El
 !=======
 if(l_elel_scatt.and.l_RT_eval_lifetimes ) then
   !
   call timing('RT relaxation (e-e)',OPR='start')
   !
   do ik=1,nkibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     call RT_Coulomb_relaxation(ik,E,k,q)
     !
   enddo
   !
   call timing('RT relaxation (e-e)',OPR='stop')
   !
 endif
 !
 ! El-Ph
 !=======
 if(l_elph_scatt.and.l_RT_eval_lifetimes) then
   !
   call timing('RT relaxation (e-p)',OPR='start')
   !
   do ik=1,nkibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     call RT_ELPH_total_Kernel(ik,E,k,q,Time)
     !
   enddo
   !
   call timing('RT relaxation (e-p)',OPR='stop')
   !
 endif
#if defined _PHEL
 !
 ! Ph-El (phonon dynamics)
 !=======
 if(l_phel_scatt.and.l_elph_scatt.and.l_RT_eval_lifetimes) then
   !
   call timing('RT relaxation (p-e)',OPR='start')
   !
   do iq=1,nqibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(iq)) cycle
     !
     call RT_PHEL_lifetimes(iq,E,k,q)
     !
   enddo
   !
   !DEBUG>
   write(*,*) HA2EV*(EQ_P_E_sp_res_lifetime(2,6)+EQ_P_E_sp_ares_lifetime(2,6))
   write(*,*) HA2EV*EQ_P_E_eh_lifetime(2,6)
   !DEBUG<
   !
   call timing('RT relaxation (p-e)',OPR='stop')
   !
 endif
#endif
 !
 ! El-Photon
 !============
 if(l_elphoton_scatt.and.l_RT_eval_lifetimes) then
   !
   call timing('RT relaxation (e-photon)',OPR='start')
   !
   do ik=1,nkibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     call RT_Radiative_Lifetimes(ik,E,k,q)
     !
   enddo
   !
   call timing('RT relaxation (e-photon)',OPR='stop')
   !
 endif
 !
 if (l_RT_eval_lifetimes) then
   !
   ! PARALLEL Mergering
   !====================
   call RT_Lifetimes_merge_driver("INTRA_k")
   !
 endif
 !
 call timing('RT relaxation (impose and idle)',OPR='start')
 !
 if (l_RT_eval_lifetimes) then
   !
   ! Conservation Rules
   !====================
   call Impose_Conservations( )
   !
 endif
 !
 if (l_initial_equilibrium) then
   !
   ! PARALLEL Mergering
   !====================
   call RT_Lifetimes_merge_driver("REF")
   !
   call timing('RT relaxation (impose and idle)',OPR='stop')
   !
   return
   !
 endif
 !
 ! SAVE, extrapolate & N/dN conservation (of the extrapolated Lifetimes)
 !======================================================================
 !
 ! Time points saved for later extrapolation
 !
 if (l_RT_eval_lifetimes) call ADD_to_saved_values(Time,SAVED_times,Life_MEM_steps)
 !
 if (l_elel_scatt) then
   call RT_Lifetimes_save_and_extrap(Time,l_RT_eval_lifetimes,E,k,E_E_Hole_lifetime,1)
   call RT_Lifetimes_save_and_extrap(Time,l_RT_eval_lifetimes,E,k,E_E_Electron_lifetime,2)
 endif
 if (l_elph_scatt) then
   call RT_Lifetimes_save_and_extrap(Time,l_RT_eval_lifetimes,E,k,E_P_Hole_lifetime,3)
   call RT_Lifetimes_save_and_extrap(Time,l_RT_eval_lifetimes,E,k,E_P_Electron_lifetime,4)
   if(Bose_Temp>0._SP) then
     call RT_Lifetimes_save_and_extrap(Time,l_RT_eval_lifetimes,E,k,E_P_Ho_abs_lifetime,5)
     call RT_Lifetimes_save_and_extrap(Time,l_RT_eval_lifetimes,E,k,E_P_El_abs_lifetime,6)
   endif
 endif
 if (l_elphoton_scatt) then
   call RT_Lifetimes_save_and_extrap(Time,l_RT_eval_lifetimes,E,k,E_PHOT_Hole_lifetime,7)
   call RT_Lifetimes_save_and_extrap(Time,l_RT_eval_lifetimes,E,k,E_PHOT_Electron_lifetime,8)
 endif
 !
 if (.not.l_RT_eval_lifetimes) call Impose_Conservations( )
 !
 call timing('RT relaxation (impose and idle)',OPR='stop')
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
     if (l_elel_scatt)     call RT_impose_zero_dE_and_dN(E,k,E_E_Hole_lifetime,E_E_Electron_lifetime,'elel ')
     if (l_elph_scatt)     call RT_impose_zero_dE_and_dN(E,k,E_P_Hole_lifetime,E_P_Electron_lifetime,'elph1')
     if (l_elph_scatt.and.Bose_Temp>0._SP) &
&                          call RT_impose_zero_dE_and_dN(E,k,E_P_Ho_abs_lifetime,E_P_El_abs_lifetime,'elph2')
     if (l_elphoton_scatt) call RT_impose_zero_dE_and_dN(E,k,E_PHOT_Hole_lifetime,E_PHOT_Electron_lifetime,'radiative')
     !
   end subroutine
   !
end subroutine RT_relaxation
