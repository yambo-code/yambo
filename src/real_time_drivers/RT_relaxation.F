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
 use electrons,      ONLY:levels
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elphoton_scatt
 use R_lattice,      ONLY:bz_samp,nkibz
#if defined _PHEL
 use drivers,        ONLY:l_phel_scatt
 use R_lattice,      ONLY:nqibz
#endif
 use parallel_m,     ONLY:PAR_IND_Xk_ibz
 use RT_occupations, ONLY:l_RT_relax_time_approx 
 use RT_lifetimes,   ONLY:RT_EP_emit_life,RT_EE_life,RT_EH_life,l_RT_eval_lifetimes,& 
&                         RT_EP_emit_REF_life,RT_EE_REF_life,RT_EH_REF_life,&
&                         Life_MEM_steps,l_initial_equilibrium,RT_EP_abs_REF_life,&
&                         SAVED_times,RT_EP_abs_life
 use RT_operations,  ONLY:ADD_to_saved_values
 use real_time,      ONLY:RT_is_statically_dephased,RT_Deph_Matrix,RT_bands,&
&                         l_RT_CCA_Kernel,RT_is_statically_dephased,&
&                         RT_nk,RT_is_dynamically_dephased
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
 !DEBUG> 
#if defined _PHEL
 integer, parameter :: out_file=20
#endif
 !DEBUG<
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
 call RT_Lifetimes_save_and_extrap(Time,l_RT_eval_lifetimes,RT_EE_life)
 call RT_Lifetimes_save_and_extrap(Time,l_RT_eval_lifetimes,RT_EP_emit_life)
 call RT_Lifetimes_save_and_extrap(Time,l_RT_eval_lifetimes,RT_EP_abs_life)
 call RT_Lifetimes_save_and_extrap(Time,l_RT_eval_lifetimes,RT_EH_life)
 !
 if (.not.l_RT_eval_lifetimes) call Impose_Conservations( )
 !
 call timing('RT relaxation (impose and idle)',OPR='stop')
 !
 contains
   !
   subroutine Impose_Conservations( )
     !
     ! Zero dN/dT & dE/dT & d(DeltaN)/dT
     !===================================
     call RT_impose_zero_dE_and_dN(E,RT_EP_emit_life,RT_EP_emit_life%emit,RT_EP_emit_life%abs,&
&                                    RT_EP_emit_REF_life%emit, RT_EP_emit_REF_life%abs,&
&                                    RT_EP_emit_life%prefactor)
     call RT_impose_zero_dE_and_dN(E,RT_EP_abs_life,RT_EP_abs_life%abs,RT_EP_abs_life%emit,&
&                                    RT_EP_abs_REF_life%abs,RT_EP_abs_REF_life%emit,&
&                                    RT_EP_abs_life%prefactor)
     call RT_impose_zero_dE_and_dN(E,RT_EE_life,RT_EE_life%emit,RT_EE_life%abs,&
&                                    RT_EE_REF_life%emit,RT_EE_REF_life%abs,&
&                                    RT_EE_life%prefactor)
     call RT_impose_zero_dE_and_dN(E,RT_EH_life,RT_EH_life%emit,RT_EH_life%abs,&
&                                    RT_EH_REF_life%emit,RT_EH_REF_life%abs,&
&                                    RT_EH_life%prefactor)
   end subroutine
   !
end subroutine RT_relaxation
