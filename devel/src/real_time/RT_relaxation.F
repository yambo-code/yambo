!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
subroutine RT_relaxation(G_input,E,k,q,Time,l_flag)
 !
 ! This subroutine calculate relaxation term coming from off-diagonal part
 ! of the self-energy in time, from the electron-phonon and/or electron-electron 
 ! coupling or simple radiative and dephasing term in the kinetic equation 
 !
 ! Output: Sigma_relax
 !
 ! Complicate scheme of INTERPOLATION + dF use
 !
 ! i:  1 1      2  2        3 3 4 4 5 5 6 6 7 7 ...
 ! T:  T T+dT/2 T' T'+dT/2  ....
 ! 
 ! After Life_MEM_steps steps... I(interpolation) E(valuation) dF
 ! 
 ! I     X      X  X          X X X  
 ! E  X                     X       X
 ! dF                       X       X
 !
 use pars,           ONLY:SP,cI,cZERO
 use electrons,      ONLY:levels,spin_occ
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt
 use R_lattice,      ONLY:bz_samp,nkibz
 use SC,             ONLY:SC_bands,it_now,RT_nk,RT_all2ibz
 use parallel_m,     ONLY:PAR_IND_Xk_ibz
 use real_time,      ONLY:Sigma_relax,G_lesser_reference,RT_is_dephased,l_NE_dynamics_started,RT_Deph_Matrix,&
&                         E_P_Electron_lifetime,E_P_Hole_lifetime,E_P_El_abs_lifetime,E_P_Ho_abs_lifetime,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,l_RT_CCA_Kernel,Life_MEM_steps,&
&                         RT_TIME_status,RT_E_occupations_reference,RT_H_occupations_reference,&
&                         REF_lifetime,it_start,RT_E_occupations,RT_H_occupations,FIRST_step,&
&                         l_equilibrium_lifetimes,l_RT_skip_impose
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k,q
 logical,       intent(in)    :: l_flag
 real(SP),      intent(in)    :: Time
 complex(SP),   intent(inout) :: G_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 !
 ! Work Space
 !
 integer          :: ib1,ib2,ik
 real(SP)         :: f_E_k,f_H_k,GAMMA_h,GAMMA_e
 real(SP),external:: RT_Lifetimes_equilibrium
 complex(SP)      :: G_input_full(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 logical          :: l_eval
 !
 l_equilibrium_lifetimes=l_flag
 !
 if (.not.l_NE_dynamics_started .and. .not.l_equilibrium_lifetimes) return
 !
 G_input_full=G_input+G_lesser_reference
 !
 ! Dephasing matrix
 !==================
 if (RT_is_dephased.and. .not.l_equilibrium_lifetimes) then
   do ik=1,RT_nk
     do ib1=SC_bands(1),SC_bands(2)
       do ib2=SC_bands(1),SC_bands(2)
         Sigma_relax(ib1,ib2,ik)=-cI*RT_Deph_Matrix(ib1,ib2,RT_all2ibz(ik))*G_input(ib1,ib2,ik)
       enddo
     enddo
   enddo
 else
   Sigma_relax=cZERO
 endif
 !
 if (.not.l_elph_scatt.and..not.l_elel_scatt) return
 !
 l_eval=l_equilibrium_lifetimes
 if (.not.l_equilibrium_lifetimes) &
&  l_eval= ( RT_TIME_status(it_now)>0 .and. (FIRST_step(it_now,Time) .or. it_now-it_start+1<=Life_MEM_steps ) ) &
&          .or. Life_MEM_steps==1
 !
 ! Lifetimes History and updates
 !================================
 call RT_Lifetimes_use_delta_Occupations(l_eval)
 !
 ! El-El
 !=======
 if (l_elel_scatt.and.l_eval) then
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
 if (l_elph_scatt.and.l_eval) then
   !
#if defined _TIMING
   call timing('RT relaxation (e-p)',OPR='start')
#endif
   !
   do ik=1,nkibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     call RT_ELPH_total_Kernel(ik,E,k,q,Time,G_input_full) 
     !
   enddo
   !
#if defined _TIMING
   call timing('RT relaxation (e-p)',OPR='stop')
#endif
   !
 endif
 !
 ! PARALLEL Mergering & N/dN conservation (of the evaluated Lifetimes)
 !=====================================================================
 if (l_eval) then
   if (l_elel_scatt) then
     call RT_Lifetimes_merge(E_E_Hole_lifetime,1)
     call RT_Lifetimes_merge(E_E_Electron_lifetime,2)
   endif
   if (l_elph_scatt) then
     call RT_Lifetimes_merge(E_P_Hole_lifetime,3)
     call RT_Lifetimes_merge(E_P_Electron_lifetime,4)
     call RT_Lifetimes_merge(E_P_Ho_abs_lifetime,5)
     call RT_Lifetimes_merge(E_P_El_abs_lifetime,6)
   endif
   if (.not.l_RT_skip_impose) call Impose_Conservations( )
 endif
 !
 if (l_equilibrium_lifetimes) return
 !
 ! SAVE, interpolate & N/dN conservation (of the interpolated Lifetimes)
 !======================================================================
 if (l_elel_scatt) then
   call RT_Lifetimes_save_and_interp(Time,l_eval,E,k,E_E_Hole_lifetime,1)
   call RT_Lifetimes_save_and_interp(Time,l_eval,E,k,E_E_Electron_lifetime,2)
 endif
 if (l_elph_scatt) then
   call RT_Lifetimes_save_and_interp(Time,l_eval,E,k,E_P_Hole_lifetime,3)
   call RT_Lifetimes_save_and_interp(Time,l_eval,E,k,E_P_Electron_lifetime,4)
   call RT_Lifetimes_save_and_interp(Time,l_eval,E,k,E_P_Ho_abs_lifetime,5)
   call RT_Lifetimes_save_and_interp(Time,l_eval,E,k,E_P_El_abs_lifetime,6)
 endif
 if (.not.l_eval.and..not.l_RT_skip_impose) call Impose_Conservations( )
 !
 ! Dephasing Matrix & Occupations reference
 !==========================================
 do ik=1,RT_nk
   do ib1=SC_bands(1),SC_bands(2)
     !
     f_E_k=RT_E_occupations(ib1,ik)/spin_occ
     f_H_k=RT_H_occupations(ib1,ik)/spin_occ
     !
     GAMMA_e=0._SP
     GAMMA_h=0._SP
     if (l_elel_scatt) then
       GAMMA_h=E_E_Hole_lifetime(ib1,ik)
       GAMMA_e=E_E_Electron_lifetime(ib1,ik)
     endif
     if (l_RT_CCA_Kernel.and.l_elph_scatt) then
       GAMMA_h=GAMMA_h+E_P_Hole_lifetime(ib1,ik)+E_P_Ho_abs_lifetime(ib1,ik)
       GAMMA_e=GAMMA_e+E_P_Electron_lifetime(ib1,ik)+E_P_El_abs_lifetime(ib1,ik)
     endif
     !
     Sigma_relax(ib1,ib1,ik)=Sigma_relax(ib1,ib1,ik) -GAMMA_h*f_H_k +GAMMA_e*f_E_k
     !
   enddo
 enddo
 !
 contains
   !
   subroutine Impose_Conservations( )
     !
     ! Zero dN/dT & dE/dT & d(DeltaN)/dT
     !===================================
     !
     if (l_elel_scatt) then
       call RT_impose_zero_dE_and_dN(E,k,E_E_Hole_lifetime,E_E_Electron_lifetime,'elel ') 
     endif
     if (l_elph_scatt) then
       call RT_impose_zero_dE_and_dN(E,k,E_P_Hole_lifetime,E_P_Electron_lifetime,'elph1')
       call RT_impose_zero_dE_and_dN(E,k,E_P_Ho_abs_lifetime,E_P_El_abs_lifetime,'elph2')
     endif
     !
   end subroutine
   !
end subroutine RT_relaxation

