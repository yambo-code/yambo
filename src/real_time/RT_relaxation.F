!
! Copyright (C) 2000-2013 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_relaxation(G_input,E,k,q,Time)
 !
 ! This subroutine calculate relaxation term coming from off-diagonal part
 ! of the self-energy in time, from the electron-phonon and/or electron-electron 
 ! coupling or simple radiative and dephasing term in the kinetic equation 
 !
 ! Output: Sigma_relax
 !
 use pars,           ONLY:SP,cI,cZERO
 use electrons,      ONLY:levels,spin_occ
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt
 use R_lattice,      ONLY:bz_samp,nkibz
 use SC,             ONLY:SC_bands,it_now,RT_nk,RT_all2ibz
 use parallel_m,     ONLY:PAR_IND_Xk_ibz
 use real_time,      ONLY:Sigma_relax,G_lesser_reference,RT_is_dephased,l_NE_dynamics_started,RT_Deph_Matrix,&
&                         E_P_Electron_lifetime,E_P_Hole_lifetime,E_P_El_abs_lifetime,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,l_RT_CCA_Kernel,&
&                         RT_GAMMA_T_status,&
&                         E_P_Ho_abs_lifetime
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k,q
 real(SP),      intent(in)    :: Time
 complex(SP),   intent(inout) :: G_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 !
 ! Work Space
 !
 integer          :: ib1,ib2,ik
 real(SP)         :: f_k(SC_bands(2))
 !
 if(.not.l_NE_dynamics_started) return
 !
 ! Andrea 4/7/2013. Fix of potential anomalous values
 ! of the G_lesser diagonal
 !
 do ik=1,RT_nk
   do ib1=SC_bands(1),SC_bands(2)
     G_input(ib1,ib1,ik)=cI*abs(-cI*G_input(ib1,ib1,ik))
   enddo
 enddo
 !
 if(RT_is_dephased) then
   do ik=1,RT_nk
     do ib1=SC_bands(1),SC_bands(2)
       do ib2=SC_bands(1),SC_bands(2)
         Sigma_relax(ib1,ib2,ik)=-cI*RT_Deph_Matrix(ib1,ib2,RT_all2ibz(ik))*&
&                                (G_input(ib1,ib2,ik)-G_lesser_reference(ib1,ib2,ik))
       enddo
     enddo
   enddo
 else
   Sigma_relax=cZERO
 endif
 !
 if (.not.l_elph_scatt.and..not.l_elel_scatt) return
 !
 ! El-El
 !=======
 if (l_elel_scatt.and.RT_GAMMA_T_status(it_now)>0) then
#if defined _TIMING
   call timing('RT relaxation (e-e)',OPR='start')
#endif
   !
   E_E_Hole_lifetime=0._SP
   E_E_Electron_lifetime=0._SP
   !
   do ik=1,nkibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     call RT_Coulomb_relaxation(ik,E,k,q,G_input)
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
 if (l_elph_scatt.and.RT_GAMMA_T_status(it_now)>0) then
   !
#if defined _TIMING
   call timing('RT relaxation (e-p)',OPR='start')
#endif
   !
   if (l_RT_CCA_Kernel.and.l_elph_scatt) then
     E_P_Hole_lifetime=0._SP
     E_P_Electron_lifetime=0._SP
     E_P_El_abs_lifetime=0._SP
     E_P_Ho_abs_lifetime=0._SP
   endif
   !
   do ik=1,nkibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     call RT_ELPH_total_Kernel(ik,E,k,q,Time,G_input) 
     !
   enddo
   !
#if defined _TIMING
   call timing('RT relaxation (e-p)',OPR='stop')
#endif
   !
 endif
 !
 ! PARALLEL Mergering, SAVE, interpolate
 !=======================================
 if (l_elel_scatt) then
   call RT_GAMMA_save_and_interp(Time,E,k,E_E_Hole_lifetime,1)
   call RT_GAMMA_save_and_interp(Time,E,k,E_E_Electron_lifetime,2)
 endif
 if (l_elph_scatt) then
   call RT_GAMMA_save_and_interp(Time,E,k,E_P_Hole_lifetime,3)
   call RT_GAMMA_save_and_interp(Time,E,k,E_P_Electron_lifetime,4)
   call RT_GAMMA_save_and_interp(Time,E,k,E_P_Ho_abs_lifetime,5)
   call RT_GAMMA_save_and_interp(Time,E,k,E_P_El_abs_lifetime,6)
 endif
 !
 ! Constant N 
 !============
 !call RT_impose_N(k) 
 !
 ! Zero dN/dT & dE/dT 
 !====================
 if (l_elel_scatt) call RT_impose_zero_dE_and_dN(E,k,E_E_Hole_lifetime,E_E_Electron_lifetime,'elel ') 
 if (l_elph_scatt) then
   call RT_impose_zero_dE_and_dN(E,k,E_P_Hole_lifetime,E_P_Electron_lifetime,'elph1')
   call RT_impose_zero_dE_and_dN(E,k,E_P_Ho_abs_lifetime,E_P_El_abs_lifetime,'elph2')
 endif
 !
 ! Dephasing Matrix
 !==================
 !
 do ik=1,RT_nk
   do ib1=SC_bands(1),SC_bands(2)
     f_k(ib1)=-cI*G_input(ib1,ib1,ik)/spin_occ
     if (l_elel_scatt) then
       Sigma_relax(ib1,ib1,ik)=Sigma_relax(ib1,ib1,ik)+E_E_Hole_lifetime(ib1,ik)*(f_k(ib1)-1._SP)+&
&                                                      E_E_Electron_lifetime(ib1,ik)*f_k(ib1)
     endif
     if (l_RT_CCA_Kernel.and.l_elph_scatt) then
       Sigma_relax(ib1,ib1,ik)=Sigma_relax(ib1,ib1,ik)+&
&                             (E_P_Hole_lifetime(ib1,ik)+E_P_Ho_abs_lifetime(ib1,ik))*(f_k(ib1)-1._SP)+&
&                             (E_P_Electron_lifetime(ib1,ik)+E_P_El_abs_lifetime(ib1,ik))*f_k(ib1)
     endif
   enddo
 enddo
 !
end subroutine RT_relaxation

