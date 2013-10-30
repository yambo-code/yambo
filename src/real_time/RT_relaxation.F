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
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PP_redux_wait
 use real_time,      ONLY:Sigma_relax,G_lesser_reference,RT_is_dumped,NE_dynamics,RT_Deph_Matrix,&
&                         E_P_Electron_lifetime,E_P_Hole_lifetime,E_P_El_abs_lifetime,&
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,l_RT_CCA_Kernel,&
&                         RT_dynamics_jumps,it_start,&
&                         Life_MEM_steps,SAVED_lifetime,E_P_Ho_abs_lifetime
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
 integer          :: ib1,ib2,ik,it_relative
 logical          :: l_evaluate,l_interpolate,l_save
 real(SP)         :: f_k(SC_bands(2))
 !
 if(.not.RT_is_dumped.and..not.l_elph_scatt.and..not.l_elel_scatt) return 
 !
 Sigma_relax=cZERO
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
 ! Eval or Interpolate ?
 !======================
 it_relative   = mod(it_now-it_start,Life_MEM_steps+RT_dynamics_jumps)+1
 if (it_now-it_start<2*(Life_MEM_steps+RT_dynamics_jumps) ) then
   l_evaluate    = .TRUE.
   l_interpolate = .FALSE.
   l_save        = .FALSE.
 else
   l_evaluate    = it_relative <= Life_MEM_steps
   l_interpolate = it_relative >  Life_MEM_steps
   l_save        = l_evaluate
 endif
 if (RT_dynamics_jumps==0) then
   l_evaluate    = .TRUE.
   l_interpolate = .FALSE.
   l_save        = .FALSE.
 endif
 !
 ! El-El
 !=======
 if (l_elel_scatt.and.l_evaluate) then
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
 endif
 !
#if defined _ELPH
 !
 ! El-Ph
 !=======
 if (l_elph_scatt.and.l_evaluate) then
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
 endif
 !
#endif
 !
 ! PARALLEL Mergering
 !====================
 !
 ! I make copies of the k-resolved matrix. In the future a completely independet
 ! k loop could be implemented
 !
 if (l_elph_scatt.or.l_elel_scatt) then
   if (l_evaluate) then
     do ik=1,RT_nk
       if (l_elel_scatt) then
         call PP_redux_wait(E_E_Electron_lifetime(:,ik))
         call PP_redux_wait(E_E_Hole_lifetime(:,ik))
         if (l_save) then
           SAVED_lifetime(:,ik,1, it_relative)=E_E_Hole_lifetime(:,ik)
           SAVED_lifetime(:,ik,2, it_relative)=E_E_Electron_lifetime(:,ik)
         endif
       endif
       if (l_RT_CCA_Kernel.and.l_elph_scatt) then
         call PP_redux_wait(E_P_Electron_lifetime(:,ik))
         call PP_redux_wait(E_P_Hole_lifetime(:,ik))
         call PP_redux_wait(E_P_El_abs_lifetime(:,ik))
         call PP_redux_wait(E_P_Ho_abs_lifetime(:,ik))
         if (l_save) then
           SAVED_lifetime(:,ik,3, it_relative)=E_P_Hole_lifetime(:,ik)
           SAVED_lifetime(:,ik,4, it_relative)=E_P_Electron_lifetime(:,ik)
           SAVED_lifetime(:,ik,5, it_relative)=E_P_Ho_abs_lifetime(:,ik)
           SAVED_lifetime(:,ik,6, it_relative)=E_P_El_abs_lifetime(:,ik)
         endif
       endif
     enddo
   else if (l_interpolate) then
     do ik=1,RT_nk
       do ib1=SC_bands(1),SC_bands(2)
         if (l_elel_scatt) then
           call RT_Lifetimes_Interpolation(E_E_Hole_lifetime(ib1,ik),SAVED_lifetime(ib1,ik,1,:))
           call RT_Lifetimes_Interpolation(E_E_Electron_lifetime(ib1,ik),SAVED_lifetime(ib1,ik,2,:))
         endif
         if (l_RT_CCA_Kernel.and.l_elph_scatt) then
           call RT_Lifetimes_Interpolation(E_P_Hole_lifetime(ib1,ik),SAVED_lifetime(ib1,ik,3,:))
           call RT_Lifetimes_Interpolation(E_P_Electron_lifetime(ib1,ik),SAVED_lifetime(ib1,ik,4,:))
           call RT_Lifetimes_Interpolation(E_P_Ho_abs_lifetime(ib1,ik),SAVED_lifetime(ib1,ik,5,:))
           call RT_Lifetimes_Interpolation(E_P_El_abs_lifetime(ib1,ik),SAVED_lifetime(ib1,ik,6,:))
         endif
       enddo
     enddo
   endif
 endif
 !
 ! Number conservation
 !=====================
 !
 call RT_impose_N_conservation(E,k)
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
 if(RT_is_dumped.and.NE_dynamics) then
   do ik=1,RT_nk
     do ib1=SC_bands(1),SC_bands(2)
       do ib2=SC_bands(1),SC_bands(2)
         Sigma_relax(ib1,ib2,ik)=Sigma_relax(ib1,ib2,ik)-cI*RT_Deph_Matrix(ib1,ib2,RT_all2ibz(ik))*&
&                                (G_input(ib1,ib2,ik)-G_lesser_reference(ib1,ib2,ik))
       enddo
     enddo
   enddo
 endif
 !
end subroutine RT_relaxation

