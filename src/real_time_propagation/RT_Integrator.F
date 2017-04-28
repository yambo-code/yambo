!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
subroutine RT_Integrator(dG_new,dG_old,A_vecpot,T_step,E,k,q,Time)
 !
 !  This subroutine perform integration using different methods: EULER / EXP / NEW ??
 !        G(t),H[G(t')],dt  --> G(t+dt)
 !  * Euler's method:    G(t+dt) = G(t) - i*dt *[H(t'),G(t)]
 !  * Exp integrator:    G(t+dt) = exp(i*H(t')*dt) * G(t) * exp(-i*H(t')*dt)
 !  * ??? integrator:    G(t+dt) = (1+i*H(t')*dt)/(1-i*H(t')*dt) * G(t)
 !
 !  In addition the IP oscillations and dephasing are "analytically integrated out"
 !
 !  Each method can be used in different "multi step" approaches
 !
 !  * Single step approach:
 !    - "SIMPLE":  G(t),H[G(t)],dt  --> G(t+dt)
 !  * Two steps approaches:
 !    - "RK2" : Runge-Kutta 2nd order:
 !               G(t),H[G(t)],dt/2     --> G(t+dt/2)
 !               G(t),H[G(t+dt/2)),dt  --> G(t+dt)
 !    - "HEUN":
 !               G(t), H[G(t)],dt               --> G(t+dt)
 !               G(t),(H[G(t)]+H[G(t+dt)])/2,dt --> G(t+dt)
 !              
 !
 ! Notice that dG_in is never overwritten 
 ! --------------------------------------
 !
 use pars,           ONLY:SP,cZERO,cONE
 use R_lattice,      ONLY:bz_samp  
 use real_time,      ONLY:l_RT_EULER,l_RT_RK2, &
&                         l_NE_with_fields,G_lesser_reference,RT_E_occupations,   &
&                         RT_H_occupations,RT_delta0_occupations,l_RT_HEUN,       &
&                         integrator_step,RT_nk,RT_bands,l_RT_induced_field
 use fields,         ONLY:gauge_field
 use electrons,      ONLY:levels,spin_occ
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 implicit none
 !
 type(bz_samp),        intent(in) :: k,q
 type(levels),         intent(in) :: E
 real(SP),             intent(in) :: T_step,Time
 complex(SP),          intent(in) :: dG_old(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 complex(SP),         intent(out) :: dG_new(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 type(gauge_field), intent(inout) :: A_vecpot
 !
 complex(SP) :: dG1(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 complex(SP) :: dG2(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 !
 ! Work Space
 !
 real(SP)    :: df_H
 !
#if defined _TIMING
 call timing('RT integrator',OPR='start')
#endif
 !
 if (l_RT_EULER) then
   !
   call RT_Glesser_evolve(E,dG_old,dG_old,dG_new,T_step,T_step)
   !
 else if (l_RT_RK2) then
   !
   ! Evaluate @ T+dt/2
   !
   call RT_Glesser_evolve(E,dG_old,dG_old,dG1,T_step/2._SP,T_step/2._SP)
   !
   call RT_update_occ(dG1)
   !
   ! Fields @ T+dT/2
   !
   call FIELD_evolve(dG1,Time,T_step/2._SP)
   !
   ! Recalculate I1(t+dt/2) and I2(t+dt/2) using  G(t+dt/2) 
   !
#if defined _TIMING
   call timing('RT integrator',OPR='stop')
#endif
   call RT_Hamiltonian(dG1,A_vecpot,E,k,q,Time+T_step/2._SP)
   call RT_relaxation(dG1,E,k,q,Time+T_step/2._SP,.false.)
#if defined _TIMING
   call timing('RT integrator',OPR='start')
#endif
   !
   ! final step @ T+dt
   !
   call RT_Glesser_evolve(E,dG_old,dG1,dG_new,T_step,T_step/2._SP)
   !
 else if (l_RT_HEUN) then
   !
   ! Evaluate @ T+dt
   !
   call RT_Glesser_evolve(E,dG_old,dG_old,dG1,T_step,T_step)
   !
   call RT_update_occ(dG1)
   !
   ! Fields @ T+dT
   !
   call FIELD_evolve(dG1,Time,T_step)
   !
   ! Recalculate I1(t+dt) and I2(t+dt) using  G(t+dt) 
   !
#if defined _TIMING
   call timing('RT integrator',OPR='stop')
#endif
   call RT_Hamiltonian(dG1,A_vecpot,E,k,q,Time+T_step)
   call RT_relaxation(dG1,E,k,q,Time+T_step,.false.)
#if defined _TIMING
   call timing('RT integrator',OPR='start')
#endif
   !
   ! final step @ T+dt again
   !
   call RT_Glesser_evolve(E,dG_old,dG1,dG2,T_step,0._SP)
   !
   dG_new=(dG1+dG2)/2._SP
   !
 endif
 !
 ! Occupations @ T+dT
 !
 call RT_update_occ(dG_new)
 !
 ! Fields @ T+dT
 !
 call FIELD_evolve(dG_new,Time,T_step)
 !
 ! Reset integrator time-point
 !
 integrator_step=1
 !
#if defined _TIMING
 call timing('RT integrator',OPR='stop')
#endif
 !
 contains
   !
   subroutine FIELD_evolve(dG,T,dT)
     complex(SP),    intent(in) :: dG(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
     real(SP),       intent(in) :: T,dT
     !
     if (l_NE_with_fields) then
       !
       if (l_RT_induced_field) call RT_current_AND_polarization(k,E,dG,A_vecpot) 
       !
       call RT_propagate_fields(E,A_vecpot,T+dT,dT)
       !
     endif
     !
   end subroutine
   !
   subroutine RT_update_occ(G_inonly)
     !
     complex(SP), intent(in) :: G_inonly(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
     !
     integer                 :: ik,ib,ibp
     !
     ! Update the occupations:
     !  RT_delta0_occupations are always sensitive to small changes 
     !  for Gref~spin_occ RT_H_occ is also sensitive
     !  for Gref~0        RT_E_occ is also sensitive
     !
     do ik=1,RT_nk
       !
       do ib=RT_bands(1),RT_bands(2)
         !
         RT_E_occupations(ib,ik)= aimag(G_inonly(ib,ib,ik))+aimag(G_lesser_reference(ib,ib,ik))
         !
         ! AM, July 2015. Intel compilers nest in ordered way the mathematical parenthesis. By using 
         ! df_H this problem is solved.
         !
         df_H                   =spin_occ-aimag(G_lesser_reference(ib,ib,ik))
         RT_H_occupations(ib,ik)=-aimag(G_inonly(ib,ib,ik))+ dF_H
         !
         RT_delta0_occupations(ib,ik)=aimag(G_inonly(ib,ib,ik))
         !
       enddo
       !
     enddo
     !
   end subroutine RT_update_occ
   !
end subroutine RT_Integrator
