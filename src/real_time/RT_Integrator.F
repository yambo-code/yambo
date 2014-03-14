!
! Copyright (C) 2000-2014 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Integrator(G_new,G_old,A_new,A_old,T_step,E,k,q,Time)
 !
 !  This subroutine perform integration using different methods
 !
 ! if(l_RT_EULER) simple Euler's method
 !
 !    y(t+dt) = y(t) + dt * f(t,y(t))
 !
 ! else if(l_RT_RK2)  Runge-Kutta 2nd order
 !
 !    y(t+dt) = y(t) + dt * f(t+dt/2,y(t+dt/2))
 !
 ! Notice that G_old is never overwritten       
 ! ------------------------------------------------------------
 !
 use pars,           ONLY:SP,schlen
 use SC,             ONLY:SC_bands,RT_nk
 use R_lattice,      ONLY:bz_samp  
 use real_time,      ONLY:l_RT_EULER,l_RT_RK2,l_RT_EXP,l_RT_INTERPOLATE,l_RT_RWA,l_NE_with_fields
 use fields,         ONLY:gauge_field
 use electrons,      ONLY:levels
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 implicit none
 !
 type(bz_samp),        intent(in) :: k,q
 type(levels),         intent(in) :: E
 real(SP),             intent(in) :: T_step,Time
 complex(SP),          intent(in) :: G_old(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 complex(SP),       intent(inout) :: G_new(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 type(gauge_field),    intent(in) :: A_old
 type(gauge_field), intent(inout) :: A_new
 ! 
 ! Work Space
 !
 integer     :: ik
 complex(SP) :: I1_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP) :: I2_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 !
#if defined _TIMING
 call timing('RT integrator',OPR='start')
#endif
 !
 if(l_RT_EULER) then
   !
   do ik=1,RT_nk
     !
     if (l_RT_RWA) call RT_apply_RWA(G_old(:,:,ik),ik,E,Time,"APPLY")
     !
     if (l_RT_EXP) then
       !
       ! U(t) = exp ( Ho_plus_Sigma * t)
       !
       call RT_build_U1_and_U2(ik,I1_k,I2_k,T_step)
       !
       ! G(T+dt) = U(dt) G(T) U(-dt) - i dt * Simga_relax(T)
       !
       call RT_EULEREXP_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,ik,T_step)
       !
     else
       !
       call RT_build_I1_and_I2(G_old(:,:,ik),ik,I1_k,I2_k)
       !
       ! G(T+dt) = G(T) - i dt *(I1(T) - I2(T))
       !
       if (.not.l_RT_INTERPOLATE) call RT_EULER_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,T_step)
       if (l_RT_INTERPOLATE)      call RT_INTERPOLATED_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,ik,Time,T_step,.TRUE.)
       !
     endif
     !
     if (l_RT_RWA) call RT_apply_RWA(G_new(:,:,ik),ik,E,Time+T_step,"REMOVE")
     !
     ! Dephasing and carriers relaxation step
     !
     call RT_DEPHASING_step(G_new(:,:,ik),ik,T_step)
     !
   enddo
   !
 else if (l_RT_RK2) then
   !
   ! Evaluate @ dt/2
   !
   do ik=1,RT_nk
     if (l_RT_RWA) call RT_apply_RWA(G_old(:,:,ik),ik,E,Time,"APPLY")
     if (l_RT_EXP) then
       call RT_build_U1_and_U2(ik,I1_k,I2_k,T_step/2._SP)
       call RT_EULEREXP_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k)
     else
       call RT_build_I1_and_I2(G_old(:,:,ik),ik,I1_k,I2_k)
       if (.not.l_RT_INTERPOLATE) call RT_EULER_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,T_step/2._SP)
       if (l_RT_INTERPOLATE)      call RT_INTERPOLATED_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,ik,Time,T_step/2.)
     endif
     if (l_RT_RWA) call RT_apply_RWA(G_new(:,:,ik),ik,E,Time+T_step/2.,"REMOVE")
     call RT_DEPHASING_step(G_new(:,:,ik),ik,T_step/2.)
   enddo
   !
   if (l_NE_with_fields) call RT_propagate_fields(k,E,G_new,A_new,A_old,Time,T_step/2._SP)
   !
#if defined _TIMING
   call timing('RT integrator',OPR='stop')
#endif
   !
   ! Recalculate I1(t+dt/2) and I2(t+dt/2) using  G(t+dt/2) 
   !
   call RT_Collision(G_new,A_new,E,k,q,Time+T_step/2._SP)
   !
#if defined _TIMING
   call timing('RT integrator',OPR='start')
#endif
   !
   ! final step @ dt
   !
   do ik=1,RT_nk
     if (l_RT_EXP) then
       call RT_build_U1_and_U2(ik,I1_k,I2_k,T_step)
       call RT_EULEREXP_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k)
     else
       call RT_build_I1_and_I2(G_new(:,:,ik),ik,I1_k,I2_k)
       if (.not.l_RT_INTERPOLATE) call RT_EULER_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,T_step)
       if (l_RT_INTERPOLATE)      call RT_INTERPOLATED_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,ik,Time,T_step)
     endif
     if (l_RT_RWA) call RT_apply_RWA(G_new(:,:,ik),ik,E,Time+T_step,"REMOVE")
     call RT_DEPHASING_step(G_new(:,:,ik),ik,T_step)
   enddo
   !
 endif
 !
 if (l_NE_with_fields) call RT_propagate_fields(k,E,G_new,A_new,A_old,Time,T_step)
 !
#if defined _TIMING
 call timing('RT integrator',OPR='stop')
#endif
 !
end subroutine RT_Integrator
