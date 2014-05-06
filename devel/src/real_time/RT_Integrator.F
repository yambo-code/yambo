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
 ! Notice that G_old is never overwritten (It is modified by the RWA)      
 ! ------------------------------------------------------------
 !
 use pars,           ONLY:SP,schlen
 use SC,             ONLY:SC_bands,RT_nk
 use R_lattice,      ONLY:bz_samp  
 use real_time,      ONLY:l_RT_EULER,l_RT_RK2,l_RT_EXP,l_RT_INTERPOLATE,l_RT_RWA, &
&                         l_NE_with_fields,G_lesser_reference,RT_E_occupations,   &
&                         RT_H_occupations,RT_delta0_occupations
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
 complex(SP),       intent(inout) :: G_old(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
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
   call RT_evolve(T_step)
   !
 else if (l_RT_RK2) then
   !
   ! Evaluate @ dt/2
   !
   call RT_evolve(T_step/2._SP)
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
   call RT_evolve(T_step)
   !
 endif
 !
 if (l_NE_with_fields) call RT_propagate_fields(k,E,G_new,A_new,A_old,Time,T_step)
 !
#if defined _TIMING
 call timing('RT integrator',OPR='stop')
#endif
 !
 contains
   !
   subroutine RT_evolve(dT)
     !
     use real_time,   ONLY:RT_apply_RWA
     !
     real(SP),intent(in)    :: dT
     !
     integer                :: ib
     complex(SP)            :: G_old_full(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
     !
     do ik=1,RT_nk
       !
       if (l_RT_RWA) call RT_apply_RWA(ik,E,Time,"APPLY",G_old(:,:,ik),&
         &                                            M2=G_lesser_reference(:,:,ik))
       !
       G_old_full=G_old(:,:,ik)+G_lesser_reference(:,:,ik)
       !
       if (l_RT_EXP) then
         !
         ! U(t) = exp ( Ho_plus_Sigma * t)
         !
         call RT_build_U1_and_U2(ik,I1_k,I2_k,dT)
         !
         ! G(T+dt) = U(dt) G(T) U(-dt)
         !
         call RT_EULEREXP_step(G_new(:,:,ik),G_old_full,I1_k,I2_k,ik,dT)
         G_new(:,:,ik)=G_new(:,:,ik)-G_lesser_reference(:,:,ik)
         !
       else
         !
         ! I1(t) = Ho_plus_Sigma*G ;  I1(t) = G*Ho_plus_Sigma
         !
         call RT_build_I1_and_I2(G_old_full,ik,I1_k,I2_k)
         !
         ! G(T+dt) = G(T) - i dt *(I1(T) - I2(T))
         !
         if (.not.l_RT_INTERPOLATE) call RT_EULER_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,dT)
         if (l_RT_INTERPOLATE)      call RT_INTERPOLATED_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,ik,Time,dT,.TRUE.)
         !
       endif
       !
       if (l_RT_RWA) call RT_apply_RWA(ik,E,Time,"REMOVE",G_old(:,:,ik),&
         &                  M2=G_lesser_reference(:,:,ik),M3=G_new(:,:,ik))
       !
       ! Dephasing and carriers relaxation step
       ! G(T+dt) = G(T) - i dt * Simga_relax(T)
       !
       call RT_DEPHASING_step(G_new(:,:,ik),ik,dT)
       !
       ! Update the occupations:
       !  RT_delta0_occupations are always sensitive to small changes 
       !  for Gref~spin_occ RT_H_occ is also sensitive
       !  for Gref~0        RT_E_occ is also sensitive
       !
       do ib=SC_bands(1),SC_bands(2)
         RT_E_occupations(ib,ik)= aimag(G_new(ib,ib,ik))+            aimag(G_lesser_reference(ib,ib,ik))
         RT_H_occupations(ib,ik)=-aimag(G_new(ib,ib,ik))+ ( spin_occ-aimag(G_lesser_reference(ib,ib,ik)) )
         !
         ! Symmetrization
         !
         G_new(ib,ib,ik)=cmplx(0._SP,aimag(G_new(ib,ib,ik)))
         !
         if( RT_E_occupations(ib,ik)<0._SP  .or. RT_H_occupations(ib,ik)>spin_occ ) then
           RT_E_occupations(ib,ik)=0._SP
           RT_H_occupations(ib,ik)=spin_occ
           G_new(ib,ib,ik)=-cmplx(0._SP,aimag(G_lesser_reference(ib,ib,ik)))
         endif
         if( RT_H_occupations(ib,ik)<0._SP  .or. RT_E_occupations(ib,ik)>spin_occ) then
           RT_E_occupations(ib,ik)=spin_occ
           RT_H_occupations(ib,ik)=0._SP
           G_new(ib,ib,ik)= cmplx(0._SP,spin_occ-aimag(G_lesser_reference(ib,ib,ik)))
         endif
         !
         RT_delta0_occupations(ib,ik)=aimag(G_new(ib,ib,ik))
         !
       enddo
       !
     enddo
     !
   end subroutine RT_evolve
   !
end subroutine RT_Integrator
