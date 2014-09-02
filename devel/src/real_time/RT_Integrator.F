!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine RT_Integrator(dG_new,dG_old,A_out,A_in,T_step,E,k,q,Time)
 !
 !  This subroutine perform integration using different methods
 !
 !  Euler's method:          yE(t+dt) = y(t) + dt *   f(t,y(t))
 !  Runge-Kutta 2nd order:   yR(t+dt) = y(t) + dt *   f(t+dt/2,yE(t+dt/2))
 !  Heun's method:           yH(t+dt) = y(t) + dt * ( f(t,y(t))+f(t+dt,yE(t+dt)) )/2
 !                                    = ( yE(t+dt) + y(t) + dt * f(t+dt,yE(t+dt)))/2
 !
 ! Notice that dG_in is never overwritten 
 ! --------------------------------------
 !
 use pars,           ONLY:SP,cZERO,cONE
 use SC,             ONLY:SC_bands
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt
 use R_lattice,      ONLY:bz_samp  
 use real_time,      ONLY:l_RT_EULER,l_RT_RK2,l_RT_EXP,l_RT_INTERPOLATE,l_RT_RWA, &
&                         l_NE_with_fields,G_lesser_reference,RT_E_occupations,   &
&                         RT_H_occupations,RT_delta0_occupations,l_RT_HEUN,       &
&                         integrator_step,RT_nk
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
 complex(SP),          intent(in) :: dG_old(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 complex(SP),       intent(inout) :: dG_new(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 type(gauge_field),    intent(in) :: A_in
 type(gauge_field), intent(inout) :: A_out
 !
 complex(SP) :: dG1(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 complex(SP) :: dG2(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 ! 
 !
#if defined _TIMING
 call timing('RT integrator',OPR='start')
#endif
 !
 if (l_RT_EULER) then
   !
   call RT_evolve(dG_old,dG_new,T_step,T_step)
   !
 else if (l_RT_RK2) then
   !
   ! Evaluate @ dt/2
   !
   call RT_evolve(dG_old,dG1,T_step/2._SP,T_step/2._SP)
   !
   if (l_NE_with_fields) call RT_propagate_fields(k,E,dG1,A_out,A_in,Time,T_step/2._SP)
   !
   ! Recalculate I1(t+dt/2) and I2(t+dt/2) using  G(t+dt/2) 
   !
#if defined _TIMING
   call timing('RT integrator',OPR='stop')
#endif
   call RT_Collision(dG1,A_out,E,k,q,Time+T_step/2._SP)
#if defined _TIMING
   call timing('RT integrator',OPR='start')
#endif
   !
   ! final step @ dt
   !
   call RT_evolve(dG1,dG_new,T_step,T_step/2._SP)
   !
 else if (l_RT_HEUN) then
   !
   ! Evaluate @ dt
   !
   call RT_evolve(dG_old,dG1,T_step,T_step)
   !
   if (l_NE_with_fields) call RT_propagate_fields(k,E,dG1,A_out,A_in,Time,T_step)
   !
   ! Recalculate I1(t+dt) and I2(t+dt) using  G(t+dt) 
   !
#if defined _TIMING
   call timing('RT integrator',OPR='stop')
#endif
   call RT_Collision(dG1,A_out,E,k,q,Time+T_step)
#if defined _TIMING
   call timing('RT integrator',OPR='start')
#endif
   !
   ! final step @ dt again
   !
   call RT_evolve(dG1,dG2,T_step,0._SP)
   !
   dG_new=(dG1+dG2)/2._SP
   !
   call RT_update_occ(dG_new)
   !
 endif
 !
 if (l_NE_with_fields) call RT_propagate_fields(k,E,dG_new,A_out,A_in,Time,T_step)
 !
#if defined _TIMING
 call timing('RT integrator',OPR='stop')
#endif
 !
 integrator_step=1
 !
 contains
   !
   subroutine RT_evolve(dG_in,dG_out,dT,dTp)
     !
     ! dG_old = dG(T0)     --> Time at first iteration in integrator with more than one step
     ! dG_in  = dG(T )     --> Effective time, i.e. at the second step of RK2 T=T0+dt/2
     ! dG_out = dG(T0+dt)
     !
     !  T0+dt = T+dt'
     !
     real(SP),     intent(in) :: dT,dTp
     complex(SP),  intent(in) :: dG_in(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
     complex(SP), intent(out) :: dG_out(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
     !
     integer                :: ik,ib
     complex(SP)            :: G_loc(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
     complex(SP)            :: I1_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
     complex(SP)            :: I2_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
     complex(SP)            :: I1_matrix(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
     !
     if(l_RT_EXP) then
       I1_matrix=cZERO
       do ib=SC_bands(1),SC_bands(2)
         I1_matrix(ib,ib)=cONE
       enddo
     endif
     !
     do ik=1,RT_nk
       !
       G_loc=dG_in(:,:,ik)+G_lesser_reference(:,:,ik)
       !
       if (l_RT_EXP) then
         !
         ! U(t) = exp ( Ho_plus_Sigma * t)
         !
         call RT_build_U1_and_U2(ik,I1_k,I2_k,dT)
         !
         ! G(T0+dt) = U(T;dt) G(T0) U(T;-dt)
         !
         call RT_EULEREXP_step(dG_out(:,:,ik),dG_old(:,:,ik),G_lesser_reference(:,:,ik),I1_k,I2_k,I1_matrix)
         !
       else
         !
         ! I1(t) = Ho_plus_Sigma*G ;  I2(t) = G*Ho_plus_Sigma
         !
         call RT_build_I1_and_I2(G_loc,ik,I1_k,I2_k)
         !
         ! G(T0+dt) -G(T0)= - i dt *(I1(T) - I2(T))
         !
         if (.not.l_RT_INTERPOLATE) call RT_EULER_step(dG_out(:,:,ik),I1_k,I2_k,dT)
         !if (l_RT_INTERPOLATE)      call RT_INTERPOLATED_step(G_out(:,:,ik),dG_old(:,:,ik),I1_k,I2_k,ik,Time,dT,.TRUE.)
         !
       endif
       !
       ! G(T0+dt) = G(T0) e^{-i\delta\epsilon dt} - i dt *(I1(T) - I2(T)) e^{-i\delta\epsilon (dt')}
       ! if(l_RT_RWA) dephasing is integrated analitically in RT_apply_RWA
       !
       if(     l_RT_RWA) call RT_apply_RWA(ik,E,dT,dTp,dG_out(:,:,ik),dG_old(:,:,ik))
       if(.not.l_RT_RWA) call RT_Dephasing_step(dG_out(:,:,ik),dG_in(:,:,ik),ik,dT)
       !
       ! Carriers relaxation step
       ! G(T0+dt) = G(T0) - i dt * Sigma_relax(T)
       !
       call RT_Scattering_step(dG_out(:,:,ik),ik,dT)
       !
     enddo
     !
     call RT_update_occ(dG_out)
     !
     integrator_step=integrator_step+1
     !
   end subroutine RT_evolve
   !
   subroutine RT_update_occ(G_in)
     !
     complex(SP), intent(inout) :: G_in(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
     !
     integer                :: ik,ib
     !
     ! Update the occupations:
     !  RT_delta0_occupations are always sensitive to small changes 
     !  for Gref~spin_occ RT_H_occ is also sensitive
     !  for Gref~0        RT_E_occ is also sensitive
     !
     do ik=1,RT_nk
       !
       do ib=SC_bands(1),SC_bands(2)
         !
         ! Symmetrization
         !
         G_in(ib,ib,ik)=cmplx(0._SP,aimag(G_in(ib,ib,ik)))
         !
         RT_E_occupations(ib,ik)= aimag(G_in(ib,ib,ik))+            aimag(G_lesser_reference(ib,ib,ik))
         RT_H_occupations(ib,ik)=-aimag(G_in(ib,ib,ik))+ ( spin_occ-aimag(G_lesser_reference(ib,ib,ik)) )
         !
         if( RT_E_occupations(ib,ik)<0._SP  .or. RT_H_occupations(ib,ik)>spin_occ ) then
           RT_E_occupations(ib,ik)=0._SP
           RT_H_occupations(ib,ik)=spin_occ
           G_in(ib,ib,ik)=-cmplx(0._SP,aimag(G_lesser_reference(ib,ib,ik)))
         endif
         if( RT_H_occupations(ib,ik)<0._SP  .or. RT_E_occupations(ib,ik)>spin_occ) then
           RT_E_occupations(ib,ik)=spin_occ
           RT_H_occupations(ib,ik)=0._SP
           G_in(ib,ib,ik)= cmplx(0._SP,spin_occ-aimag(G_lesser_reference(ib,ib,ik)))
         endif
         !
         RT_delta0_occupations(ib,ik)=aimag(G_in(ib,ib,ik))
         !
       enddo
       !
     enddo
     !
   end subroutine RT_update_occ
   !
end subroutine RT_Integrator
