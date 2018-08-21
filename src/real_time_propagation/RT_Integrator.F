!
!        Copyright (C) 2000-2018 the YAMBO team
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
 use R_lattice,      ONLY:bz_samp  
 use real_time,      ONLY:l_RT_EULER,l_RT_RK2,l_RT_EXP,l_RT_RWA, &
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
 complex(SP),       intent(inout) :: dG_new(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 type(gauge_field),    intent(in) :: A_in
 type(gauge_field), intent(inout) :: A_out
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
   call RT_evolve(dG_old,dG_new,T_step,T_step)
   !
 else if (l_RT_RK2) then
   !
   ! Evaluate @ T+dt/2
   !
   call RT_evolve(dG_old,dG1,T_step/2._SP,T_step/2._SP)
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
   call RT_Hamiltonian(dG1,A_out,E,k)
   !
   !
#if defined _TIMING
   call timing('RT integrator',OPR='start')
#endif
   !
   ! final step @ T+dt
   !
   call RT_evolve(dG1,dG_new,T_step,T_step/2._SP)
   !
 else if (l_RT_HEUN) then
   !
   ! Evaluate @ T+dt
   !
   call RT_evolve(dG_old,dG1,T_step,T_step)
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
   call RT_Hamiltonian(dG1,A_out,E,k)
   !
   !
#if defined _TIMING
   call timing('RT integrator',OPR='start')
#endif
   !
   ! final step @ T+dt again
   !
   call RT_evolve(dG1,dG2,T_step,0._SP)
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
       if (l_RT_induced_field) call RT_current_AND_polarization(k,E,dG,A_in) 
       !
       call RT_propagate_fields(E,A_out,T+dT,dT)
       !
     endif
     !
   end subroutine
   !
   subroutine RT_evolve(dG_in,dG_out,dT,dTp)
     !
     ! dG_old = dG(T0)     --> Time at first iteration in integrator with more than one step
     ! dG_in  = dG(T )     --> Effective time, i.e. at the second step of RK2 T=T0+dt/2
     ! dG_out = dG(T0+dt)
     !
     !  T0+dt = T+dt'
     !
     real(SP),       intent(in) :: dT,dTp
     complex(SP),    intent(in) :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
     complex(SP),   intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
     !
     integer                :: ik,ib
     complex(SP)            :: G_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
     complex(SP)            :: I1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
     complex(SP)            :: I2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
     complex(SP)            :: I1_matrix(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
     !
     if(l_RT_EXP) then
       I1_matrix=cZERO
       do ib=RT_bands(1),RT_bands(2)
         I1_matrix(ib,ib)=cONE
       enddo
     endif
     !
     do ik=1,RT_nk
       !
       if (l_RT_EXP) then
         !
         ! U(T;dt) = exp ( Ho_plus_Sigma(T) * dt)
         !
         call RT_build_U1_and_U2(ik,I1_k,I2_k,dT)
         !
         ! dG_out = G(T0+dt) -G(T0) = U(T;dt) G(T0) U(T;-dt) - G(T0)
         !
         call RT_EULEREXP_step(dG_out(:,:,ik),dG_old(:,:,ik),G_lesser_reference(:,:,ik),I1_k,I2_k,I1_matrix)
         !
       else
         !
         ! I1(t) = Ho_plus_Sigma*G   
         ! I2(t) = G*Ho_plus_Sigma
         !
         G_tmp=dG_in(:,:,ik)+G_lesser_reference(:,:,ik)
         !
         call RT_build_I1_and_I2(G_tmp,ik,I1_k,I2_k)
         !
         ! dG_out = G(T0+dt) -G(T0)= - i dt *(I1(T) - I2(T))
         !
         call RT_EULER_step(dG_out(:,:,ik),I1_k,I2_k,dT)
         !
       endif
       !
       ! Note that the input of RT_Dephasing_step/RT_apply_RWA is the only (I1_k G-I2_k G) contribution
       !
       if(l_RT_RWA) then
         !
         ! G(T0+dt) = G(T0) e^{-i\delta\epsilon dt} - i dt *(I1(T) - I2(T)) e^{-i\delta\epsilon (dt')}
         !
         ! Dephasing is integrated analitically in this case.
         !
         call RT_apply_RWA(ik,E,dT,dTp,dG_out(:,:,ik),dG_old(:,:,ik))
         !
       else
         !
         call RT_Dephasing_step(dG_out(:,:,ik),dG_old(:,:,ik),ik,dT)
         !
       endif
       !
       !
     enddo
     !
     call RT_G_symmetrization(dG_out)
     !
     integrator_step=integrator_step+1
     !
   end subroutine RT_evolve
   !
   subroutine RT_G_symmetrization(G_inout)
     !
     complex(SP), intent(inout) :: G_inout(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
     !
     integer                :: ik,ib,ibp
     real(SP)               :: E_occ_tmp,H_occ_tmp
     complex(SP)            :: tmp
     !
     do ik=1,RT_nk
       !
       do ib=RT_bands(1),RT_bands(2)
         !
         G_inout(ib,ib,ik)=cmplx(0._SP,aimag(G_inout(ib,ib,ik)))
         !
         E_occ_tmp= aimag(G_inout(ib,ib,ik))+            aimag(G_lesser_reference(ib,ib,ik))
         H_occ_tmp=-aimag(G_inout(ib,ib,ik))+ ( spin_occ-aimag(G_lesser_reference(ib,ib,ik)) )
         !
         if ( E_occ_tmp<0._SP .or. H_occ_tmp>spin_occ ) G_inout(ib,ib,ik)=                      G_lesser_reference(ib,ib,ik)
         if ( H_occ_tmp<0._SP .or. E_occ_tmp>spin_occ ) G_inout(ib,ib,ik)=cmplx(0._SP,spin_occ)-G_lesser_reference(ib,ib,ik)
         !
         do ibp=ib+1,RT_bands(2)
           !
           tmp=( G_inout(ib,ibp,ik)-conjg(G_inout(ibp,ib,ik)) )/2._SP
           !
           G_inout(ib,ibp,ik)=       tmp
           G_inout(ibp,ib,ik)=-conjg(tmp)
           !
         enddo
         !
       enddo
       !
     enddo
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
