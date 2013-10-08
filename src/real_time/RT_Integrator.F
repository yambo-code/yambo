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
subroutine RT_Integrator(G_new,G_old,A_new,A_old,T_step,E,k,q,Integrator,Time)
 !
 !  This subroutine perform integration using different methods
 !
 ! if(Integrator=="EULER") simple Euler's method
 !
 !    y(t+dt) = y(t) + dt * f(t,y(t))
 !
 ! else if(Integrator=="RK2")  Runge-Kutta 2nd order
 !
 !    y(t+dt) = y(t) + dt * f(t+dt/2,y(t+dt/2))
 !
 ! Notice that G_old is never overwritten       
 ! ------------------------------------------------------------
 !
 use pars,           ONLY:SP,schlen
 use com,            ONLY:error
 use SC,             ONLY:SC_bands,SC_nbands,RT_ibz_coo,RT_all2ibz,RT_nk
 use R_lattice,      ONLY:bz_samp  
 use real_time,      ONLY:EULER,RK2,EULEREXP,RK2EXP,l_NE_with_fields
 use fields,         ONLY:gauge_field
 use electrons,      ONLY:levels
 implicit none
 !
 character(schlen),    intent(in) :: Integrator
 type(bz_samp),        intent(in) :: k,q
 type(levels),         intent(in) :: E
 real(SP),             intent(in) :: T_step,Time
 complex(SP),          intent(in) :: G_old(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),k%nibz)
 complex(SP),       intent(inout) :: G_new(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),k%nibz)
 type(gauge_field),    intent(in) :: A_old
 type(gauge_field), intent(inout) :: A_new
 ! 
 ! Work Space
 !
 integer     :: ik,SC_bands2_dot_nibz,ik_rim
 complex(SP) :: I1_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP) :: I2_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 !
 SC_bands2_dot_nibz=SC_nbands**2*k%nibz
 !
 if(trim(Integrator)==EULER) then
   !
   ! G(T+dt) = G(T) - i dt *(I1(T) - I2(T))
   !
   do ik=1,RT_nk
     call RT_build_I1_and_I2(G_old(:,:,ik),RT_all2ibz(ik),ik,I1_k,I2_k)
     call RT_EULER_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,T_step)
   enddo
   !
   if (l_NE_with_fields) call RT_propagate_fields(A_new,A_old,Time,T_step)
   !
 else if(trim(Integrator)==EULEREXP) then
   !
   ! G(T+dt) = U(dt) G(T) U(-dt) - i dt * Simga_relax(T)
   ! U(t) = exp ( Ho_plus_Sigma * t)
   !
   do ik=1,k%nibz
     call RT_build_U1_and_U2(ik,I1_k,I2_k,T_step)
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       call RT_EULEREXP_step(G_new(:,:,ik_rim),G_old(:,:,ik_rim),I1_k,I2_k,ik_rim,T_step)
     enddo
   enddo
   !
   if (l_NE_with_fields) call RT_propagate_fields(A_new,A_old,Time,T_step)
   !
 else if(trim(Integrator)==RK2) then
   !
   ! Evaluate G(t+dt/2)
   !
   do ik=1,RT_nk
     call RT_build_I1_and_I2(G_old(:,:,ik),RT_all2ibz(ik),ik,I1_k,I2_k)
     call RT_EULER_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,T_step/2._SP)
   enddo
   !
   ! Recalculate I1(t+dt/2) and I2(t+dt/2) using  G(t+dt/2) 
   !
   if (l_NE_with_fields) call RT_propagate_fields(A_new,A_old,Time,T_step/2._SP)
   !
   call RT_Collision(G_new,A_new,E,k,q,Time+T_step/2._SP)
   !
   ! final step G(t+dt)=G(t) + dt * (I1(t+dt/2) - I2(t+dt/2))
   !
    do ik=1,RT_nk
     call RT_build_I1_and_I2(G_new(:,:,ik),RT_all2ibz(ik),ik,I1_k,I2_k)
     call RT_EULER_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,T_step)
   enddo
   !
   if (l_NE_with_fields) call RT_propagate_fields(A_new,A_old,Time,T_step)
   !
 else if(trim(Integrator)==RK2EXP) then
   !
   ! Evaluate G(t+dt/2)
   !
   do ik=1,k%nibz
     call RT_build_U1_and_U2(ik,I1_k,I2_k,T_step/2._SP)
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       call RT_EULEREXP_step(G_new(:,:,ik_rim),G_old(:,:,ik_rim),I1_k,I2_k,ik_rim,T_step/2._SP)
     enddo
   enddo
   !
   ! Recalculate I1(t+dt/2) and Ho_plus_Sigma(t+dt/2) using  G(t+dt/2) 
   !
   if (l_NE_with_fields) call RT_propagate_fields(A_new,A_old,Time,T_step/2._SP)
   !
   call RT_Collision(G_new,A_new,E,k,q,Time+T_step/2._SP)
   !
   ! final step G(t+dt)= U1(t + dt) G(t) U2(t+dt) + dt * Sigma_relax(t+dt/2)
   !
   do ik=1,k%nibz
     call RT_build_U1_and_U2(ik,I1_k,I2_k,T_step)
     do ik_rim=RT_ibz_coo(ik,1),RT_ibz_coo(ik,2)
       call RT_EULEREXP_step(G_new(:,:,ik_rim),G_old(:,:,ik_rim),I1_k,I2_k,ik_rim,T_step)
     enddo
   enddo
   !
   if (l_NE_with_fields) call RT_propagate_fields(A_new,A_old,Time,T_step)
   !
 else
   !
   call error(" Unknown Integrator!")
   !
 endif   
 !
end subroutine RT_Integrator
