!
! Copyright (C) 2000-2012 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Integrator(G_new,G_old,A_new,A_old,T_step,Xen,k,q,Integrator,Time)
 !
 !  This subroutine perform integration using different methods
 !
 ! if(Integrator=="EULER") simple Euler's method
 !
 !    y(t+dt) = y(t) + dt * f(t,y(t))
 !
 ! elseif(Integrator=="RK2")  Runge-Kutta 2nd order
 !
 !    y(t+dt) = y(t) + dt * f(t+dt/2,y(t+dt/2))
 !
 ! Notice that G_old is never overwritten       
 ! ------------------------------------------------------------
 !
 use pars,           ONLY:SP,schlen
 use com,            ONLY:error
 use SC,             ONLY:SC_bands,SC_nbands
 use R_lattice,      ONLY:bz_samp  
 use real_time,      ONLY:Ho_plus_Sigma,EULER,RK2,l_NE_with_fields
 use fields,         ONLY:gauge_field
 use electrons,      ONLY:levels
 implicit none
 !
 character(schlen),    intent(in) :: Integrator
 type(bz_samp),        intent(in) :: k,q
 type(levels),         intent(in) :: Xen
 real(SP),             intent(in) :: T_step,Time
 complex(SP),          intent(in) :: G_old(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),k%nibz)
 complex(SP),       intent(inout) :: G_new(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),k%nibz)
 type(gauge_field),    intent(in) :: A_old
 type(gauge_field), intent(inout) :: A_new
 ! 
 ! Work Space
 !
 integer     :: ik,i1,i2,SC_bands2_dot_nibz
 complex(SP) :: I1_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP) :: I2_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 !
 SC_bands2_dot_nibz=SC_nbands**2*k%nibz
 !
 if(trim(Integrator)==EULER) then
   !
   ! G(T+dt) = G(T) - i dt *(I1(T) - I2(T))
   !
   do ik=1,k%nibz
     call build_I1_and_I2(G_old(:,:,ik),ik,I1_k,I2_k)
     call EULER_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,T_step)
   enddo
   !
   if(l_NE_with_fields) call RT_propagate_fields(A_new,A_old,Time,T_step)
   !
 elseif(trim(Integrator)==RK2) then
   !
   ! Evaluate G(t+dt/2)
   !
   do ik=1,k%nibz
     call build_I1_and_I2(G_old(:,:,ik),ik,I1_k,I2_k)
     call EULER_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,T_step/2._SP)
   enddo
   !
   ! Recalculate I1(t+dt/2) and I2(t+dt/2) using  G(t+dt/2) 
   !
   if(l_NE_with_fields) call RT_propagate_fields(A_new,A_old,Time,T_step/2._SP)
   !
   call RT_Collision(G_new,A_new,Xen,k,q,Time+T_step/2._SP)
   !
   ! final step G(t+dt)=G(t) + dt * (I1(t+dt/2) - I2(t+dt/2))
   !
    do ik=1,k%nibz
     call build_I1_and_I2(G_new(:,:,ik),ik,I1_k,I2_k)
     call EULER_step(G_new(:,:,ik),G_old(:,:,ik),I1_k,I2_k,T_step)
   enddo
   !
 else
   !
   call error(" Unknown Integrator!")
   !
 endif   
 !
end subroutine RT_Integrator

subroutine build_I1_and_I2(G_input_k,ik,I1_k,I2_k)
  !
  use pars,           ONLY:SP,cZERO
  use real_time,      ONLY:Ho_plus_Sigma,Sigma_relax
  use SC,             ONLY:SC_bands,SC_nbands
  use wrapper,        ONLY:M_by_M
  implicit none
  !
  integer, intent(in)     :: ik
  complex(SP), intent(in)  :: G_input_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
  complex(SP), intent(out) :: I1_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
  complex(SP), intent(out) :: I2_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
  !
  I1_k=cZERO
  I2_k=cZERO
  call M_by_M('n','n',SC_nbands,Ho_plus_Sigma(:,:,ik,1),G_input_k,I1_k)
  call M_by_M('n','n',SC_nbands,G_input_k,Ho_plus_Sigma(:,:,ik,1),I2_k)     
  !
  if(allocated(Sigma_relax))  I1_k = I1_k + Sigma_relax(:,:,ik)
  !
end subroutine build_I1_and_I2

subroutine EULER_step(G_new_k,G_old_k,I1_k,I2_k,deltaT)
 !
 use pars,           ONLY:SP,cI
 use wrapper,        ONLY:V_by_V_plus_V
 use SC,             ONLY:SC_bands,SC_nbands
 !
 implicit none
 !
 real(SP),    intent(in)  :: deltaT
 complex(SP), intent(in)  :: G_old_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(in)  :: I1_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(in)  :: I2_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(out) :: G_new_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 !
 ! G(T+dt) = G(T)
 !
 G_new_k=G_old_k
 !
 ! G(T+dt) = G(T) - i dt *(I1(T) - I2(T))
 !
 call V_by_V_plus_V(SC_nbands, -cI*deltaT, I1_k-I2_k, G_new_k)
 !
end subroutine EULER_step   
