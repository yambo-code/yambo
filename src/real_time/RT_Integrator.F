!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
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
!
recursive subroutine RT_Integrator(G_new,G_old,A_new,A_old,T_step,Xen,k,q,Integrator,RT_time)
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
 ! elseif(Integrator=="EXACT")    Exact Integration
 !
 ! It diagonalizes the Hamiltonian and integrates in the 
 ! eigenstate space
 !
 ! elseif(Integrator=="RK2EXACT") 
 ! 
 ! Runge-Kutta with Exact Integration instead of the Euler step
 ! The full Hamiltonian is diagonalized Ho + Sigma_s
 ! and the G_lesser is integrated in this diagonal bases
 !
 !
 ! Notice that G_old is never overwritten       
 ! ------------------------------------------------------------
 !
 use pars,           ONLY:SP,schlen
 use com,            ONLY:error
 use SC,             ONLY:SC_bands,H_rotate
 use R_lattice,      ONLY:bz_samp  
 use real_time,      ONLY:I1_lesser,I2_lesser,H_t,EULER,RK2,EXACT,RK2EXACT, &
&                         i_time,l_NE_with_fields
 use fields,         ONLY:gauge_field
 use electrons,      ONLY:levels
 use matrix_operate, ONLY:mat_dia_inv,USE_LK,DIAGO
 implicit none
 !
 character(schlen),    intent(in) :: Integrator
 type(bz_samp),        intent(in) :: k,q
 type(levels),         intent(in) :: Xen
 real(SP),             intent(in) :: T_step,RT_time
 complex(SP),          intent(in) :: G_old(SC_bands,SC_bands,k%nibz)
 complex(SP),       intent(inout) :: G_new(SC_bands,SC_bands,k%nibz)
 type(gauge_field),    intent(in) :: A_old
 type(gauge_field), intent(inout) :: A_new
 ! 
 ! Work Space
 !
 complex(SP) :: TmpMatrix(SC_bands,SC_bands)
 integer     :: ik,i1,i2,SC_bands2_dot_nibz
 !
 SC_bands2_dot_nibz=SC_bands**2*k%nibz
 !
 if(trim(Integrator)==EULER) then
   !
   ! Gf_{n,m}(T+dt) = G_{n,m}(T) 
   !
   G_new=G_old
   !
   ! G(T+dt) = G(T) - i dt *(I1(T) - I2(T))
   !
#if defined _DOUBLE
   call zaxpy(SC_bands2_dot_nibz,dcmplx(0._SP,-T_step),I1_lesser,1,G_new,1)
   call zaxpy(SC_bands2_dot_nibz,dcmplx(0._SP,+T_step),I2_lesser,1,G_new,1)
#else
   call caxpy(SC_bands2_dot_nibz,cmplx(0._SP,-T_step),I1_lesser,1,G_new,1)
   call caxpy(SC_bands2_dot_nibz,cmplx(0._SP,+T_step),I2_lesser,1,G_new,1)
#endif
   !
   if(l_NE_with_fields) call RT_propagate_vecpot(A_new,A_old,T_step,RT_time)
   !
 elseif(trim(Integrator)==RK2) then
   !
   ! Evaluate G(t+dt/2)
   !
   call RT_Integrator(G_new,G_old,A_new,A_old,T_step/2._SP,Xen,k,q,EULER,RT_time) 
   !
   ! use G(t+dt/2) to evaluate I1(t+dt/2) and I2(t+dt/2)
   !
   call RT_Collision(G_new,A_new,Xen,k,q,RT_time+T_step/2._SP)
   if(l_NE_with_fields.and.i_time>0)  call RT_current_AND_polarization(k,G_new,A_new)
   !
   ! final step G(t+dt)=G(t) + dt * (I1(t+dt/2) - I2(t+dt/2))
   !
   call RT_Integrator(G_new,G_old,A_new,A_old,T_step,Xen,k,q,EULER,RT_time)
   !
 elseif(trim(Integrator)==EXACT) then
   !
   ! Diagonalize the Self-Energy (H_t) and make a step 
   ! in the space of its eigenstates
   !
   G_new=G_old
   !
   do ik=1,Xen%nk
     !
     TmpMatrix(1:SC_bands,1:SC_bands)=H_t(1:SC_bands,1:SC_bands,ik,1)
     !
     call mat_dia_inv(DIAGO,USE_LK,TmpMatrix,E_real=Xen%E(:SC_bands,ik,1))
     !
     call H_rotate(TmpMatrix,G_new(1:SC_bands,1:SC_bands,ik),SC_bands,1)
     !
     forall(i1=1:SC_bands,i2=1:SC_bands)
       G_new(i1,i2,ik)=G_new(i1,i2,ik)*exp(-(0._SP,1._SP)*(Xen%E(i1,ik,1)-Xen%E(i2,ik,1))*T_step)
     end forall
     !
     call H_rotate(TmpMatrix,G_new(1:SC_bands,1:SC_bands,ik),SC_bands,-1)
     !
     ! Symmetrization
     !
     do i1=1,SC_bands
       G_new(i1,i1,ik)=(0._SP,1._SP)*aimag(G_new(i1,i1,ik))
       !
       do i2=i1+1,SC_bands
         G_new(i1,i2,ik)=(G_new(i1,i2,ik)-conjg(G_new(i2,i1,ik)))/2._SP
         G_new(i2,i1,ik)=-conjg(G_new(i1,i2,ik))
       enddo
       !
     enddo
     !
   enddo
   !
   if(l_NE_with_fields) call RT_propagate_vecpot(A_new,A_old,T_step,RT_time)
   !
 elseif(trim(Integrator)==RK2EXACT) then
   !
   ! Evaluate G(t+dt/2)
   !
   call RT_Integrator(G_new,G_old,A_new,A_old,T_step/2._SP,Xen,k,q,EXACT,RT_time) 
   !
   ! use G(t+dt/2) to evaluate I1(t+dt/2) and I2(t+dt/2)
   !
   call RT_Collision(G_new,A_new,Xen,k,q,RT_time+T_step/2._SP)
   if(l_NE_with_fields.and.i_time>0)  call RT_current_AND_polarization(k,G_new,A_new)
   !
   ! final step G(t+dt)=G(t) + dt * (I1(t+dt/2) - I2(t+dt/2))
   !
   call RT_Integrator(G_new,G_old,A_new,A_old,T_step,Xen,k,q,EXACT,RT_time)
   !                                                        
 else
   !
   call error(" Unknown Integrator!")
   !
 endif   
 !
end subroutine RT_Integrator

