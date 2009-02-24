!
! Copyright (C) 2000-2008 A. Marini, C. Attaccalite and the YAMBO team 
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
!
!
!  This subroutine perform integration using different methods
!
! if(Integrator_name=="EULER") simple Euler's method
!
!    y(t+dt) = y(t) + dt * f(t,y(t))
!
! if(Integrator_name=="RK2")  Runge-Kutta 2nd order
!
!    y(t+dt) = y(y) + dt * f(t+dt/2,y(t+dt/2))
!
! Notice that G_old is never overwritten
!
recursive subroutine RT_Integrator(G_new,G_old,T_step,en,k,int_name,RT_time)
 !
 use pars,           ONLY:SP,schlen
 use SC,             ONLY:SC_potential,SC_bands
 use R_lattice,      ONLY:bz_samp  
 use real_time,      ONLY:G_lesser,G_l_old,Go_Phases,I1_lesser,I2_lesser,RT_step&
&                        ,n_poles,Integrator_Name,RT_step,EULER,RK2,RT_no_phase_int
 use electrons,      ONLY:levels
 use X_m,            ONLY:X_poles,X_poles_tab 
 implicit none
 character(schlen), intent(in) :: int_name
 type(bz_samp),     intent(in) :: k
 type(levels),      intent(in) :: en
 real(SP),          intent(in) :: T_step,RT_time
 complex(SP),       intent(in) :: G_old(SC_bands,SC_bands,k%nibz)
 complex(SP),    intent(inout) :: G_new(SC_bands,SC_bands,k%nibz)
 ! 
 ! Work Space
 !
 complex(SP) :: P_Factor(SC_bands,SC_bands),Tmp_Factor(SC_bands,SC_bands)
 integer     :: ik,i1,i2,i,j,i_pole,iv,ic
 !
 if(trim(int_name)==EULER) then
   !
   if(RT_no_phase_int) then
     !      
     ! Gf_{n,m}(T+dt) = G_{n,m}(T) 
     !
     G_new(:,:,:)=G_old(:,:,:)
     !
     ! G(T+dt) = G(T+dt) - i dt *(I1(T) - I2(T))
     !
     G_new(:,:,:)=G_new(:,:,:)-(0._SP,1._SP)*T_step*(I1_lesser(:,:,:)-I2_lesser(:,:,:))
     !                                                            
   else
     !
     do ik=1,k%nibz
       !      
#if defined _DOUBLE
       P_Factor=(0._SP,0._SP)
       call zgerc(SC_bands,SC_bands,(1._SP,0._SP),Go_Phases(:,ik),1,Go_Phases(:,ik),1,P_Factor &
               &,SC_bands)
#else
       P_Factor=(0._SP,0._SP)
       call cgerc(SC_bands,SC_bands,(1._SP,0._SP),Go_Phases(:,ik),1,Go_Phases(:,ik),1,P_Factor,&
               &SC_bands)
#endif
       !
       ! Gf_{n,m}(T+dt) = G_{n,m}(T) * exp(-dt(E_{n}-E_{n'}))
       !
       forall(i=1:SC_bands,j=1:SC_bands) 
           G_new(i,j,ik)=G_old(i,j,ik)*P_Factor(i,j)
       end forall
       !
       ! Put the correct limit for E_eh --> 0
       !
       Tmp_Factor(:,:)=P_Factor(:,:)
       P_Factor(:,:)  =(0._SP,1._SP)*T_step
       !
       do i_pole=1,n_poles
         !     
         if(X_poles_tab(i_pole,1)/=ik) cycle
           !
           iv=X_poles_tab(i_pole,2)
           ic=X_poles_tab(i_pole,3)
           P_factor(iv,ic)=(1.0-Tmp_Factor(iv,ic))/X_poles(i_pole)
           P_factor(ic,iv)=(1.0-Tmp_Factor(ic,iv))/(-X_poles(i_pole))
           !
       enddo
       !
       ! G(T+dt) = G(T+dt) - (1 - exp(-i dt (E_n-E_m))/(E_n-E_m))*(I1(T) - I2(T))
       !
       forall(i=1:SC_bands,j=1:SC_bands) 
         G_new(i,j,ik)=G_new(i,j,ik)-P_factor(i,j)*(I1_lesser(i,j,ik)-I2_lesser(i,j,ik))
       end forall
       !                                                            
     enddo
     !
   endif
   !
 elseif(trim(Integrator_name)==RK2) then
   !
   ! Evaluate G(t+dt/2)
   !
   call RT_Integrator(G_new,G_old,T_step/2._SP,en,k,EULER,RT_time) 
   !
   ! use G(t+dt/2) to evaluate I1(t+dt/2) and I2(t+dt/2)
   !
   call RT_Collision(G_new,en,k,RT_time+T_step/2._SP)
   !
   ! final step G(t+dt)=G(t) + dt * (I1(t+dt/2) - I2(t+dt/2))
   !
   call RT_Integrator(G_new,G_old,T_step,en,k,EULER,RT_time)
   !
 endif   
 !
end subroutine RT_Integrator

