!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine RT_TwoL_Glesser(Poles_and_derivatives,Eo,Rabi,G_lesser)
 !
 use pars,           ONLY:SP,cI
 use units,          ONLY:HARTREE
 use real_time,      ONLY:NE_time,Phase_Lifetime,QP_LifeTime,it_start,NE_i_time
 use fields,         ONLY:global_gauge
 use SC,             ONLY:it_now
 !
 implicit none
 real(SP),    intent(in)   :: Eo
 complex(SP), intent(in)   :: Rabi
 complex(SP), intent(out)  :: G_lesser
 complex(SP), intent(inout):: Poles_and_derivatives(4)
 ! 
 ! Work Space
 !
 integer,parameter :: N_W_delta_search=100000
 integer           :: i_f,i_found(2)
 real(SP)          :: W_step
 complex(SP)       :: Poles_func(N_W_delta_search),Poles_func_deriv(N_W_delta_search),W_func(N_W_delta_search),&
&                     Gamma_mat,dGinfty,So,S_wo,S_wbar,S_minus_wbar_star,S_gamma,R_gamma,R_wbar,&
&                     W_bar,Beta_wbar,W_gamma,Beta_gamma
 !
 G_lesser=(0.,0.)
 !
 if (NE_i_time==1.or.it_now==it_start) then
   !
   i_found=0
   !
   W_step=2.*Eo/real(N_W_delta_search)
   !
   do i_f=1,N_W_delta_search-1
     W_func(i_f)=-0.1/HARTREE+W_step*i_f
     Poles_func(i_f)=DELTA_Pump_F(W_func(i_f))
     Poles_func_deriv(i_f)=(DELTA_Pump_F(W_func(i_f)+W_step)-DELTA_Pump_F(W_func(i_f)))/W_step
     if (i_f==1) cycle
     if ( (real(Poles_func(i_f-1))<0..and.real(Poles_func(i_f))>0. ) ) then
       W_bar    = W_func(i_f-1) - cI*aimag(Poles_func(i_f-1))/Poles_func_deriv(i_f-1)
       Beta_wbar= Poles_func_deriv(i_f-1)
       i_found(1)=1
     endif
     if ( (real(W_func(i_f-1))<0..and.real(W_func(i_f))>0. ) ) then
       W_gamma   = W_func(i_f-1) - cI*aimag(Poles_func(i_f-1))/Poles_func_deriv(i_f-1)
       Beta_gamma= Poles_func_deriv(i_f-1)
       i_found(2)=1 
     endif
   enddo
   !
   if (.not.all(i_found==1)) then 
!     do i_f=1,N_W_delta_search-1
!       write (100,'(4f20.10)') real(W_func(i_f))*HARTREE,Poles_func(i_f)
!     enddo
!     write (*,*) i_found,Eo*HARTREE,W_func(N_W_delta_search-1)*HARTREE
!     stop
   endif
   !
   Poles_and_derivatives=(/W_bar,Beta_wbar,W_gamma,Beta_gamma/)
   !
 else
   !
   W_bar=Poles_and_derivatives(1)
   Beta_wbar=Poles_and_derivatives(2)
   W_gamma=Poles_and_derivatives(3)
   Beta_gamma=Poles_and_derivatives(4)
   !
 endif
 !
 if (NE_i_time==1) return
 !
 Gamma_mat=abs(Rabi)**2/(Eo**2.+1./Phase_LifeTime**2.)*QP_LifeTime/Phase_LifeTime
 dGinfty=-2.*cI/(Gamma_mat+1.)
 !
 R_wbar=2*abs(Rabi)**2/W_bar/Beta_wbar*( W_bar+cI/Phase_LifeTime )
 R_gamma=2*abs(Rabi)**2/W_gamma/Beta_gamma*( W_gamma+cI/Phase_LifeTime )
 !
 So  =cI*dGinfty/(Eo+cI/Phase_LifeTime)
 S_wo=R_wbar/(W_bar+Eo+cI/Phase_LifeTime)
 S_wbar=R_wbar/(W_bar+Eo+cI/Phase_LifeTime)
 S_minus_wbar_star=conjg(R_wbar)/(-conjg(W_bar)+Eo+cI/Phase_LifeTime)
 S_gamma=R_gamma/(Eo+W_gamma+cI/Phase_LifeTime)
 !
 G_lesser=exp_by_sigma_12(NE_time)-exp_by_sigma_12(0.)*exp(cI*Eo*NE_time-NE_time/Phase_LifeTime)
 !
 !G_lesser=-cI*Rabi/(Eo+cI/Phase_LifeTime)*(1.-exp(cI*(Eo+cI/Phase_Lifetime)*NE_time))
 !if (trim(global_gauge)=="length") G_lesser=cI*Rabi/Eo*exp(cI*(Eo+cI/Phase_Lifetime)*NE_time)
 !
 contains
   !
   complex(SP) function DELTA_Pump_F(W)
     complex(SP) :: W
     DELTA_Pump_F=(W+cI/QP_LifeTime)*(W+Eo+cI/Phase_LifeTime)* & 
&                 (W-Eo+cI/Phase_LifeTime)-(W+cI/Phase_LifeTime)*abs(Rabi)**2.
   end function
   !
   complex(SP) function exp_by_sigma_12(Time)
    real(SP)    :: Time
    exp_by_sigma_12=-cI*Rabi/2.*(&
&       So&
&      +S_gamma*exp( -cI*W_gamma*Time)&
&      +S_wbar*exp( -cI*W_bar*Time)&
&      +S_minus_wbar_star*exp( cI*conjg(W_bar)*Time)&
&       )
   end function
   !
end subroutine RT_TwoL_Glesser
