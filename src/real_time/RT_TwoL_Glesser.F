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
 use SC,             ONLY:it_now
 use fields,         ONLY:Efield,global_gauge,i_Probe,i_Pump
 !
! use real_time,      ONLY:P,W_pump
! use stderr,         ONLY:real2ch
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
 integer           :: i_f,i_found(2),i_r
 real(SP)          :: W_step,Eo_,W_pump
 complex(SP)       :: Poles_func(N_W_delta_search),Poles_func_deriv(N_W_delta_search),W_func(N_W_delta_search),&
&                     Gamma_mat,dGinfty,R_gamma,R_wbar,G_lesser_prefactor,&
&                     W_bar,Beta_wbar,W_gamma,Beta_gamma
 complex(SP)       :: dG_residuals(4),G_residuals(4),Omega(4)
 !
 G_lesser=(0.,0.)
 !
 if (trim(global_gauge)=="length".or.i_Probe/=i_Pump) return
 !
 Eo_=Eo
 W_pump=0.
 G_lesser_prefactor=(1.,0.)
 !
 select case(trim(Efield(i_Probe)%ef_name))
   case('RES')
     W_pump=Efield(i_Probe)%frequency(1)
     Eo_=Eo+W_pump
     G_lesser_prefactor=exp(-cI*W_pump*NE_time)
   case('ANTIRES')
     W_pump=Efield(i_Probe)%frequency(1)
     Eo_=Eo-W_pump
     G_lesser_prefactor=exp( cI*W_pump*NE_time)
 end select
 !
 W_step=10.*Eo/real(N_W_delta_search)
 !
 if (NE_i_time==1.or.it_now==it_start) then
   !
1  i_found=0
   !
   do i_f=1,N_W_delta_search-1
     W_func(i_f)=-0.1/HARTREE+W_step*i_f
     Poles_func(i_f)=DELTA_Pump_F(W_func(i_f))
     Poles_func_deriv(i_f)=(DELTA_Pump_F(W_func(i_f)+W_step)-DELTA_Pump_F(W_func(i_f)))/W_step
     !
     if (i_f==1) cycle
     !
     if ( (real(Poles_func(i_f-1))<0..and.real(Poles_func(i_f))>0. ) ) then
       W_bar    = W_func(i_f-1) - cI*aimag(Poles_func(i_f-1))/Poles_func_deriv(i_f-1)
       Beta_wbar= Poles_func_deriv(i_f-1)
       i_found(1)=1
     endif
     !
     if ( (real(W_func(i_f-1))<0..and.real(W_func(i_f))>0. ) ) then
       W_gamma   = W_func(i_f-1) - cI*aimag(Poles_func(i_f-1))/Poles_func_deriv(i_f-1)
       Beta_gamma= Poles_func_deriv(i_f-1)
       i_found(2)=1 
     endif
     !
   enddo
   !
   if (.not.all(i_found==1)) then 
     if (W_step>10.*Eo_/real(N_W_delta_search)) then
       Poles_and_derivatives(1)=-1.
       i_found=1
       goto 2
     endif
     W_step=2*W_step
     goto 1
   else
     !
     Poles_and_derivatives=(/W_bar,Beta_wbar,W_gamma,Beta_gamma/)
     !
   endif
   !
2  continue
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
 if (NE_i_time==1.or.W_bar==-1.) return
 !
 Gamma_mat=abs(Rabi)**2/(Eo_**2.+1./Phase_LifeTime**2.)*QP_LifeTime/Phase_LifeTime
 dGinfty=-2.*cI/(Gamma_mat+1.)
 R_wbar=2*abs(Rabi)**2/W_bar/Beta_wbar*( W_bar+cI/Phase_LifeTime )
 R_gamma=2*abs(Rabi)**2/W_gamma/Beta_gamma*( W_gamma+cI/Phase_LifeTime )
 !
 dG_residuals=(/ cI*dGinfty, R_wbar, conjg(R_wbar), R_gamma/)
 Omega=       (/ (0._SP,0._SP)   , -W_bar, conjg(W_bar), -W_gamma/)
 !
 do i_r=1,4
   G_residuals(i_r)=dG_residuals(i_r)/(Omega(i_r)-Eo_-cI/Phase_LifeTime)
 enddo
 !
 G_lesser=(0.,0.)
 !
 do i_r=1,4
   G_lesser=G_lesser+G_lesser_prefactor*cI*Rabi/2.*G_residuals(i_r)*&
&           ( exp(cI*Omega(i_r)*NE_time) - exp(cI*(Eo_+cI/Phase_Lifetime)*NE_time))
 enddo
 !
 contains
   !
   complex(SP) function DELTA_Pump_F(W)
     complex(SP) :: W
     DELTA_Pump_F=(W+cI/QP_LifeTime)*(W+Eo_+cI/Phase_LifeTime)* & 
&                 (W-Eo_+cI/Phase_LifeTime)-(W+cI/Phase_LifeTime)*abs(Rabi)**2.
   end function
   !
end subroutine RT_TwoL_Glesser
