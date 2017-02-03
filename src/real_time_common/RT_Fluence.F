!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_Fluence(i_field)
 !
 use pars,           ONLY:SP,pi
 use units,          ONLY:SPEED_OF_LIGHT,AU2KWCMm2,PS2AUT
 use real_time,      ONLY:RT_step,NE_steps
 use fields,         ONLY:Efield,small_a
 use functions,      ONLY:theta_function
 !
 implicit none
 !
 integer      :: i_field
 ! 
 ! Work Space
 !
 integer      :: it0,it,last_it_evaluated
 real(SP)     :: I(NE_steps),T(NE_steps),Field_time,E_coeff
 complex(SP)  :: E(3,NE_steps),A_of_t(2),E_of_t(2),theta,delta
 real(SP),external :: RIntegrate
 !
 ! E(t)=-d A(t)/dt / SPEED_OF_LIGHT
 !
 ! From RT_Ext_fields we see that
 !
 ! A'(t)=-Efield(i_field)%amplitude*Efield(i_field)%versor(:)*SPEED_OF_LIGHT * (small_a' theta + small_a theta')
 !
 ! Thus
 ! 
 ! |E(t)|^2= Efield(i_field)%amplitude**2 * | small_a' theta + small_a theta' |^2
 !
 if ( Efield(i_field)%intensity<1.E-15_SP ) then
   Efield(i_field)%t_final=Efield(i_field)%t_initial
   Efield(i_field)%t_final_indx=Efield(i_field)%t_initial_indx
   Efield(i_field)%fluence=0._SP
   return
 endif
 !
 I=0._SP
 T=0._SP
 last_it_evaluated=0
 !
 Efield(i_field)%fluence=0._SP
 !
 it0=Efield(i_field)%t_initial_indx
 !
 do it=it0,NE_steps
   !
   T(it)=it*RT_step
   !
   if ((it-it0)>6*nint(Efield(i_field)%width/RT_step)) exit
   !
   Field_time=T(it)-Efield(i_field)%t_initial
   !
   A_of_t=small_a(Field_time,Efield(i_field),0)
   E_of_t=small_a(Field_time,Efield(i_field),1)
   !
   theta=theta_function(Field_time,RT_step,0)
   delta=theta_function(Field_time,RT_step,1)
   !
   select case(Efield(i_field)%ef_pol)
   case("linear")
     E_coeff=Efield(i_field)%amplitude
     E(:,it)=E_coeff*Efield(i_field)%versor*( E_of_t(1)*theta+A_of_t(1)*delta )
   case('circular')
     E_coeff=Efield(i_field)%amplitude/sqrt(2._SP)
     E(:,it)=E_coeff*Efield(i_field)%versor*( E_of_t(1)*theta+A_of_t(1)*delta )                 &
&           +E_coeff*Efield(i_field)%versor_circ*( E_of_t(2)*theta+A_of_t(2)*delta )
   end select
   !
   I(it)=(abs(E(1,it))**2+abs(E(2,it))**2+abs(E(3,it))**2)
   !
   last_it_evaluated=it
   !
 enddo
 !
 ! I(t) = \frac{c}{8\pi} E^2(t)
 ! From au to kW/cm^2
 !
 I(:)=I(:)*SPEED_OF_LIGHT/4._SP/pi*AU2KWCMm2
 ! 
 ! To get the fluence in kJ/cm^2 I should translate T in sec.
 ! Then I would have a factor 10^-12 ( 1 nJ/cm^2 = 10^-12 kJ/cm^2 )
 ! thus I ruther convert time in ps
 !
 T(:)=T(:)/PS2AUT      ! au -> ps
 !
 Efield(i_field)%fluence=RIntegrate(I(it0:last_it_evaluated),T(it0:last_it_evaluated),last_it_evaluated-it0+1)
 !
 Efield(i_field)%t_final_indx=last_it_evaluated
 Efield(i_field)%t_final=(Efield(i_field)%t_final_indx-1)*RT_step
 !
end subroutine RT_Fluence
