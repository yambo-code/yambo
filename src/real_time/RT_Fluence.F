!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine RT_Fluence()
 !
 use pars,           ONLY:SP,pi
 use com,            ONLY:msg
 use units,          ONLY:SPEED_OF_LIGHT,AU2KWCMm2,FS2AUT
 use real_time,      ONLY:RT_step,NE_steps,Fluence
 use fields,         ONLY:Efield,small_a,theta_function,ef_i_time,i_Pump
 use LIVE_t,         ONLY:live_timing
 !
 implicit none
 ! 
 ! Work Space
 !
 integer      :: it
 real(SP)     :: I(NE_steps),T(NE_steps)
 complex(SP)  :: Field_time,E(NE_steps)
 real(SP),external :: RIntegrate
 !
 ! E(t)=-d A(t)/dt / SPEED_OF_LIGHT
 !
 ! From RT_Ext_fields we see that
 !
 ! A'(t)=-Efield(i_Pump)%amplitude*Efield(i_Pump)%versor(:)*SPEED_OF_LIGHT * (small_a' theta + small_a theta')
 !
 ! Thus
 ! 
 ! |E(t)|^2= Efield(i_Pump)%amplitude**2 * | small_a' theta + small_a theta' |^2
 !
 I=0._SP
 !
 return
 !
 call live_timing('Fluence:',NE_steps-ef_i_time(i_Pump))
 !
 do it=ef_i_time(i_Pump)+1,NE_steps
   !
   T(it)=it*RT_step
   !
   Field_time=cmplx(T(it)-ef_i_time(i_Pump)*RT_step,0._SP)
   !
   ! I(t) = \frac{c}{8\pi} E^2(t)
   !
   E(it)=Efield(i_Pump)%amplitude*&
&        small_a(Field_time,Efield(i_Pump),1)*theta_function(real(Field_time),RT_step,0)+&
&        small_a(Field_time,Efield(i_Pump),0)*theta_function(real(Field_time),RT_step,1)
   !
   I(it)=SPEED_OF_LIGHT/8./pi*abs(E(it))**2.
   !
   !
   ! From au to kW/cm^2
   !
   I(it)=I(it)*AU2KWCMm2
   ! 
   ! To get the fluence in kJ/cm^2 I should translate T in secs. However to get 
   ! the integration more stable I multiply I by 10^-15
   !
   T(it)=T(it)/FS2AUT ! au -> fs
   I(it)=I(it)*1.E-15_SP
   !
   Fluence(it)=RIntegrate(I(:it),T(:it),it)
   !
   if(it<1001) cycle
   if(abs((Fluence(it)-Fluence(it-1000))/Fluence(it))<1.E-5) exit
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 if ( it< (NE_steps+1) ) it=it-1000
 if ( it==(NE_steps+1) ) it=it-1
 !
 call msg('l','done ==')
 !
 call msg('r','[RT] Max Fluence           [kJ/cm^2]:',Fluence(it))
 call msg('r','     reached at time            [fs]:',T(it))
 !
 if(it<NE_steps) Fluence(it+1:)=Fluence(it)
 !
end subroutine RT_Fluence
