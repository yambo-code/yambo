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
subroutine RT_Fluence(i_field)
 !
 use com,            ONLY:msg
 use pars,           ONLY:SP,pi,schlen
 use units,          ONLY:SPEED_OF_LIGHT,AU2KWCMm2,FS2AUT
 use real_time,      ONLY:RT_step,NE_steps
 use fields,         ONLY:Efield,small_a,theta_function,i_Pump
 !
 implicit none
 !
 integer, intent(in) :: i_field
 ! 
 ! Work Space
 !
 integer             :: it
 character(schlen)   :: string
 real(SP)            :: Intensity(NE_steps),Time(NE_steps),TMP_Fluence(NE_steps)
 complex(SP)         :: Field_time,E_m2(NE_steps)
 !
 real(SP),external   :: RIntegrate
 !
 ! E(t)=-d A(t)/dt / SPEED_OF_LIGHT
 !
 ! From RT_Ext_fields:
 ! A'(t)=-Efield(i_field)%amplitude*Efield(i_field)%versor(:)*SPEED_OF_LIGHT * (small_a' theta + small_a theta')
 !
 ! Thus:
 ! |E(t)|^2= Efield(i_field)%amplitude**2 * | small_a' theta + small_a theta' |^2
 !
 if ( Efield(i_field)%intensity<1.E-7_SP ) then
   Efield(i_field)%t_final=Efield(i_field)%t_initial
   Efield(i_field)%t_final_indx=Efield(i_field)%t_initial_indx
   allocate(Efield(i_field)%Fluence(1) )
   Efield(i_field)%Fluence=0._SP
   return
 endif
 !
 Intensity=0._SP
 !
 do it=Efield(i_field)%t_initial_indx,NE_steps
   !
   Time(it)=it*RT_step
   !
   Field_time=cmplx(Time(it)-Efield(i_field)%t_initial,0._SP)
   !
   E_m2(it)=Efield(i_field)%amplitude*&
&        small_a(Field_time,Efield(i_field),1)*theta_function(real(Field_time),RT_step,0)+&
&        small_a(Field_time,Efield(i_field),0)*theta_function(real(Field_time),RT_step,1)
   !
   E_m2(it)=abs(E_m2(it))**2._SP
   !
   ! Intensity(t) = \frac{c}{8\pi} E^2(t)
   !
   Intensity(it)=SPEED_OF_LIGHT/8./pi*E_m2(it)
   !
   Time(it)=Time(it)/FS2AUT               ! au -> fs
   Intensity(it)=Intensity(it)*AU2KWCMm2  ! au -> kW/cm^2
   ! 
   ! To get the fluence in kJ/cm^2 I should express Time in secs.
   ! However for a more stable integration I multiply Intensity by 10^-15
   !
   Intensity(it)=Intensity(it)*1.E-15_SP
   !
   TMP_Fluence(it)=RIntegrate(Intensity(:it),Time(:it),it)
   !
   if(it<1001) cycle
   if(abs((TMP_Fluence(it)-TMP_Fluence(it-1000))/TMP_Fluence(it))<1.E-5) exit
   !
 enddo
 !
 if ( it< (NE_steps+1) ) it=it-1000
 if ( it==(NE_steps+1) ) it=it-1
 !
 Efield(i_field)%t_final_indx=it
 Efield(i_field)%t_final=(Efield(i_field)%t_final_indx-1)*RT_step
 !
 allocate(Efield(i_field)%Fluence( Efield(i_field)%t_final_indx ) )
 Efield(i_field)%Fluence(:Efield(i_field)%t_final_indx)=&
&            TMP_Fluence(:Efield(i_field)%t_final_indx)
 !
 if(     i_field==i_Pump) string='Total Fluence (Pump)  '
 if(.not.i_field==i_Pump) string='Total Fluence (Probe) '
 !
 call msg('r','[RT] ' // trim(string) // '[kJ/cm^2]:',Efield(i_field)%Fluence(Efield(i_field)%t_final_indx) )
 call msg('r','     reached at time            [fs]:',Time( Efield(i_field)%t_final_indx ) )
 !
end subroutine RT_Fluence
