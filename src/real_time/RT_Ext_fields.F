!
! Copyright (C) 2000-2011 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Ext_fields(A,time)
 !
 ! The external and total fields in Yambo are defined and organized as follow:
 !
 ! The general vector potential is defined as
 !
 !   A(t)=-c Eo a(t) theta(t-to) [1]
 !  
 ! the function a(t) and theta(t) are defined in mod_fields. From [1] the first and
 ! second order  derivatives are easily defined:
 !
 !   A'(t)=-c Eo ( a'(t) theta(t-to)+ a(0) delta(t-to)) [2]
 !   A"(t)=-c Eo ( a"(t) theta(t-to)+ a'(0) delta(t-to) - a"(0) sign(t)) [3]
 !
 ! Using [1-3] yambo :
 !
 ! a. calculates the analytical form of A/A'/A" in RT_Ext_fields 
 ! b. uses A" (the ext current) to evolve the Maxwell equations when
 !    the induced current is included in the dynamics ( RT_propagate_fields )
 !
 use pars,           ONLY:SP
 use units,          ONLY:SPEED_OF_LIGHT
 use real_time,      ONLY:RT_step,NE_i_time
 use fields,         ONLY:n_ext_fields,Efield,small_a,theta_function,ef_i_time,gauge_field
 !
 implicit none
 type(gauge_field),intent(inout) :: A
 real(SP),         intent(in)    :: time
 ! 
 ! Work Space
 !
 integer     :: i_f
 complex(SP) :: Field_time
 complex(SP) :: E_times_c(3)
 !
 A%vecpot    =(0._SP,0._SP)
 A%vecpot_vel=(0._SP,0._SP)
 A%vecpot_acc=(0._SP,0._SP)
 !
 do i_f=1,n_ext_fields
   !
   if(Efield(i_f)%ef_name=="none".or.ef_i_time(i_f)> NE_i_time.or.NE_i_time<1) cycle
   !
   E_times_c=Efield(i_f)%amplitude*Efield(i_f)%versor(:)*SPEED_OF_LIGHT
   !
   Field_time=cmplx(time-ef_i_time(i_f)*RT_step,0._SP)
   !
   A%vecpot(:)=A%vecpot(:)-E_times_c(:)*&
&                     small_a(Field_time,Efield(i_f),0)*theta_function(real(Field_time),RT_step,0)
   !
   A%vecpot_vel(:)=A%vecpot_vel(:)-E_times_c(:)*&
&                         (small_a(Field_time,Efield(i_f),1)*theta_function(real(Field_time),RT_step,0)+&
&                          small_a(Field_time,Efield(i_f),0)*theta_function(real(Field_time),RT_step,1))
   !
   A%vecpot_acc(:)=A%vecpot_vel(:)-E_times_c(:)*&
&                         (small_a(Field_time,Efield(i_f),2)*theta_function(real(Field_time),RT_step,0)+&
&                          small_a(Field_time,Efield(i_f),1)*theta_function(real(Field_time),RT_step,1)+&
&                          small_a(Field_time,Efield(i_f),0)*theta_function(real(Field_time),RT_step,2))
   !
 enddo
 !
end subroutine RT_Ext_fields
