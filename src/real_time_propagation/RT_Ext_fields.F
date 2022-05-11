!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
subroutine RT_Ext_fields(A,time,i_f)
 !
 ! The external and total fields in Yambo are defined and organized as follow:
 !
 ! The general vector potential is defined as
 !
 !   A(t)=-c E(t) theta(t-to) [1]
 !  
 ! the function a(t) and theta(t) are defined in mod_fields. From [1] the first and
 ! second order  derivatives are easily defined:
 !
 !   A'(t)=-c (E'(t) theta(t-to)+ E(t) delta(t-to)) [2]
 !   A"(t)=-c (E"(t) theta(t-to)+ E'(t) delta(t-to) - E"(t) sign(t)) [3]
 !
 ! Using [1-3] yambo :
 !
 ! a. calculates the analytical form of A/A'/A" in RT_Ext_fields 
 ! b. uses A" (the ext current) to evolve the Maxwell equations when
 !    the induced current is included in the dynamics ( RT_propagate_fields )
 !
 use pars,           ONLY:SP,cZERO
 use units,          ONLY:SPEED_OF_LIGHT
 use real_time,      ONLY:RT_dyn_step,NE_i_time
 use functions,      ONLY:theta_function
 use fields,         ONLY:Efield,small_a,gauge_field,compute_intensity,compute_envelop,A_vecpot_reset
 !
 implicit none
 !
 type(gauge_field),intent(inout) :: A
 real(SP),         intent(in)    :: time
 integer,          intent(in)    :: i_f
 ! 
 ! Work Space
 !
 integer     :: ikind
 real(SP)    :: Field_time,A_coeff
 complex(SP) :: A_of_t(2),E_of_t(2),J_of_t(2),theta,delta,signf
 !
 Field_time=time-Efield(i_f)%t_initial
 !
 if (Efield(i_f)%ef_name=="none" .or. Efield(i_f)%t_initial_indx>(NE_i_time+2) ) then
   call A_vecpot_reset(A)
   return
 endif
 !
 theta=theta_function(Field_time,RT_dyn_step,0)
 delta=theta_function(Field_time,RT_dyn_step,1)
 signf=theta_function(Field_time,RT_dyn_step,2)
 !
 do ikind=1,2
   !
   A_of_t=small_a(Field_time,Efield(i_f),0,envelop_only=(ikind==1))
   E_of_t=small_a(Field_time,Efield(i_f),1,envelop_only=(ikind==1))
   J_of_t=small_a(Field_time,Efield(i_f),2,envelop_only=(ikind==1))
   !
   select case(Efield(i_f)%ef_pol)
   case("linear")
     A_coeff=SPEED_OF_LIGHT*Efield(i_f)%amplitude
     A%vecpot(:)    =-A_coeff*Efield(i_f)%versor*( A_of_t(1)*theta)
     A%vecpot_vel(:)=-A_coeff*Efield(i_f)%versor*( E_of_t(1)*theta+A_of_t(1)*delta )
     A%vecpot_acc(:)=-A_coeff*Efield(i_f)%versor*( J_of_t(1)*theta+E_of_t(1)*delta+A_of_t(1)*signf)
   case('circular')
     A_coeff=SPEED_OF_LIGHT*Efield(i_f)%amplitude/sqrt(2._SP)
     A%vecpot(:)    =-A_coeff*Efield(i_f)%versor     *( A_of_t(1)*theta)                                  &
&                     -A_coeff*Efield(i_f)%versor_circ*( A_of_t(2)*theta)
     A%vecpot_vel(:)=-A_coeff*Efield(i_f)%versor     *( E_of_t(1)*theta+A_of_t(1)*delta )                 &
&                     -A_coeff*Efield(i_f)%versor_circ*( E_of_t(2)*theta+A_of_t(2)*delta )
     A%vecpot_acc(:)=-A_coeff*Efield(i_f)%versor     *( J_of_t(1)*theta+E_of_t(1)*delta+A_of_t(1)*signf)  &
&                     -A_coeff*Efield(i_f)%versor_circ*( J_of_t(2)*theta+E_of_t(2)*delta+A_of_t(2)*signf)
   case default
     call error("Unknown external field polarization! ")
   end select
   !
   if(ikind==1) call compute_envelop(A)
   if(ikind==2) call compute_intensity(A)
   !
 enddo
 !
end subroutine RT_Ext_fields
!
!
subroutine RT_Ext_fields_sum(A,time)
 !
 use pars,           ONLY:SP,cZERO,rZERO
 use fields,         ONLY:n_ext_fields,gauge_field
 !
 implicit none
 !
 type(gauge_field),intent(out)  :: A
 real(SP),         intent(in)   :: time
 !
 ! Work space
 !
 type(gauge_field) :: A_tmp
 integer           :: i_f
 !
 A%vecpot    =cZERO
 A%vecpot_vel=cZERO
 A%vecpot_acc=cZERO
 A%envelop   =rZERO
 A%intensity =rZERO
 !
 do i_f=1,n_ext_fields
   call RT_Ext_fields(A_tmp,time,i_f)
   A%vecpot    =A%vecpot    +A_tmp%vecpot
   A%vecpot_vel=A%vecpot_vel+A_tmp%vecpot_vel
   A%vecpot_acc=A%vecpot_acc+A_tmp%vecpot_acc
   A%envelop   =A%envelop   +A_tmp%envelop
   A%intensity =A%intensity +A_tmp%intensity
 enddo
 !
end subroutine RT_Ext_fields_sum
