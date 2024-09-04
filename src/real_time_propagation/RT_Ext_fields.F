!
! License-Identifier: GPL
!
! Copyright (C) 2011 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
subroutine RT_Ext_fields(A_in,time,i_f_in)
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
 ! Using [1-3] calculates the analytical form of A/A'/A".
 !
 ! If i_f_in>0 it calculates only that specific contribution otherwise they are all summed.
 !
 use pars,           ONLY:SP
 use units,          ONLY:SPEED_OF_LIGHT
 use real_time,      ONLY:RT_dyn_step,NE_i_time
 use functions,      ONLY:theta_function
 use fields,         ONLY:Efield,small_a,gauge_field,compute_intensity,&
&                         compute_envelop,A_vecpot_reset,n_ext_fields
 !
 implicit none
 !
 type(gauge_field),intent(inout) :: A_in
 real(SP),         intent(in)    :: time
 integer,          intent(in)    :: i_f_in
 ! 
 ! Work Space
 !
 integer           :: ikind,i_f,Nfields,idummy
 real(SP)          :: Field_time,A_coeff
 complex(SP)       :: A_of_t(2),E_of_t(2),J_of_t(2),theta,delta,signf
 type(gauge_field) :: A
 !
 Nfields=1
 if (i_f_in<0) Nfields=n_ext_fields
 call A_vecpot_reset(A_in)
 call A_vecpot_reset(A)
 !
 do idummy=1,Nfields
   !
   i_f=i_f_in
   if (i_f_in<0) i_f=idummy
   !
   Field_time=time-Efield(i_f)%t_initial
   !
   if (Efield(i_f)%ef_name=="none" .or. Efield(i_f)%t_initial_indx>(NE_i_time+2) ) then
     cycle
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
&                      -A_coeff*Efield(i_f)%versor_circ*( A_of_t(2)*theta)
       A%vecpot_vel(:)=-A_coeff*Efield(i_f)%versor     *( E_of_t(1)*theta+A_of_t(1)*delta )                 &
&                      -A_coeff*Efield(i_f)%versor_circ*( E_of_t(2)*theta+A_of_t(2)*delta )
       A%vecpot_acc(:)=-A_coeff*Efield(i_f)%versor     *( J_of_t(1)*theta+E_of_t(1)*delta+A_of_t(1)*signf)  &
&                      -A_coeff*Efield(i_f)%versor_circ*( J_of_t(2)*theta+E_of_t(2)*delta+A_of_t(2)*signf)
     case default
       call error("Unknown external field polarization! ")
     end select
     !
     if (ikind==1) call compute_envelop(A)
     if (ikind==2) call compute_intensity(A)
     !
   enddo
   !
   A_in%vecpot    =A_in%vecpot    +A%vecpot
   A_in%vecpot_vel=A_in%vecpot_vel+A%vecpot_vel
   A_in%vecpot_acc=A_in%vecpot_acc+A%vecpot_acc
   A_in%envelop   =A_in%envelop   +A%envelop
   A_in%intensity =A_in%intensity +A%intensity
   !
 enddo
 !
end subroutine RT_Ext_fields
