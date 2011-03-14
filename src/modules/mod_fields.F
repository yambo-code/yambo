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
module fields
 !
 use pars, ONLY:SP,schlen,lchlen
 !
 implicit none
 !
 ! Main Gauge
 !
 character(schlen) :: global_gauge
 !
 ! Path for the shifted grids
 !
 character(lchlen) :: grid_path
 !
 ! GPL_EXCLUDE_START
 !
 ! Electric Field(s)
 ! 
 type ext_field
   real(SP) :: t_initial
   integer  :: int_t_initial
   real(SP) :: frequency
   real(SP) :: damping
   real(SP) :: intensity
   real(SP) :: amplitude
   real(SP) :: versor(3)
   character(schlen) :: ef_name
 end type ext_field
 !
 integer, parameter  :: n_ext_fields=2
 integer             :: i_Probe
 integer             :: i_Pump
 type(ext_field)     :: Efield(n_ext_fields)
 !
 ! ... switch on time of external field
 !
 integer  :: theta_function_iTo_ref
 integer  :: ef_i_time(n_ext_fields) ! time index of extern field switch on
 !
 ! Vector potential
 ! 
 type gauge_field
   complex(SP) :: vecpot(3)
   complex(SP) :: vecpot_vel(3)
   complex(SP) :: vecpot_acc(3)
 end type gauge_field
 !
 type(gauge_field) :: A_vecpot
 type(gauge_field) :: A_vecpot_previous
 !
 contains
   !
   logical function field_is_ok(E_field)
     type(ext_field)   ::E_field
     field_is_ok= any((/ trim(E_field%ef_name) == 'SIN' ,&
&                        trim(E_field%ef_name) == 'ANTIRES' ,& 
&                        trim(E_field%ef_name) == 'RES' ,& 
&                        trim(E_field%ef_name) == 'DELTA' ,& 
&                        trim(E_field%ef_name) == 'GAUSS' ,& 
&                        trim(E_field%ef_name) == 'QSSIN' /))
   end function
   !
   complex(SP) function small_a(T_or_W,E_field,order)
     !
     ! The vector potential is generally written as
     !
     !  A(t) =-cEo a(t) theta(t)
     !  A'(t)=-cEo (a'(t) theta(t)+a(t) delta(t))
     !  A"(t)=-cEo (a"(t) theta(t)+a'(t) delta(t)-a(t) sign(t))
     !
     ! the functions theta,delta and sign can be the standard distributions
     ! or more fancy functions that can mimic the distributions.
     !
     ! Note that A is evolved using A''(t) starting from A(0) and A'(0). 
     !
     ! If order=-1 small_a returns the Electric field at frequency T_or_W. If there
     ! is no analytic form possible order is set to be -2.
     !
     use pars,      ONLY:cI,pi
     type(ext_field)   ::E_field
     real(SP)          ::T_or_W
     integer           ::order
     !
     real(SP)          ::T_0,freq,sigma
     !
     freq=E_field%frequency
     sigma=E_field%damping
     T_0=10.*sigma
     small_a=(0._SP,0._SP)
     !
     select case( trim(E_field%ef_name) )
       case('SIN')
         if (order==0 ) small_a=-(cos(freq*T_or_W)-1._SP)/freq
         if (order==1 ) small_a= sin(freq*T_or_W)
         if (order==2 ) small_a=freq*cos(freq*T_or_W)
         if (order==-1) small_a=1._SP/2._SP/(T_or_W-freq)-1._SP/2._SP/(T_or_W+freq)-1._SP/freq
       case('ANTIRES')
         if (order==0 ) small_a=-cI*exp(cI*freq*T_or_W)/freq
         if (order==1 ) small_a=    exp(cI*freq*T_or_W)
         if (order==2 ) small_a=cI*freq*exp(cI*freq*T_or_W)
         if (order==-1) small_a=-cI*(1._SP/(T_or_W+freq)+1._SP/freq)
       case('RES')
         if (order==0 ) small_a= cI*exp(-cI*freq*T_or_W)/freq
         if (order==1 ) small_a=    exp(-cI*freq*T_or_W)
         if (order==2 ) small_a=-cI*freq*exp(-cI*freq*T_or_W)
         if (order==-1) small_a=-cI*(1._SP/(T_or_W-freq)-1._SP/freq)
       case('DELTA')
         if (order==0 ) small_a=1.
         if (order> 0 ) small_a=0.
         if (order==-1) small_a=1.
       case('GAUSS')
         if (order==0 ) small_a= sigma*sqrt(pi/2._SP)* ( erf( (T_or_W-T_0)/(sigma*sqrt(2._SP)) )+1._SP )
         if (order==1 ) small_a= exp(-(T_or_W-T_0)**2/(2._SP*sigma**2) )
         if (order==2 ) small_a=-exp(-(T_or_W-T_0)**2/(2._SP*sigma**2) )*(T_or_W-T_0)/sigma**2
         if (order==-1) order=-2
       case('QSSIN')
         if (order==0 ) small_a= 0.
         if (order==1 ) small_a= sin(freq*(T_or_W-T_0))*exp(-(T_or_W-T_0)**2/(2._SP*sigma**2) )
         if (order==2 ) small_a= (freq*cos(freq*(T_or_W-T_0))-(T_or_W-T_0)/sigma**2)*exp(-(T_or_W-T_0)**2/(2._SP*sigma**2) )
         if (order==-1) order=-2
     end select
     !
   end function
   !
   real(SP) function theta_function(T,step,order)
     !
     ! This is an easy numerical implementation of the
     ! theta function and its derivatives (the delta and the sign).
     !
     integer :: iT,order
     real(SP):: T,step
     !
     theta_function=0.
     iT=nint(T/step)
     !
     if (iT==theta_function_iTo_ref) then
       if (order==2) theta_function= 1./step**2.
     endif
     !
     if (iT==theta_function_iTo_ref+1) then
       if (order==1) theta_function= 1./step
       if (order==2) theta_function=-1./step**2.
     endif
     !
     if (iT>=theta_function_iTo_ref+2) then
       if (order==0) theta_function= 1.
     endif
     !
   end function
   !
   ! GPL_EXCLUDE_END
   !
end module
