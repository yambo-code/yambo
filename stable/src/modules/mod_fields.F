!
! Copyright (C) 2000-2016 A. Marini and the YAMBO team 
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
 use pars, ONLY:SP,schlen,lchlen,pi
 use units,ONLY:HA2EV,FS2AUT,HBAR_eVfs
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
   integer  :: n_frequencies
   real(SP) :: frequency(2)
   real(SP) :: damping
   real(SP) :: intensity
   real(SP) :: amplitude
   real(SP) :: W_step
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
&                        trim(E_field%ef_name) == 'PULSE' ,& 
&                        trim(E_field%ef_name) == 'QSSIN' /))
   end function
   !
   real(SP) function EtoT(E,T)
     real(SP), optional ::T
     real(SP), optional ::E
     if (present(T)) EtoT=2._SP*pi*HBAR_eVfs/(T*HA2EV)*FS2AUT
     if (present(E)) EtoT=2._SP*pi*HBAR_eVfs/(E*HA2EV)*FS2AUT
   end function
   !
   real(SP) function field_frequency(E_field,i_f)
     integer           ::i_f
     type(ext_field)   ::E_field
     field_frequency=E_field%frequency(1)
     if (i_f>1.and.E_field%n_frequencies>1) field_frequency=E_field%frequency(1)+&
&       (E_field%frequency(2)-E_field%frequency(1))/(E_field%n_frequencies-1)*(i_f-1)
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
     complex(SP)       ::T_or_W
     integer           ::order
     !
     integer           ::i_f
     real(SP)          ::T_0,freq,sigma
     complex(SP)       ::local_a
     !
     freq=E_field%frequency(1)
     sigma=E_field%damping
     T_0=3._SP*sigma
     local_a=(0._SP,0._SP)
     !
     select case( trim(E_field%ef_name) )
       case('SIN')
         do i_f=1,E_field%n_frequencies     
           !
           freq=field_frequency(E_field,i_f)
           !
           if (order==0 ) local_a=local_a-(cos(freq*T_or_W)-1._SP)/freq
           if (order==1 ) local_a=local_a+ sin(freq*T_or_W)
           if (order==2 ) local_a=local_a+freq*cos(freq*T_or_W)
           if (order==-1) local_a=local_a+1._SP/2._SP/(T_or_W-freq)-1._SP/2._SP/(T_or_W+freq)-1._SP/freq
           !
         enddo 
       case('ANTIRES')
         do i_f=1,E_field%n_frequencies
           !
           freq=field_frequency(E_field,i_f)
           !
           if (order==0 ) local_a=local_a-cI*exp(cI*freq*T_or_W)/freq
           if (order==1 ) local_a=local_a+    exp(cI*freq*T_or_W)
           if (order==2 ) local_a=local_a+cI*freq*exp(cI*freq*T_or_W)
           if (order==-1) local_a=local_a-cI*(1._SP/(T_or_W+freq)+1._SP/freq)
           !
         enddo
       case('RES')
         do i_f=1,E_field%n_frequencies
           !
           freq=field_frequency(E_field,i_f)
           !
           if (order==0 ) local_a= cI*exp(-cI*freq*T_or_W)/freq
           if (order==1 ) local_a=    exp(-cI*freq*T_or_W)
           if (order==2 ) local_a=-cI*freq*exp(-cI*freq*T_or_W)
           if (order==-1) local_a=-cI*(1._SP/(T_or_W-freq)-1._SP/freq)
           !
         enddo
       case('DELTA')
         if (order==0 ) local_a=1._SP
         if (order> 0 ) local_a=0._SP
         if (order==-1) local_a=1._SP
       case('GAUSS')
         !if (order==0 ) local_a= sigma*sqrt(pi/2._SP)* ( erf( (real(T_or_W,SP)-T_0)/(sigma*sqrt(2._SP)) )+1._SP )
         if (order==1 ) local_a= exp(-(T_or_W-T_0)**2/(2._SP*sigma**2) )
         if (order==2 ) local_a=-exp(-(T_or_W-T_0)**2/(2._SP*sigma**2) )*(T_or_W-T_0)/sigma**2
         if (order==-1) order=-2
       case('QSSIN')
         if (order==0 ) local_a= 0._SP
         if (order==1 ) local_a= sin(freq*(T_or_W-T_0))*exp(-(T_or_W-T_0)**2/(2._SP*sigma**2) )
         if (order==2 ) local_a= (freq*cos(freq*(T_or_W-T_0)) &
&            -(T_or_W-T_0)* sin(freq*(T_or_W-T_0))/sigma**2)*exp(-(T_or_W-T_0)**2/(2._SP*sigma**2) )
         if (order==-1) order=-2
       case('PULSE')
         if(abs((real(T_or_W)-T_0))< sigma) then
           if (order==0 ) local_a= 0._SP
           if (order==1 ) local_a=(T_or_W-T_0 + sigma)**2 *(T_or_W-T_0 - sigma)**2/sigma**4*cos(freq*(T_or_W-T_0))
           if (order==2 ) local_a=(2*(T_or_W-T_0 + sigma)*    (T_or_W-T_0 - sigma)**2/sigma**4  + &
&                                  2*(T_or_W-T_0 + sigma)**2 *(T_or_W-T_0 - sigma)   /sigma**4) * &
&                                  cos(freq*(T_or_W-T_0))    &
&                                   -(T_or_W-T_0 + sigma)**2 *(T_or_W-T_0 - sigma)**2/sigma**4  * &
&                                  freq*sin(freq*(T_or_W-T_0))
         else 
           local_a=0._SP
         endif        
           if (order==-1) order=-2
     end select
     !
     small_a=local_a
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
     theta_function=0._SP
     iT=floor(T/step)
     !
     if (iT==theta_function_iTo_ref) then
       if (order==2) theta_function= 1._SP/step**2.
     endif
     !
     if (iT==theta_function_iTo_ref+1) then
       if (order==1) theta_function= 1._SP/step
       if (order==2) theta_function=-1._SP/step**2.
     endif
     !
     if (iT>=theta_function_iTo_ref+2) then
       if (order==0) theta_function= 1._SP
     endif
     !
   end function
   !
   subroutine Efield_duplicate(E1,E2)
     type(ext_field) :: E1,E2
     E2%t_initial=E1%t_initial  
     E2%int_t_initial=E1%int_t_initial  
     E2%frequency=E1%frequency  
     E2%n_frequencies=E1%n_frequencies  
     E2%W_step=E1%W_step  
     E2%damping=E1%damping  
     E2%intensity=E1%intensity  
     E2%amplitude=E1%amplitude  
     E2%versor=E1%versor
     E2%ef_name=E1%ef_name
   end subroutine
   !
   subroutine Efield_reset(E)
     type(ext_field) :: E
     E%t_initial    =0._SP
     E%int_t_initial=0
     E%frequency    =0._SP
     E%n_frequencies=1
     E%W_step       =0._SP
     E%intensity    =0._SP
     E%damping      =0._SP
     E%ef_name      ='none'
     E%versor       =(/1._SP,0._SP,0._SP/)
   end subroutine
   !
   complex(SP) function Rabi_Factor(Field,P,R)
     !
     ! Rabi=2 * \mu.E
     !
     ! E is the field amplitude = sqrt ( I *8*pi/c  ) 
     !
     ! I is given in the input file
     !
     use pars, ONLY: cI
     type(ext_field), intent(in) :: Field
     complex(SP)                 :: P(3),R(3)
     if ( trim(global_gauge)=='length' ) then
       Rabi_Factor=2._SP*sum(R(:)*Field%versor(:))*Field%amplitude
     else 
       Rabi_Factor=2._SP*sum(P(:)*Field%versor(:))*Field%amplitude
       if( any( trim(Field%ef_name)==(/'ANTIRES','SIN    '/)) ) Rabi_Factor=-cI*Rabi_Factor/Field%frequency(1)
       if( any( trim(Field%ef_name)==(/'RES'/)) )               Rabi_Factor= cI*Rabi_Factor/Field%frequency(1)
     endif
   end function Rabi_Factor
   !
   complex(SP) function Divide_by_Field(Field,order)
     !
     use pars, ONLY: cI,pi
     type(ext_field):: Field
     integer        :: order
     ! work space
     complex(SP) :: E_w
     real(SP)    :: T_0, sigma, W_0, T
     !
     select case(Field%ef_name)
       case('ANTIRES')
         Divide_by_Field = (1._SP,0._SP)/Field%amplitude**order
         if (order==0) Divide_by_Field =(1._SP,0._SP)/Field%amplitude**2
       case('SIN')
         Divide_by_Field =  (2._SP*cI/Field%amplitude)**order
         if (order==0) Divide_by_Field =  (4._SP,0._SP)/Field%amplitude**2
       case('QSSIN') !Approximate relations/does not work yet
         sigma=Field%damping
         T_0=10._SP*sigma
         W_0=Field%frequency(1)
         T = 2._SP*pi/W_0
!         E_w= sqrt(pi/2._SP)*Field%amplitude*sigma*exp(-cI*W_0*T_0)*&
!              &(erf((T-T_0)/sqrt(2._SP)/sigma)+erf(T_0/sqrt(2._SP)/sigma))
         Divide_by_Field = (2._SP*cI/E_w)**order
         if (order==0) Divide_by_Field =  (4._SP,0._SP)/(E_w*conjg(E_w))
       end select
       !
   end function Divide_by_Field
   !
   real(SP) function Efield_strength(Intensity,unit_system)
     ! 
     ! From Octopus ( http://www.tddft.org/programs/octopus )
     !
     ! It is very common to describe the strength of a laser field by it intensity, 
     ! rather than using the electric field amplitude. In atomic units, the relationship 
     ! between instantaneous electric field and intensity is:
     !
     ! I(t) = \frac{c}{8\pi} E^2(t)
     !
     ! It is common to read intensities in kW cm^-2. The dimensions of intensities are [W]/(L^2T), 
     ! where [W] are the dimensions of energy. The relevant conversion factors are:
     !
     ! Hartree / (a_0^2 atomic_time) = 6.4364086e+15 W / cm^2 = 6.4364086e+12 kW / cm^2
     !
     ! In Yambo AU2KWCMm2=6.4364086e+12
     !
     !--------------------------------------------------------------------------------------------
     !
     ! This simple function uses the formula valid in SI and CGS to extract 
     ! the Field intensity given in au
     !
     use units,ONLY:AU2KWCMm2,SPEED_OF_LIGHT_SI,FREE_SPACE_PERM,VMm12AU,WMm22KWCMm2,&
&                   WMm22ERGCMm2SECm1,SVCMm12VMm1,SPEED_OF_LIGHT
     implicit none
     !
     real(SP)     :: Intensity
     character(*) :: unit_system
     !
     ! Work Space
     !
     real(SP) :: SPEED,I
     !
     ! From Boyd, "Non linear optics", 3rd edition, page 602,603
     !
     ! Assuming n=1 (epsilon=1)
     !
     if (unit_system=="SI") then
       !
       I=Intensity*AU2KWCMm2/WMm22kWCMm2
       !
       SPEED=SPEED_OF_LIGHT_SI
       !
       ! I = 1 FREE_SPACE_PERM SPEED_OF_LIGHT |E|^2 
       !
       Efield_strength= sqrt(I/1./FREE_SPACE_PERM/SPEED)*VMm12AU
       !
     endif
     !
     if (unit_system=="CGS") then
       !
       I=Intensity*AU2KWCMm2/WMm22kWCMm2/WMm22ERGCMm2SECm1
       !
       SPEED=SPEED_OF_LIGHT_SI*100._SP ! cm/sec
       !
       ! I = SPEED_OF_LIGHT/4/pi |E|^2 
       !
       Efield_strength= sqrt(I*4._SP*pi/SPEED)*SVCMm12VMm1*VMm12AU
       !
     endif
     !
     if (unit_system=="AU") then
       !
       I=Intensity
       !
       SPEED=SPEED_OF_LIGHT
       !
       ! I = SPEED_OF_LIGHT/4/pi |E|^2 
       !
       Efield_strength= sqrt(I*4._SP*pi/SPEED)
       !
     endif
     !
   end function
   !
   ! GPL_EXCLUDE_END
   !
end module
