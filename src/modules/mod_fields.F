!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
 use pars, ONLY:SP,DP,schlen,lchlen,pi,n_ext_fields_max
 use units,ONLY:FS2AUT
 !
 implicit none
 !
 ! Electric Field(s)
 ! 
 type ext_field
   character(schlen) :: ef_name
   character(schlen) :: ef_pol
   real(SP) :: To
   real(SP) :: t_initial
   integer  :: t_initial_indx    ! time index of extern field switch on  (input variable)
   real(SP) :: t_final
   integer  :: t_final_indx      ! time index of extern field switch off (computed in RT_Fluence)
   integer  :: n_frequencies
   real(SP) :: frequency(2)
   real(SP) :: width
   real(SP) :: FWHM
   real(SP) :: fluence = 0._SP
   real(SP) :: intensity
   real(SP) :: amplitude
   real(SP) :: W_step
   real(SP) :: versor(3)
   real(SP) :: versor_circ(3)
 end type ext_field
 !
 integer               :: n_ext_fields    =0
 type(ext_field), save :: Efield(n_ext_fields_max)
 !
 ! Vector potential
 ! 
 type gauge_field
   complex(SP) :: vecpot(3)
   complex(SP) :: vecpot_vel(3)
   complex(SP) :: vecpot_acc(3)
   real(SP)    :: fluence
   real(SP)    :: intensity
 end type gauge_field
 !
 type(gauge_field) :: A_tot    ! This is either the total or the externa field
 type(gauge_field) :: A_ext
 type(gauge_field) :: A_ind
 !
 contains
   !
   logical function field_is_ok(E_field)
     type(ext_field)   ::E_field
     field_is_ok= any((/ trim(E_field%ef_name) == 'SIN' ,&
&                        trim(E_field%ef_name) == 'SOFTSIN' ,& 
&                        trim(E_field%ef_name) == 'ANTIRES' ,& 
&                        trim(E_field%ef_name) == 'RES' ,& 
&                        trim(E_field%ef_name) == 'DELTA' ,& 
&                        trim(E_field%ef_name) == 'GAUSS' ,& 
&                        trim(E_field%ef_name) == 'THETA' ,& 
&                        trim(E_field%ef_name) == 'PULSE' ,& 
&                        trim(E_field%ef_name) == 'QSSIN' ,&
&                        trim(E_field%ef_name) == 'QSFIELD' /)) .and. &
&                 any((/ trim(E_field%ef_pol)  == 'linear' ,&
&                        trim(E_field%ef_pol)  == 'circular' /))
   end function
   !
   real(SP) function EtoT(E,T)
     use QP_m,       ONLY:Time_of
     real(SP), optional ::T
     real(SP), optional ::E
     if (present(T)) EtoT=2._SP*pi*Time_of(T,"fs")*FS2AUT
     if (present(E)) EtoT=2._SP*pi*Time_of(E,"fs")*FS2AUT
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
   function small_a(T,E_field,order)
     !
     ! The vector potential is generally written as
     !
     !  order=0  A (t)=-cEo  a (t) theta(t)
     !  order=1  A'(t)=-cEo (a'(t) theta(t)+a (t) delta(t))
     !  order=2  A"(t)=-cEo (a"(t) theta(t)+a'(t) delta(t)-a(t) sign(t))
     !
     ! the functions theta,delta and sign can be the standard distributions
     ! or more fancy functions that can mimic the distributions.
     !
     ! Note that A is evolved using A''(t) starting from A(0) and A'(0). 
     !
     use X_m,       ONLY:global_gauge
     use pars,      ONLY:cI,cONE,cZERO,pi
     use wrapper,   ONLY:FADEVA
     !
     type(ext_field)   ::E_field
     !
     real(SP)          ::T
     integer           ::order
     !
     real(SP)          ::damp_func,a,b,c
     complex(SP), dimension(2) :: small_a
     !
     integer           ::i_fr,i_field,n_fields
     real(SP)          ::T_0,W_0,W_field,sigma,WtimesT,EXPf,fr_shift(2)
     complex(SP)       ::f_t(2),cZ,cS
     !
     small_a=cZERO
     !
     W_0=E_field%frequency(1)
     sigma=E_field%width
     T_0=pi/W_0*(real(nint(W_0/pi*3._SP*sigma),SP))
     f_t=cZERO
     fr_shift(1)=0._SP
     fr_shift(2)=pi/2._SP
     !
     if(trim(E_field%ef_pol)=="linear"  ) n_fields=1
     if(trim(E_field%ef_pol)=="circular") n_fields=2
     !
     do i_field=1,n_fields
       !
       select case( trim(E_field%ef_name) )
       case('STATIC')
         if (order==0 ) f_t(i_field)=T_i
         if (order==1 ) f_t(i_field)=1._SP
         if (order==2 ) f_t(i_field)=0._SP
       case('SIN')
         do i_fr=1,E_field%n_frequencies     
           W_field=field_frequency(E_field,i_fr)
           WtimesT=W_field*T+fr_shift(i_field)
           if (order==0 ) f_t(i_field)=f_t(i_field)-    (cos(WtimesT)-1._SP)/W_field
           if (order==1 ) f_t(i_field)=f_t(i_field)+     sin(WtimesT)
           if (order==2 ) f_t(i_field)=f_t(i_field)+W_0* cos(WtimesT)
         enddo 
       case('SOFTSIN')
         T_0=sigma
         !
         a =  3._SP/T_0**4 
         b = -8._SP/T_0**3
         c =  6._SP/T_0**2
         !
         damp_func=1._SP
         if(real(T_i)<T_0.and.T_0>0._SP) damp_func=(a*T_i**4 + b*T_i**3 + c*T_i**2)       
         !
         do i_fr=1,E_field%n_frequencies     
           !
           W_i=field_frequency(E_field,i_fr)
           WT_i=W_i*T_i+fr_shift(i_field)
           !
           if (order==0 ) f_t(i_field)=f_t(i_field)- damp_func*(cos(WT_i)-1._SP)/W_i
           if (order==1 ) f_t(i_field)=f_t(i_field)+ damp_func*sin(WT_i)
           if (order==2 ) f_t(i_field)=f_t(i_field)+ damp_func*W_0*cos(WT_i)
           if (order==-1) f_t(i_field)=-2
           !
         enddo 
         !
       case('THETA')
         !  
         T_0=sigma
         !
         a =  3._SP/T_0**4 
         b = -8._SP/T_0**3
         c =  6._SP/T_0**2
         !
         damp_func=1._SP
         if(real(T_i)<T_0.and.T_0>0._SP) damp_func=(a*T_i**4 + b*T_i**3 + c*T_i**2)       
         !
         if (order==0 ) f_t(i_field)=damp_func*T_i
         if (order==1 ) f_t(i_field)=damp_func
         if (order==2 ) f_t(i_field)=0._SP
         !
       case('ANTIRES')
         do i_fr=1,E_field%n_frequencies
           W_field=field_frequency(E_field,i_fr)
           WtimesT=W_field*T+fr_shift(i_field)
           if (order==0 ) f_t(i_field)=f_t(i_field)-cI*        exp(cI*WtimesT)/W_field
           if (order==1 ) f_t(i_field)=f_t(i_field)+           exp(cI*WtimesT)
           if (order==2 ) f_t(i_field)=f_t(i_field)+cI*W_field*exp(cI*WtimesT)
         enddo
         !
       case('RES')
         do i_fr=1,E_field%n_frequencies
           W_field=field_frequency(E_field,i_fr)
           WtimesT=W_field*T+fr_shift(i_field)
           if (order==0 ) f_t(i_field)=f_t(i_field)+cI*        exp(-cI*WtimesT)/W_field
           if (order==1 ) f_t(i_field)=f_t(i_field)-           exp(-cI*WtimesT)
           if (order==2 ) f_t(i_field)=f_t(i_field)-cI*W_field*exp(-cI*WtimesT)
         enddo
         !
       case('DELTA')
         if (order==0 ) f_t(i_field)=1._SP
         if (order> 0 ) f_t(i_field)=0._SP
         if (order==-1) f_t(i_field)=1._SP
         !
       case('GAUSS')
         if (order==0 ) f_t(i_field)= sigma*sqrt(pi/2._SP)* ( erf( (T-T_0)/(sigma*sqrt(2._SP)) )+1._SP )
         if (order==1 ) f_t(i_field)= exp(-(T-T_0)**2/(2._SP*sigma**2) )
         if (order==2 ) f_t(i_field)=-exp(-(T-T_0)**2/(2._SP*sigma**2) )*(T-T_0)/sigma**2
         !
       case('QSSIN')
         WtimesT=W_0*(T-T_0)+fr_shift(i_field)
         EXPf=exp(-(T-T_0)**2/(2._SP*sigma**2) )
         cZ=(sigma**2*W_0-cI*(T-T_0))/(sigma*sqrt(2._SP))
         cS=cmplx(W_0**2*sigma**2/2._SP,fr_shift(i_field),SP)
         !
         ! W    =FADEVA function
         ! W(-z)=exp(-z^2)*(1-  erf (i*z))
         !      =exp(-z^2)*(1-i*erfi( z ))
         !
         ! ERFI(z)=i*(exp(z^2)*W(-z)-1)
         !
         if (order==0 .and. trim(global_gauge)=="velocity" ) f_t(i_field)=-sqrt(pi/2._SP)*sigma/2._SP*         &
&                                      cI*( (exp(      cZ **2-      cS )*FADEVA(-      cZ )-exp(-      cS )) + &
&                                           (exp(conjg(cZ)**2-conjg(cS))*FADEVA(-conjg(cZ))-exp(-conjg(cS))) )
         if (order==0 .and. trim(global_gauge)=="length" ) f_t(i_field)=cZERO
         if (order==1 ) f_t(i_field)=     sin(WtimesT)                                 *EXPf
         if (order==2 ) f_t(i_field)=(W_0*cos(WtimesT)-(T-T_0)*sin(WtimesT)/sigma**2)*EXPf
         !
       case('QSFIELD')
         WtimesT=W_0*(T-T_0)+fr_shift(i_field)
         EXPf=exp(-(T-T_0)**2/(2._SP*sigma**2) )
         cZ=(sigma**2*W_0-cI*(T-T_0))/(sigma*sqrt(2._SP))
         cS=cmplx(W_0**2*sigma**2/2._SP,fr_shift(i_field),SP)
         !
         if (order==0 ) f_t(i_field)=      sin(WtimesT)                                 *EXPf
         if (order==1 ) f_t(i_field)=( W_0*cos(WtimesT)-(T-T_0)*sin(WtimesT)/sigma**2)*EXPf
         if (order==2 ) f_t(i_field)=(-W_0*sin(WtimesT)-sin(WtimesT)/sigma**2-W_0*(T-T_0)*sin(WtimesT)/sigma**2 &
&                                     -(T-T_0)*(W_0*cos(WtimesT)-(T-T_0)*sin(WtimesT)/sigma**2)/sigma**2)*EXPf
         !
       case('PULSE')
         f_t(i_field)= 0._SP
         if(abs((T-T_0))< sigma) then
           WtimesT=W_0*(T-T_0)+fr_shift(i_field)
           if (order==0 ) f_t(i_field)= 0._SP
           if (order==1 ) f_t(i_field)=       (T-T_0 + sigma)**2 *(T-T_0 -sigma)**2/sigma**4      *cos(WtimesT)
           if (order==2 ) f_t(i_field)=(2._SP*(T-T_0 + sigma)    *(T-T_0 -sigma)**2/sigma**4                 &
&                                      +2._SP*(T-T_0 + sigma)**2 *(T-T_0 -sigma)   /sigma**4 )    *cos(WtimesT) &
&                                      -      (T-T_0 + sigma)**2 *(T-T_0 -sigma)**2/sigma**4  *W_0*sin(WtimesT)
         endif
         !
       end select
       !
     enddo
     !
     small_a=f_t
     !
   end function small_a
   !
   complex(SP) function small_a_frequency(W_i,E_field,iErr)
     !
     use pars,      ONLY:cZERO,cI,pi
     type(ext_field)   ::E_field
     complex(SP)       ::W_i
     integer           ::iErr
     !
     integer           ::i_f
     real(SP)          ::W_0
     complex(SP)       ::local_a
     !
     iErr=-1
     local_a=cZERO
     !
     do i_f=1,E_field%n_frequencies     
       W_0=field_frequency(E_field,i_f)
       select case( trim(E_field%ef_name) )
       case('SIN')
         iErr=0
         local_a=local_a+   (1._SP/(W_i-W_0)-1._SP/(W_i+W_0))/2._SP-1._SP/W_0
       case('ANTIRES')
         iErr=0
         local_a=local_a+cI*(               -1._SP/(W_i+W_0)       -1._SP/W_0)
       case('RES')
         iErr=0
         local_a=       -cI*(1._SP/(W_i-W_0)                       -1._SP/W_0)
       case('DELTA')
         iErr=0
         local_a=1._SP
       end select
     enddo
     !
     small_a_frequency=local_a
     !
   end function small_a_frequency
   !
   subroutine Efield_duplicate(E1,E2)
     type(ext_field) :: E1,E2
     E2%t_initial=E1%t_initial  
     E2%t_initial_indx=E1%t_initial_indx  
     E2%t_final=E1%t_final  
     E2%t_final_indx=E1%t_final_indx  
     E2%frequency=E1%frequency  
     E2%n_frequencies=E1%n_frequencies  
     E2%W_step=E1%W_step  
     E2%width=E1%width  
     E2%FWHM=E1%FWHM  
     E2%intensity=E1%intensity  
     E2%amplitude=E1%amplitude  
     E2%versor=E1%versor
     E2%versor_circ=E1%versor_circ
     E2%ef_name=E1%ef_name
     E2%ef_pol=E1%ef_pol
   end subroutine
   !
   subroutine Efield_reset(E)
     use real_time,  ONLY:RT_step
     type(ext_field) :: E
     E%t_initial    =RT_step
     E%t_initial_indx=2
     E%t_final      =0._SP
     E%t_final_indx =0
     E%frequency    =0._SP
     E%n_frequencies=1
     E%W_step       =0._SP
     E%intensity    =0._SP
     E%width        =0._SP
     E%ef_name      ='none'
     E%ef_pol       ='none'
     E%versor       =(/1._SP,0._SP,0._SP/)
     E%versor_circ  =(/0._SP,1._SP,0._SP/)
     !GPL_INCLUDE_START
     E%ef_name      ='DELTA'
     E%ef_pol       ='linear'
     !GPL_INCLUDE_END
   end subroutine
   !
   subroutine A_vecpot_reset(A)
     type(gauge_field) :: A
     A%vecpot_acc=0._SP
     A%vecpot_vel=0._SP
     A%vecpot    =0._SP
   end subroutine
   !
   complex(SP) function Rabi_Factor(Field,P,R)
     use X_m,       ONLY:global_gauge
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
         if (order/=0) Divide_by_Field =(1._SP,0._SP)/Field%amplitude**order
         if (order==0) Divide_by_Field =(1._SP,0._SP)/Field%amplitude**2
       case('SIN','SOFTSIN')
         Divide_by_Field =  (-2._SP*cI/Field%amplitude)**order
         if (order==0) Divide_by_Field =  (4._SP,0._SP)/Field%amplitude**2
       case('QSSIN') !Approximate relations/does not work yet
         sigma=Field%width
         T_0=10._SP*sigma
         W_0=Field%frequency(1)
         T = 2._SP*pi/W_0
         E_w= sqrt(pi/2._SP)*Field%amplitude*sigma*exp(-cI*W_0*T_0)*&
              &(erf((T-T_0)/sqrt(2._SP)/sigma)+erf(T_0/sqrt(2._SP)/sigma))
         if (order/=0) Divide_by_Field =  (2._SP*cI/E_w)**order
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
       Efield_strength= sqrt(I/1._SP/FREE_SPACE_PERM/SPEED)*VMm12AU
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
end module
