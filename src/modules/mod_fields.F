!
!        Copyright (C) 2000-2023 the YAMBO team
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
 use pars,  ONLY:SP,DP,schlen,lchlen,pi,n_ext_fields_max
 use stderr,ONLY:STRING_split
 use units, ONLY:FS2AUT
 !
 implicit none
 !
 ! Electric Field(s)
 ! 
 type ext_field
   character(schlen) :: ef_name
   character(schlen) :: ef_pol
   real(SP) :: To= 0._SP
   real(SP) :: t_initial
   integer  :: t_initial_indx    ! time index of extern field switch on  (input variable)
   real(SP) :: t_final
   integer  :: t_final_indx      ! time index of extern field switch off (computed in RT_Fluence)
   integer  :: n_frequencies
   real(SP) :: frequency(2)
   real(SP) :: width
   real(SP) :: FWHM
   real(SP) :: fluence
   real(SP) :: pi_kind
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
   real(SP)    :: envelop
   real(SP)    :: intensity
   real(SP)    :: fluence
 end type gauge_field
 !
 type(gauge_field) :: A_tot    ! This is either the total or the externa field
 type(gauge_field) :: A_ext
 type(gauge_field) :: A_ind
 !
 contains
   !
   logical function field_is_ok(E_field)
     use stderr,        ONLY:STRING_same
     type(ext_field)   ::E_field
     field_is_ok= any((/ STRING_same(E_field%ef_name,'SIN') ,&
&                        STRING_same(E_field%ef_name,'SOFTSIN') ,& 
&                        STRING_same(E_field%ef_name,'DELTA') ,& 
&                        STRING_same(E_field%ef_name,'GAUSS') ,& 
&                        STRING_same(E_field%ef_name,'THETA') ,& 
&                        STRING_same(E_field%ef_name,'PULSE') ,& 
&                        STRING_same(E_field%ef_name,'QSSIN') ,&
&                        STRING_same(E_field%ef_name,'SPULSE'),&
&                        STRING_same(E_field%ef_name,'QSFIELD') /)) .and. &
&                 any((/ STRING_same(E_field%ef_pol,'linear') ,&
&                        STRING_same(E_field%ef_pol,'circular') /))
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
   function small_a(T,E_field,order,envelop_only)
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
     use pars,      ONLY:cI,cONE,cZERO,pi,schlen
     use wrapper,   ONLY:FADEVA
     !
     type(ext_field), intent(inout)   ::E_field
     !
     real(SP), intent(in)  ::T
     integer,  intent(in)  ::order
     logical,  intent(in), optional :: envelop_only
     !
     ! Workspace
     !
     real(SP)          ::damp_func,a,b,c
     complex(SP), dimension(2) :: small_a
     !
     integer           ::i_fr,i_field,n_fields
     real(SP)          ::T_0,W_0,sigma,WtimesT,EXPf,fr_shift(2),W_field,W_field_m1,cos_wt,sin_wt
     complex(SP)       ::f_now,f_t(2),f0t,f1t,cZ,cS,exp_iwt,erfi_z
     character(schlen) ::field_defs(3)
     logical           ::envelop_only_
     !
     ! Zeroing
     !
     small_a=cZERO
     f_t=cZERO
     damp_func=1._SP
     !
     envelop_only_=.false.
     if(present(envelop_only)) envelop_only_=envelop_only
     !
     if(trim(E_field%ef_pol)=="linear"  ) n_fields=1
     if(trim(E_field%ef_pol)=="circular") n_fields=2
     !
     ! Field parameters
     !
     sigma=E_field%width
     fr_shift(1)=0._SP
     fr_shift(2)=pi/2._SP
     !
     field_defs=""
     call STRING_split(trim(E_field%ef_name),field_defs)
     !
     select case( trim(field_defs(1)) )
       case('STATIC','SIN','ANTIRES','RES','DELTA')
         ! Fields which do not need T_0
         W_0=0._SP
         T_0=0._SP
         damp_func=1._SP
         ! DEBUG <
         ! damping for sin 
         !if (sigma>0._SP) then
         !  T_0=5*sigma
         !  damp_func=1._SP
         !  if (T<T_0)             damp_func=exp( (T-T_0)/sigma)
         !  if (T>NE_tot_time-T_0) damp_func=exp(-(T-NE_tot_time+T_0)/sigma)
         !endif
         ! DEBUG >
       case('SOFTSIN','THETA')
         ! Fields which do not need T_0 and with damp_func
         W_0=0._SP
         T_0=0._SP
         a =  3._SP/sigma**4 
         b = -8._SP/sigma**3
         c =  6._SP/sigma**2
         damp_func=1._SP
         if(real(T)<sigma.and.sigma>0._SP) damp_func=(a*T**4 + b*T**3 + c*T**2) 
       case('GAUSS','QSSIN','QSFIELD','PULSE','SPULSE')
         ! Fields which need T_0
         W_0=field_frequency(E_field,1)
         T_0=pi/W_0*(real(nint(W_0/pi*3._SP*sigma),SP))
         if(trim(field_defs(2))=="2SIGMA" .or. trim(field_defs(3))=="2SIGMA") T_0=2._SP*sigma
         if(trim(field_defs(2))=="3SIGMA" .or. trim(field_defs(3))=="3SIGMA") T_0=3._SP*sigma
         if(trim(field_defs(2))=="4SIGMA" .or. trim(field_defs(3))=="4SIGMA") T_0=4._SP*sigma
         if(trim(field_defs(2))=="5SIGMA" .or. trim(field_defs(3))=="5SIGMA") T_0=5._SP*sigma
     end select
     !
     E_field%To=T_0
     !
     do i_field=1,n_fields
       do i_fr=1,max(1,E_field%n_frequencies)
         !
         if (envelop_only_) then
           W_field=0._SP
           W_field_m1=0._SP
           WtimesT=fr_shift(i_field)
           f0t=cONE ; f1t=cONE
         else
           W_field=field_frequency(E_field,i_fr)
           W_field_m1=1._SP/W_field
           WtimesT=W_field*(T-T_0)+fr_shift(i_field)
           cos_wt=cos(WtimesT) ; sin_wt=sin(WtimesT); exp_iwt=cos_wt+cI*sin_wt
           f0t=cmplx(cos_wt,0._SP) ; f1t=cmplx(sin_wt,0._SP)
           if(trim(field_defs(2))=="ANTIRES")     then ; f0t= 0.5_SP*      exp_iwt  ; f1t=-cI*0.5_SP*      exp_iwt   ; endif
           if(trim(field_defs(2))=="RES")         then ; f0t= 0.5_SP*conjg(exp_iwt) ; f1t= cI*0.5_SP*conjg(exp_iwt)  ; endif
         endif
         !
         EXPf=exp(-(T-T_0)**2/(2._SP*sigma**2) )
         !
         select case( trim(field_defs(1)) )
         case('STATIC')
           if (order==0 ) f_now=T
           if (order==1 ) f_now=1._SP
           if (order==2 ) f_now=0._SP
           !
         case('SIN')
           if (order==0 ) f_now=-damp_func*(f0t-1._SP)*W_field_m1
           if (order==1 ) f_now=+damp_func* f1t
           if (order==2 ) f_now=+damp_func* f0t       *W_field
           !
         case('SOFTSIN')
           if (order==-1) f_now=-2
           if (order== 0) f_now=-damp_func*(f0t-1._SP)*W_field_m1
           if (order== 1) f_now=+damp_func* f1t
           if (order== 2) f_now=+damp_func* f0t       *W_field
           !
         case('THETA')
           if (order==0 ) f_now=damp_func*T
           if (order==1 ) f_now=damp_func
           if (order==2 ) f_now=0._SP
           !
         case('DELTA')
           if (order==0 ) f_now=1._SP
           if (order> 0 ) f_now=0._SP
           if (order==-1) f_now=1._SP
           !
         case('GAUSS')
           if (order==0 ) f_now= sigma*sqrt(pi/2._SP)* ( erf( (T-T_0)/(sigma*sqrt(2._SP)) )+1._SP )
           if (order==1 ) f_now= Expf
           if (order==2 ) f_now=-Expf*(T-T_0)/sigma**2
           !
         case('QSSIN')
           !
           ! W    =FADEVA function
           ! W(-z)=exp(-z^2)*(1-  erf (i*z))
           !      =exp(-z^2)*(1-i*erfi( z ))
           !
           ! ERFI(z)=i*(exp(z^2)*W(-z)-1)
           !
           cZ=(sigma**2*W_field-cI*(T-T_0))/(sigma*sqrt(2._SP))
           cS=cmplx(W_field**2*sigma**2/2._SP,fr_shift(i_field),SP)
           !           
           if (order==0 .and. trim(global_gauge)=="velocity" ) erfi_z=-sqrt(pi/2._SP)*sigma/2._SP*         &
&                                      cI*( (exp(      cZ **2-      cS )*FADEVA(-      cZ )-exp(-      cS )) + &
&                                           (exp(conjg(cZ)**2-conjg(cS))*FADEVA(-conjg(cZ))-exp(-conjg(cS))) )
           if (order==0 .and. trim(global_gauge)=="length" )   erfi_z=cZERO
           !
           if (order==0 ) f_now= erfi_z
           if (order==1 ) f_now=                     f1t          *EXPf
           if (order==2 ) f_now=(W_field*f0t-(T-T_0)*f1t/sigma**2)*EXPf
           !
         case('QSFIELD')
           !
           if (order==0 ) f_now=                      f1t          *EXPf
           if (order==1 ) f_now=( W_field*f0t-(T-T_0)*f1t/sigma**2)*EXPf
           if (order==2 ) f_now=(-W_field*f1t-f1t/sigma**2                           &
                          &             -W_field*(T-T_0)*f1t/sigma**2                    &
                          &             -(T-T_0)*(W_field*f0t-(T-T_0)*f1t/sigma**2)/sigma**2)*EXPf
           !
         case('PULSE')
           if(abs((T-T_0))< sigma) then
             if (order==0 ) f_now= 0._SP
             if (order==1 ) f_now=       (T-T_0 + sigma)**2 *(T-T_0 -sigma)**2/sigma**4 *f0t
             if (order==2 ) f_now=(4._SP*(T-T_0 + sigma)    *(T-T_0 -sigma)**2/sigma**4)*f0t &
&                                       -(1._SP*(T-T_0 + sigma)**2 *(T-T_0 -sigma)**2/sigma**4)*W_field*f1t
           endif
           !
         case('SPULSE')
           T_0=sigma
           W_0=W_field
           f_now=cZERO
           if(abs((T-T_0))< sigma) then
             if (order==0 ) f_now=(4.0*W_0*(T-T_0)*f0t*(-sigma**2*W_0**2+W_0**2*(T-T_0)**2-6.0)   &
&              +f1t*(sigma**4*W_0**4-2.0*sigma**2*W_0**2*(W_0**2*(T-T_0)**2-2.0)+W_0**4*(T-T_0)**4-12.0* &
&               W_0**2*(T -T_0)**2+24))/W_0**5/sigma**4+                                                          &
&               ((4.0*W_0*sigma*f0t*(-sigma**2*W_0**2+W_0**2*sigma**2-6.0)                             &
&              +f1t*(sigma**4*W_0**4-2.0*sigma**2*W_0**2*(W_0**2*sigma**2-2.0)+W_0**4*sigma**4-12.0*   &
&               W_0**2*sigma**2+24))/W_0**5/sigma**4)                                                             
             if (order==1 ) f_now=((T-T_0)**2 - sigma**2)**2/sigma**4*f0t
             if (order==2 ) f_now=(2._SP*(T-T_0 + sigma)    *(T-T_0 -sigma)**2/sigma**4                 &
&                                        +2._SP*(T-T_0 + sigma)**2 *(T-T_0 -sigma)   /sigma**4 )    *f0t &
&                                        -      (T-T_0 + sigma)**2 *(T-T_0 -sigma)**2/sigma**4  *W_0*f1t
           endif
         end select
         !
         f_t(i_field)=f_t(i_field)+f_now
         !
       enddo
     enddo
     !
     small_a=f_t
     !
   end function small_a
   !
   complex(SP) function small_a_frequency(W_i,E_field,iErr)
     !
     use pars,      ONLY:cZERO,cI,pi,schlen
     type(ext_field)   ::E_field
     complex(SP)       ::W_i
     integer           ::iErr
     !
     integer           ::i_f
     real(SP)          ::W_0
     complex(SP)       ::local_a(2)
     character(schlen) ::field_defs(3)
     !
     iErr=-1
     local_a=cZERO
     !
     field_defs=""
     call STRING_split(trim(E_field%ef_name),field_defs)
     !
     do i_f=1,E_field%n_frequencies     
       W_0=field_frequency(E_field,i_f)
       select case( trim(field_defs(1)) )
       case('SIN')
         iErr=0
         local_a(1)=local_a(1)+(1._SP/(W_i-W_0)                 -1._SP/W_0)/2._SP  ! RES
         local_a(2)=local_a(2)+(               -1._SP/(W_i+W_0) -1._SP/W_0)/2._SP  ! ARES
       case('DELTA')
         iErr=0
         local_a=1._SP/2._SP
       end select
     enddo
     !
     if(trim(field_defs(2))==    'RES') local_a(2)=0._SP
     if(trim(field_defs(2))=='ANTIRES') local_a(1)=0._SP
     !
     small_a_frequency=local_a(1)+local_a(2)
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
     E2%amplitude=E1%amplitude  
     E2%intensity=E1%intensity  
     E2%fluence=E1%fluence
     E2%pi_kind=E1%pi_kind
     E2%versor=E1%versor
     E2%versor_circ=E1%versor_circ
     E2%ef_name=E1%ef_name
     E2%ef_pol=E1%ef_pol
   end subroutine
   !
   subroutine Efield_reset(E)
     type(ext_field) :: E
     E%t_initial    =0._SP
     E%t_initial_indx=2
     E%t_final      =0._SP
     E%t_final_indx =0
     E%frequency    =0._SP
     E%n_frequencies=1
     E%W_step       =0._SP
     E%amplitude    =0._SP
     E%intensity    =0._SP
     E%fluence      =0._SP
     E%pi_kind      =0._SP
     E%width        =0._SP
     E%ef_name      ='none'
     E%ef_pol       ='none'
     E%versor       =(/1._SP,0._SP,0._SP/)
     E%versor_circ  =(/0._SP,1._SP,0._SP/)
   end subroutine
   !
   subroutine A_vecpot_reset(A)
     type(gauge_field) :: A
     A%vecpot_acc=0._SP
     A%vecpot_vel=0._SP
     A%vecpot    =0._SP
     A%envelop   =0._SP
     A%fluence   =0._SP
     A%intensity =0._SP
   end subroutine
   !
   subroutine compute_intensity(A)
     use pars,   ONLY:SP
     use units,  ONLY:SPEED_OF_LIGHT
     type(gauge_field) :: A
     A%intensity= (abs(A%vecpot_vel(1))**2+abs(A%vecpot_vel(2))**2+abs(A%vecpot_vel(3))**2)/SPEED_OF_LIGHT/4._SP/pi
   end subroutine compute_intensity
   !
   subroutine compute_envelop(A)
     use pars,   ONLY:SP
     use units,  ONLY:SPEED_OF_LIGHT
     type(gauge_field) :: A
     A%envelop= sqrt(abs(A%vecpot_vel(1))**2+abs(A%vecpot_vel(2))**2+abs(A%vecpot_vel(3))**2)/SPEED_OF_LIGHT
   end subroutine compute_envelop
   !
   complex(SP) function Divide_by_Field(Field,order)
     !
     use pars, ONLY: cI,cONE,pi,schlen
     type(ext_field):: Field
     integer        :: order
     ! work space
     complex(SP) :: E_w
     real(SP)    :: T_0, sigma, W_0, T
     character(schlen) ::field_defs(3)
     !
     call STRING_split(trim(Field%ef_name),field_defs)
     !
     select case(trim(field_defs(1)))
       case('SIN','SOFTSIN')
         if (order/=0) Divide_by_Field =(-2._SP*cI  /Field%amplitude)**order
         if (order==0) Divide_by_Field =  4._SP*cONE/Field%amplitude**2
       case('QSSIN') !Approximate relations/does not work yet
         sigma=Field%width
         T_0=10._SP*sigma
         W_0=Field%frequency(1)
         T = 2._SP*pi/W_0
         E_w= sqrt(pi/2._SP)*Field%amplitude*sigma*exp(-cI*W_0*T_0)*&
              &(erf((T-T_0)/sqrt(2._SP)/sigma)+erf(T_0/sqrt(2._SP)/sigma))
         if (order/=0) Divide_by_Field =  (2._SP*cI  / E_w)**order
         if (order==0) Divide_by_Field =   4._SP*cONE/(E_w*conjg(E_w))
       end select
       !
   end function Divide_by_Field
   !
   real(SP) function Efield_strength(Intensity,unit_system)
     ! 
     ! From Octopus ( http://www.tddft.org/programs/octopus )
     !
     ! It is very common to describe the strength of a laser field by its intensity, 
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
