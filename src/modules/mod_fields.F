!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
! External fields:
!
! SIN:      E(t)=\theta(t) * sin(t)
! SOFTSIN:  E(t)= (c*t^2 + b*t^3 + a*t^4 )* sin(t)  and 0 for t<=0
! DELTA:    E(t)= \delta(t)
! GAUSS:    E(t)= exp((t-t_o)^2/(2*sigma^2))
! THETA:    E(t)= \theta(t)
! RES:      E(t)= \theta(t)*exp(-i\omega t)
! ANTIRES:  E(t)= \theta(t)*exp(i\omega t)
! PULSE:    E(t)=(T-T_0 + sigma)**2 *(T-T_0 -sigma)**2/sigma**4 * cos(w t)
! QSSIN:    E(t)= exp((t-t_o)^2/(2*sigma^2))*sin(w t)
! SPULSE:   E(t)=((T-T_0)**2 - sigma**2)**2/sigma**4*cos(w t)
! PHHG:     E(t)=sin^2(\pi*(T-T_0)/sigma)*cos( w * t) 
! QSFIELD:  see below
!      
module fields
 !
 use pars,  ONLY:SP,DP,schlen,lchlen,pi, &
&                n_ext_fields_max,n_fields_defs_max
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
   real(SP) :: chirp
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
 real(SP), allocatable :: field_from_file(:,:,:)
 character(schlen)     :: field_from_file_fname(n_ext_fields_max)
 integer               :: field_from_file_steps
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
     use stderr,        ONLY:STRING_same,STRING_split
     type(ext_field), intent(in) :: E_field
     character(schlen)   :: ef_name(n_fields_defs_max)
     call STRING_split(E_field%ef_name,ef_name)
     field_is_ok= any((/ STRING_same(ef_name(1),'SIN') ,&
&                        STRING_same(ef_name(1),'SOFTSIN') ,&
&                        STRING_same(ef_name(1),'DELTA') ,&
&                        STRING_same(ef_name(1),'RECT')  ,&
&                        STRING_same(ef_name(1),'RECTSIN'),&
&                        STRING_same(ef_name(1),'GAUSS') ,&
&                        STRING_same(ef_name(1),'THETA') ,&
&                        STRING_same(ef_name(1),'PULSE') ,&
&                        STRING_same(ef_name(1),'QSSIN') ,&
&                        STRING_same(ef_name(1),'SPULSE'),&
&                        STRING_same(ef_name(1),'PHHG')  ,&
&                        STRING_same(ef_name(1),'QSFIELD'),&
&                        STRING_same(ef_name(1),'FROM_FILE') /)) .and. &
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
     if (i_f<=1.or.E_field%n_frequencies<=1) return
     field_frequency=E_field%frequency(1)+&
&       (E_field%frequency(2)-E_field%frequency(1))/(E_field%n_frequencies-1)*(i_f-1)
   end function
   !
   integer function get_field_file_index(filename)
     character(*)      ::filename
     integer           ::i1
     get_field_file_index=-1
     do i1=1,n_ext_fields_max
       if ( trim(filename)/=trim(field_from_file_fname(i1)) ) cycle
       get_field_file_index=i1
       return
     enddo
   end function
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
     E2%chirp=E1%chirp
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
     E%chirp        =0._SP
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
     use stderr,ONLY:STRING_split
     type(ext_field):: Field
     integer        :: order
     ! work space
     complex(SP) :: E_w
     real(SP)    :: T_0, sigma, W_0, T
     character(schlen) ::field_defs(n_fields_defs_max)
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
end module fields
!
!
module fields_int
 !
 interface
   !
   function small_a(T,dt,E_field,order,envelop_only)
     !
     use pars,     ONLY:SP
     use fields,   ONLY:ext_field
     !
     type(ext_field), intent(inout)   ::E_field
     !
     real(SP), intent(in)  ::T,dt
     integer,  intent(in)  ::order
     logical,  intent(in), optional :: envelop_only
     !
     complex(SP), dimension(2) :: small_a
     !
   end function small_a
   !
   function small_a_frequency(W_i,E_field,iErr)
     !
     use pars,     ONLY:SP
     use fields,   ONLY:ext_field
     !
     type(ext_field)   ::E_field
     complex(SP)       ::W_i
     integer           ::iErr
     !
     complex(SP) :: small_a_frequency
     !
   end function small_a_frequency
   !
 end interface
 !
end module fields_int
