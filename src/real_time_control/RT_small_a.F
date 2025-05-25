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
! FROMFILE: shape of the electric field from file
!
! Linear frequency chirp coded for QSSIN pulse: 
! (see also https://www.rp-photonics.com/chirp.html)
!
!    s_sigma_chirp=sigma**2/(2.*(sigma**4+chirp**4))
!    c_sigma_chirp=chirp**2/(2.*(sigma**4+chirp**4))
!    sigma_eff=sqrt((sigma**4+chirp**4)/sigma**2)=sqrt(1./(2.*s_sigma_chirp))
!
!    sin(omega*(T-T0)+c_sigma_chirp*(T-T_0)**2) * exp(-(T-T_0)**2/(2.*sigma_eff**2) )
!
!   = [ exp( cI*(omega*(T-T0))*exp( cI*c_sigma_chirp*(T-T_0)**2) 
!      -exp(-cI*(omega*(T-T0))*exp(-cI*c_sigma_chirp*(T-T_0)**2) ]
!      * exp(-(T-T_0)**2 * s_sigma_chirp ) / 2.
!
!   = [ exp( cI*(omega*(T-T0))* exp( (s_sigma_chirp+cI*c_sigma_chirp) *(T-T_0)**2)
!     - exp(-cI*(omega*(T-T0))* exp( (s_sigma_chirp-cI*c_sigma_chirp) *(T-T_0)**2) ]
!
!      
function small_a(T,dt,E_field,order,envelop_only)
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
  use pars,      ONLY:SP,cI,cONE,cZERO,pi,schlen
  use units,     ONLY:FS2AUT
  use stderr,    ONLY:STRING_split
  use wrapper,   ONLY:FADEVA
  use functions, ONLY:theta_function
  use fields,    ONLY:ext_field,get_field_file_index,field_frequency,&
  &                   field_from_file,n_fields_defs_max
  !
  implicit none
  !
  complex(SP), dimension(2) :: small_a
  !
  type(ext_field), intent(inout)   ::E_field
  !
  real(SP), intent(in)  ::T,dt
  integer,  intent(in)  ::order
  logical,  intent(in), optional :: envelop_only
  !
  ! Workspace
  !
  real(SP)          ::damp_func,a,b,c,sarg,WT,Tl
  !
  integer           ::i1,i_fr,i_T,i_field,i_file,n_fields,n_freqs
  real(SP)          ::Tloc,T_0_fac,T_0,W_0,sigma,chirp,sigma_eff,s_sigma_chirp,c_sigma_chirp,der_fac, &
  &                   WtimesT,EXPf,fr_shift(2),W_field,W_field_m1,cos_wt,sin_wt
  complex(SP)       ::f_now,f_t(2),f0t,f1t,cZ,cS,exp_iwt,erfi_z
  character(schlen) ::field_defs(n_fields_defs_max)
  logical           ::l_multi_phase,envelop_only_
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
  chirp=E_field%chirp
  !
  s_sigma_chirp=cZERO
  c_sigma_chirp=cZERO
  !
  if (abs(sigma)>0._SP .or. abs(chirp)>0._SP) then
    s_sigma_chirp=sigma**2/(2._SP*(sigma**4+chirp**4))
    c_sigma_chirp=chirp**2/(2._SP*(sigma**4+chirp**4))
  endif
  !
  sigma_eff=sqrt((sigma**4+chirp**4)/sigma**2)
  !
  n_freqs=max(1,E_field%n_frequencies)
  !
  field_defs=""
  call STRING_split(trim(E_field%ef_name),field_defs)
  !
  Tloc=T
  if( index(field_defs(1),"RECT")>0 .and. order==0 .and. abs(T)>=sigma) Tloc=sigma
  !
  select case( trim(field_defs(1)) )
    case('STATIC','RECT','RECTSIN','SIN','DELTA')
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
    case('FROM_FILE')
      i_file=get_field_file_index(field_defs(2))
      T_0=field_from_file(1,1,i_file)*FS2AUT
      W_0=0._SP
      damp_func=1._SP
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
      W_0=(field_frequency(E_field,1)+field_frequency(E_field,n_freqs))/2._SP
      T_0_fac=3._SP*sigma_eff
      if(trim(field_defs(2))=="1SIGMA" .or. trim(field_defs(3))=="1SIGMA") T_0_fac=1._SP*sigma_eff
      if(trim(field_defs(2))=="2SIGMA" .or. trim(field_defs(3))=="2SIGMA") T_0_fac=2._SP*sigma_eff
      if(trim(field_defs(2))=="3SIGMA" .or. trim(field_defs(3))=="3SIGMA") T_0_fac=3._SP*sigma_eff
      if(trim(field_defs(2))=="4SIGMA" .or. trim(field_defs(3))=="4SIGMA") T_0_fac=4._SP*sigma_eff
      if(trim(field_defs(2))=="5SIGMA" .or. trim(field_defs(3))=="5SIGMA") T_0_fac=5._SP*sigma_eff
      T_0=pi/W_0*(real(nint(W_0/pi*T_0_fac),SP))
      if( index(field_defs(1),"PULSE")>0 ) T_0=T_0_fac
  end select
  !
  ! Initial and relative phases control
  l_multi_phase=.false.
  fr_shift(1)=0._SP
  fr_shift(2)=pi/2._SP
  do i1=1,n_fields_defs_max
    if( trim(field_defs(i1))=="PHPI180") fr_shift(:)=fr_shift(:)+pi             ! 180 deg
    if( trim(field_defs(i1))=="PHPI120") fr_shift(:)=fr_shift(:)+pi*2._SP/3._SP ! 120 deg
    if( trim(field_defs(i1))=="PHPI90")  fr_shift(:)=fr_shift(:)+pi/2._SP       ! 90 deg
    if( trim(field_defs(i1))=="PHPI60")  fr_shift(:)=fr_shift(:)+pi/3._SP       ! 60 deg
    if( trim(field_defs(i1))=="PHPI30")  fr_shift(:)=fr_shift(:)+pi/6._SP       ! 30 deg
    if( trim(field_defs(i1))=="PHPI20")  fr_shift(:)=fr_shift(:)+pi/9._SP       ! 20 deg
    if( trim(field_defs(i1))=="PHMULT" .and. n_freqs>1) l_multi_phase=.true.
  enddo
  !
  E_field%To=T_0
  !
  do i_field=1,n_fields
    do i_fr=1,n_freqs
      !
      W_field=field_frequency(E_field,i_fr)
      W_field_m1=1._SP/W_field
      if(W_field> 0._SP) W_field_m1=1._SP/W_field
      if(W_field==0._SP) W_field_m1=0._SP
      der_fac=W_field+2._SP*c_sigma_chirp*(Tloc-T_0)
      !
      ! The frequency shift is applied in two cases
      ! (i n_fields=2) to have a circular polarized pulse, and
      WtimesT=W_field*(Tloc-T_0)+fr_shift(i_field)
      ! each frequency has a different initial phase
      if(l_multi_phase) WtimesT=WtimesT+(i_fr-1)/real(n_freqs-1,SP)*(fr_shift(mod(i_field,2)+1)-fr_shift(i_field))
      if(chirp>0._SP) WtimesT=WtimesT+c_sigma_chirp*(Tloc-T_0)**2
      !
      if (envelop_only_) then
        f0t=cONE ; f1t=cONE
      else
        ! CONTROL RES / ANTIRES case
        cos_wt=cos(WtimesT) ; sin_wt=sin(WtimesT); exp_iwt=cos_wt+cI*sin_wt
        ! f1t=-f0t'
        f0t=cmplx(cos_wt,0._SP) ; f1t=cmplx(sin_wt,0._SP)
        if(trim(field_defs(2))=="ANTIRES")     then ; f0t= 0.5_SP*      exp_iwt  ; f1t=-cI*0.5_SP*      exp_iwt   ; endif
        if(trim(field_defs(2))=="RES")         then ; f0t= 0.5_SP*conjg(exp_iwt) ; f1t= cI*0.5_SP*conjg(exp_iwt)  ; endif
      endif
      !
      EXPf=exp(-(T-T_0)**2/(2._SP*sigma_eff**2) )
      !
      select case( trim(field_defs(1)) )
      case('FROM_FILE')
        i_T=nint((T-T_0)/(dT/2._SP))+1
        if (i_T<=0.or.envelop_only_) then
          f_now=0
        else
          if (order==0 ) f_now=field_from_file(i_T,2+(i_field-1),i_file)
          if (order==1 ) f_now=field_from_file(i_T,4+(i_field-1),i_file)
          if (order==2 ) f_now=field_from_file(i_T,6+(i_field-1),i_file)
        endif
        !
      case('STATIC')
        if (order==0 ) f_now=T
        if (order==1 ) f_now=1._SP
        if (order==2 ) f_now=0._SP
        !
      case('RECT')
        if (order==0 ) f_now= Tloc
        if (order==1 ) f_now= theta_function(sigma-T,dT,0) ! theta function
        if (order==2 ) f_now=-theta_function(sigma-T,dT,1) ! delta function
        !
      case('RECTSIN')
        if (chirp>0._SP) call error("chirp not implemented with "//trim(field_defs(1)))
        if (order==0 ) f_now=-(f0t-1._SP)*W_field_m1
        if (order==1 ) f_now= theta_function(sigma-T,dT,0)*f1t     ! theta function
        if (order==2 ) f_now=-theta_function(sigma-T,dT,1)*f1t  &  ! delta function
                            &+theta_function(sigma-T,dT,0)*f0t*der_fac
        !
      case('SIN')
        if (chirp>0._SP) call error("chirp not implemented with "//trim(field_defs(1)))
        if (order==0 ) f_now=-damp_func*(f0t-1._SP)*W_field_m1
        if (order==1 ) f_now=+damp_func* f1t
        if (order==2 ) f_now=+damp_func* f0t       *der_fac
        !
      case('SOFTSIN')
        if (chirp>0._SP) call error("chirp not implemented with "//trim(field_defs(1)))
        if (order==-1) f_now=-2
        if (order== 0) f_now=-damp_func*(f0t-1._SP)*W_field_m1
        if (order== 1) f_now=+damp_func* f1t
        if (order== 2) f_now=+damp_func* f0t       *der_fac
        !
      case('THETA')
        if (order==0 ) f_now=damp_func*T
        if (order==1 ) f_now=damp_func
        if (order==2 ) f_now=0._SP
        !
      case('DELTA')
        if (order==-1) f_now=1._SP
        if (order==0 ) f_now=1._SP
        if (order> 0 ) f_now=0._SP
        !
      case('PHHG')
        if (chirp>0._SP) call error("chirp not implemented with "//trim(field_defs(1)))
        sarg=pi*(T-T_0)/sigma     
        WT  =W_field*T
        if(T-T_0<=0.0.or.T-T_0>=sigma.and.order>0) then
          f_now=0.0
        elseif(T-T_0>=sigma.and.order==0) then
          Tl=sigma+T_0
          WT  =W_field*Tl
          f_now=-(sigma*sin(((sigma*W_field+2*pi)*Tl-2*pi*T_0)/sigma))/(4*(sigma*W_field+2*pi))  & 
          & -(sigma*sin(((sigma*W_field-2*pi)*Tl+2*pi*T_0)/sigma))/(4*(sigma*W_field-2*pi))+sin(WT)/(2*W_field)
        else
          if (order==0 ) f_now=-(sigma*sin(((sigma*W_field+2*pi)*T-2*pi*T_0)/sigma))/(4*(sigma*W_field+2*pi)) &
          &  -(sigma*sin(((sigma*W_field-2*pi)*T+2*pi*T_0)/sigma))/(4*(sigma*W_field-2*pi))+sin(WT)/(2*W_field)
          if (order==1 ) f_now=sin(sarg)**2*cos(WT)
          if (order==2 ) f_now=(2*pi*cos(WT)*cos(sarg)*sin(sarg))/sigma-W_field*sin(WT)*sin(sarg)**2
        endif
        !
      case('GAUSS')
        if (order==0 ) f_now= sigma_eff*sqrt(pi/2._SP)* ( erf( (T-T_0)/(sigma_eff*sqrt(2._SP)) )+1._SP )
        if (order==1 ) f_now= Expf
        if (order==2 ) f_now=-Expf*(T-T_0)/sigma_eff**2
        !
      case('QSSIN')
        !
        ! W    =FADEVA function
        ! W(-z)=exp(-z^2)*(1-  erf (i*z))
        !      =exp(-z^2)*(1-i*erfi( z ))
        !
        ! ERFI(z)=i*(exp(z^2)*W(-z)-1)
        !
        if (trim(global_gauge)=="velocity" .and. chirp>0._SP) &
        &  call error("chirp not implemented with QSSIN and velocity gauge")
        !
        cZ=(sigma**2*W_field-cI*(T-T_0))/(sigma*sqrt(2._SP))
        cS=cmplx(W_field**2*sigma**2/2._SP,fr_shift(i_field),SP)
        !           
        if (order==0 .and. trim(global_gauge)=="velocity" ) erfi_z=-sqrt(pi/2._SP)*sigma/2._SP*         &
        &                       cI*( (exp(      cZ **2-      cS )*FADEVA(-      cZ )-exp(-      cS )) + &
        &                            (exp(conjg(cZ)**2-conjg(cS))*FADEVA(-conjg(cZ))-exp(-conjg(cS))) )
        if (order==0 .and. trim(global_gauge)=="length" )   erfi_z=cZERO
        !
        if (order==0 ) f_now= erfi_z
        if (order==1 ) f_now=                     f1t              *EXPf
        if (order==2 ) f_now=(der_fac*f0t-(T-T_0)*f1t/sigma_eff**2)*EXPf
        !
      case('QSFIELD')
        !
        if (order==0 ) f_now=                      f1t          *EXPf
        if (order==1 ) f_now=( der_fac*f0t-(T-T_0)*f1t/sigma**2)*EXPf
        if (order==2 ) f_now=(-der_fac*f1t-f1t/sigma**2                           &
                       &             -der_fac*(T-T_0)*f1t/sigma**2                &
                       &             -(T-T_0)*(der_fac*f0t-(T-T_0)*f1t/sigma**2)/sigma**2)*EXPf
        f_now=f_now/der_fac
        !
      case('PULSE')
        if (chirp>0._SP) call error("chirp not implemented with "//trim(field_defs(1)))
        !
        if(abs((T-T_0))< sigma) then
          if (order==0 ) f_now= 0._SP
          if (order==1 ) f_now=       (T-T_0 + sigma)**2 *(T-T_0 -sigma)**2/sigma**4 *f0t
          if (order==2 ) f_now=(4._SP*(T-T_0 + sigma)    *(T-T_0 -sigma)**2/sigma**4)*f0t &
          &                   -(1._SP*(T-T_0 + sigma)**2 *(T-T_0 -sigma)**2/sigma**4)*W_field*f1t
        endif
        !
      case('SPULSE')
        if (chirp>0._SP) call error("chirp not implemented with "//trim(field_defs(1)))
        T_0=sigma
        W_0=W_field
        f_now=cZERO
        if(abs((T-T_0))< sigma) then
          if (order==0 ) f_now=(4.0*W_0*(T-T_0)*f0t*(-sigma**2*W_0**2+W_0**2*(T-T_0)**2-6.0)   &
          &+f1t*(sigma**4*W_0**4-2.0*sigma**2*W_0**2*(W_0**2*(T-T_0)**2-2.0)+W_0**4*(T-T_0)**4-12.0* &
          & W_0**2*(T -T_0)**2+24))/W_0**5/sigma**4+                                                 &
          &   ((4.0*W_0*sigma*f0t*(-sigma**2*W_0**2+W_0**2*sigma**2-6.0)                             &
          &+f1t*(sigma**4*W_0**4-2.0*sigma**2*W_0**2*(W_0**2*sigma**2-2.0)+W_0**4*sigma**4-12.0*     &
          &               W_0**2*sigma**2+24))/W_0**5/sigma**4) 
          if (order==1 ) f_now=((T-T_0)**2 - sigma**2)**2/sigma**4*f0t
          if (order==2 ) f_now=(2._SP*(T-T_0 + sigma)    *(T-T_0 -sigma)**2/sigma**4                 &
          &                          +2._SP*(T-T_0 + sigma)**2 *(T-T_0 -sigma)   /sigma**4 )    *f0t &
          &                          -      (T-T_0 + sigma)**2 *(T-T_0 -sigma)**2/sigma**4  *W_0*f1t
        endif
      end select
      !
      f_t(i_field)=f_t(i_field)+f_now
      !
    enddo
  enddo
  !
  small_a=f_t/real(n_freqs,SP)
  !
end function small_a
!
function small_a_frequency(W_i,E_field,iErr)
  !
  use pars,      ONLY:SP,cZERO,cI,pi,schlen
  use stderr,    ONLY:STRING_split
  use fields,    ONLY:ext_field,n_fields_defs_max,field_frequency
  !
  implicit none
  !
  complex(SP) :: small_a_frequency
  !
  type(ext_field)   :: E_field
  complex(SP)       :: W_i
  integer           :: iErr
  !
  integer           ::i_f
  real(SP)          ::W_0
  complex(SP)       ::local_a(2)
  character(schlen) ::field_defs(n_fields_defs_max)
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
