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
function small_a_frequency(W_i,E_field,iErr)
  !
  use pars,      ONLY:SP,cZERO,cI,pi,schlen
  use stderr,    ONLY:STRING_split
  use fields,    ONLY:ext_field,n_fields_defs_max
  !
  implicit none
  !
  complex(SP) :: small_a_frequency
  !
  type(ext_field)   :: E_field
  complex(SP)       :: W_i
  integer           :: iErr
  !
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
  W_0=E_field%frequency
  select case( trim(field_defs(1)) )
  case('SIN')
    iErr=0
    local_a(1)=local_a(1)+(1._SP/(W_i-W_0)                 -1._SP/W_0)/2._SP  ! RES
    local_a(2)=local_a(2)+(               -1._SP/(W_i+W_0) -1._SP/W_0)/2._SP  ! ARES
  case('DELTA')
    iErr=0
    local_a=1._SP/2._SP
  end select
  !
  if(trim(field_defs(2))==    'RES') local_a(2)=0._SP
  if(trim(field_defs(2))=='ANTIRES') local_a(1)=0._SP
  !
  small_a_frequency=local_a(1)+local_a(2)
  !
end function small_a_frequency
