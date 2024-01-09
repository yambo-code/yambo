!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AMDS
!
function Lorentzian_FT(W,pole,ordering)
 !
 ! This subrutine returns the frequency dependent Green function
 ! which is the imaginary part of the
 ! Fourier transform of the Lorentzian function
 ! theta(t)*e^{-i*pole*t} [this is the retarded version, other ordering are computed]
 !
 ! AM,01/08/16. Note that aimag(pole) is always negative while aimag(W)>0
 !
 ! DS,31/10/18. Note that the present structure uses the relation e_{n,k}=e_{n,-k},
 !              See Fetter-Walecka, p.158 from Eq.(12.29) to (12.30)
 !              Such relation is always true. However it
 !              implies that f_{n,k}=f_{n,-k} and rho_{n,k}=rho_{n,-k} (eventually with a star)
 !              when the occupations and the oscillators are used.
 !              Here "n" is a generalized index for the transition when speaking about oscillators
 !              There are two cases where this is not true:
 !              a) with SOC rho_{n,k}/=rho_{n,-k} if SI is not a symmetry of the system
 !              b) in NEQ f_{n,k}=f_{n,-k} may not be true
 !              
 !
 use pars, ONLY:SP
 !
 implicit none
 !
 complex(SP)  ::pole,W,Lorentzian_FT
 character(*) ::ordering
 !              
 logical      ::ord_t_ordered,ord_true_t_or,ord_retarded_,ord_resonant_,ord_ares_tord,ord_ares_reta
 !
 ord_t_ordered= trim(ordering)=="T"      ! T-ordered
 ord_true_t_or= trim(ordering)=="t"      ! T-ordered with null damping !DALV: true time ordering for MPA
 ord_retarded_= trim(ordering)=="R"      ! Retarded 
 ord_resonant_= trim(ordering)=="r"      ! Resonant      part of the T-ordered/Retarded
 ord_ares_tord= trim(ordering)=="Ta"     ! Anti-resonant part of the T-ordered
 ord_ares_reta= trim(ordering)=="Ra"     ! Anti-resonant part of the Retarded
 !
 !                                  RESONANT       ANTIRESONANT
 !
 if ( ord_t_ordered ) Lorentzian_FT=1._SP/(W-pole)-1._SP/(conjg(W)+      pole )
 if ( ord_true_t_or ) Lorentzian_FT=1._SP/(W-pole)-1._SP/(      W +      pole ) 
 if ( ord_retarded_ ) Lorentzian_FT=1._SP/(W-pole)-1._SP/(      W +conjg(pole))       
 if ( ord_resonant_ ) Lorentzian_FT=1._SP/(W-pole)
 if ( ord_ares_tord ) Lorentzian_FT=              -1._SP/(conjg(W)+      pole )
 if ( ord_ares_reta ) Lorentzian_FT=              -1._SP/(      W +conjg(pole))
 !
end function
