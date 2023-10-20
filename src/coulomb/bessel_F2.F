!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DV
!
function bessel_F2(z)
 use pars,          ONLY:SP,rZERO
 use R_lattice,     ONLY:cyl_zz_save,cyl_ph_radius
 implicit none
 real(SP) :: z
 ! 
 ! Work Space
 !
 integer       :: neval,ier,last,iwork(3)
 real(SP)      :: abserr,G,work(12),bessel_F2
 real(SP), external :: bessel_J0
 cyl_zz_save=z
#if defined _DOUBLE
 call dqag(bessel_J0,rZERO,cyl_ph_radius,8.e-1,0.,4,G,abserr,neval,ier, &
&         3,12,last,iwork,work)
#else
 call qag(bessel_J0,rZERO,cyl_ph_radius,8.e-1,0.,4,G,abserr,neval,ier, &
&         3,12,last,iwork,work)
#endif
 bessel_F2=G
end function 
