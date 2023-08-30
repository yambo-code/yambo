!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DV
!
function bessel_F5(z)
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_vr_save
 implicit none
 ! 
 ! Work Space
 !
 real(SP)           :: z,arg,bessel_F5
#if defined _DOUBLE
 real(SP), external :: DBESJ0_
#else
 real(SP), external :: BESJ0
#endif
 if (z==0) then
   bessel_F5=0.
 else
   arg=cyl_vr_save*z
#if defined _DOUBLE
   bessel_F5=DBESJ0_(arg)*z*log(z)
#else
   bessel_F5=BESJ0(arg)*z*log(z)
#endif
 endif
 !
end function
