!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DV
!
function bessel_F4(z)
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_vr_save,cyl_cut
 implicit none
 real(SP)  :: z
 ! 
 ! Work Space
 !
 real(SP)  :: arg,bessel_F4
#if defined _DOUBLE
 real(SP), external :: DBESJ0_
#else
 real(SP), external :: BESJ0
#endif
 if (z==0) then
   bessel_F4=0.
 else 
   arg=cyl_vr_save*z
#if defined _DOUBLE
   bessel_F4=DBESJ0_(arg)*z*log((cyl_cut+sqrt(cyl_cut**2+z**2))/z)
#else
   bessel_F4=BESJ0(arg)*z*log((cyl_cut+sqrt(cyl_cut**2+z**2))/z)
#endif
 endif
end function
