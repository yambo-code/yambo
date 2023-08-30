!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DV
!
function bessel_J0(s)
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_vr_save,cyl_zz_save
 implicit none
 !
 real(SP) ::bessel_J0
 ! 
 ! Work Space
 !
 real(SP) ::  s,arg
#if defined _DOUBLE
 real(SP) :: DBESJ0_
#else
 real(SP) :: BESJ0
#endif
 !
 arg=cyl_vr_save*s
 if(cyl_zz_save==0) then
#if defined _DOUBLE
   bessel_J0=DBESJ0_(arg)
#else
   bessel_J0=BESJ0(arg)
#endif
 else
#if defined _DOUBLE
   bessel_J0=DBESJ0_(arg)*s/sqrt(s**2+cyl_zz_save**2)
#else
   bessel_J0=BESJ0(arg)*s/sqrt(s**2+cyl_zz_save**2)
#endif
 endif
end function
