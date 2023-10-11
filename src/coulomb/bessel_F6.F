!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DV
!
function bessel_F6(x,y)
 use pars,          ONLY:SP
 use R_lattice,     ONLY:cyl_ph_radius
 implicit none
 real(SP) :: x,y
 ! 
 ! Work Space
 !
 real(SP) :: arg1,arg2,bk0,bk1,bessel_F6
#if defined _DOUBLE
 real(SP), external :: DBESJ0_,DBESJ1_,DBESK0,DBESK1
#else
 real(SP), external :: BESJ0,BESJ1,BESK0,BESK1
#endif
 !
 arg1=x*cyl_ph_radius
 arg2=y*cyl_ph_radius
 if (arg2 > 80) then
   bk0=0.
   bk1=0.
 else
#if defined _DOUBLE
   bk0=DBESK0(arg2)
   bk1=DBESK1(arg2)
#else
   bk0=BESK0(arg2)
   bk1=BESK1(arg2)
#endif
 endif
#if defined _DOUBLE
 bessel_F6=arg1*DBESJ1_(arg1)*bk0-arg2*DBESJ0_(arg1)*bk1
 bessel_F6=bessel_F6+1
#else
 bessel_F6=arg1*BESJ1(arg1)*bk0-arg2*BESJ0(arg1)*bk1
 bessel_F6=bessel_F6+1
#endif
 !
end function
