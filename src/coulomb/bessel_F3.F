!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DV
!
function bessel_F3(z)
 use pars,          ONLY:SP 
 use R_lattice,     ONLY:cyl_ph_radius
 implicit none
 real(SP) :: z,bessel_F3
 bessel_F3=z/sqrt(cyl_ph_radius**2+z**2)
end function 
