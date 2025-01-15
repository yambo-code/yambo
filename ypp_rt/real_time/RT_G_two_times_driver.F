!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine RT_G_two_times_driver(en,kpt,qpt)
 !
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 !
 implicit none
 !
 type(levels)  :: en
 type(bz_samp) :: kpt,qpt
 !
 call RT_G_two_times_build(en,kpt,qpt)
 !
 call RT_G_two_times_interp_and_plot(en,kpt,qpt)
 !
end subroutine RT_G_two_times_driver
