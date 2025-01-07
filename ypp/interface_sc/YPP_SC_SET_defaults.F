!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DSAM
!
subroutine YPP_SC_SET_defaults()
 !
 use pars,        ONLY:SP
 use YPPm,        ONLY:l_mean_potential,B_field,B_psi,B_theta,l_angular_momentum,l_position,&
 &                     l_amplitude,E_field
 !
 implicit none
 !
 call YPP_SET_defaults()
 !
 l_mean_potential=.false.
 !
 ! ANGULAR MOMENTUM & ELECTRON DISPLACEMENT
 !
 l_angular_momentum=.false.
 l_position=.false.
 !
 !REMOVE SYMMETRIES
 !
 E_field=0._SP
 B_field=0._SP
 B_theta=0._SP
 B_psi=0._SP
 !
end subroutine YPP_SC_SET_defaults
