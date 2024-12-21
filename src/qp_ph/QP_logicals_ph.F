!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine QP_logicals_ph( )
 !
#if defined _PHEL
 use PHEL,          ONLY:PH_Self_Energy_mode
#endif
 use ELPH,          ONLY:l_GKKP_hosts_bare_dV
 !
 implicit none
 !
 call QP_logicals()
 !
 ! Ph-El...
 !
#if defined _PHEL
 ! ... Kind
 if ( index(PH_Self_Energy_mode,"bare")>0 ) l_GKKP_hosts_bare_dV = .true.
#endif
 !
end subroutine
