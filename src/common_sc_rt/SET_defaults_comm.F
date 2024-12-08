!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SET_defaults_comm()
 !
 use pars,           ONLY:SP
 use collision_ext,  ONLY:COLLISIONS_naming,COLLISIONS_cutoff,COLLISIONS_CV_only, &
&                         COLLISIONS_load_SP,COLLISIONS_compr
 use hamiltonian,    ONLY:H_potential
 !
 implicit none
 !
 call COLLISIONS_naming( 0 , "all" )
 !
 ! COLLISIONS I/O
 !
#if defined _PAR_IO
 COLLISIONS_cutoff=0.0000_SP
#else
 COLLISIONS_cutoff=0.0005_SP
#endif
 !
#if defined _NL
 COLLISIONS_cutoff=0.0005_SP
#endif
 !
 COLLISIONS_CV_only=.FALSE.
 COLLISIONS_load_SP=.FALSE.
 COLLISIONS_compr  =.FALSE.
 !
 H_potential="NONE"
 !
end subroutine
