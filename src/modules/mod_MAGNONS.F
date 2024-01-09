!
! License-Identifier: GPL
!
! Copyright (C) 2011 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
module MAGNONS
 !
 use pars,       ONLY:SP
 use BS,         ONLY:l_BS_magnons
 !
#include<memory.h>
 !
 complex(SP), allocatable :: BSS_dipoles_magn(:,:)
 !
 contains
   !
   subroutine BSS_MAGN_alloc( )
     use pars,       ONLY:cZERO
     use BS,         ONLY:BS_H_dim
     if (.not.l_BS_magnons) return
     YAMBO_ALLOC(BSS_dipoles_magn,(2,BS_H_dim))
     BSS_dipoles_magn = cZERO
   end subroutine
   !
   subroutine BSS_MAGN_free( )
     if (.not.l_BS_magnons) return
     YAMBO_FREE(BSS_dipoles_magn)
   end subroutine
   !
end module
