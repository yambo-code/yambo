!
! License-Identifier: GPL
!
! Copyright (C) 2011 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
module DICHROISM
 !
 use pars,    ONLY:SP
 use BS,      ONLY:l_BS_dichroism
 !
#include<memory.h>
 !
 complex(SP), allocatable :: BSS_dipoles_dich(:,:)
 !
 contains
   !
   subroutine BSS_DICH_alloc( )
     use pars,       ONLY:cZERO
     use BS,         ONLY:BS_dip_size,BS_H_dim
     if (.not.l_BS_dichroism) return
     YAMBO_ALLOC(BSS_dipoles_dich,(BS_dip_size,BS_H_dim))
     BSS_dipoles_dich = cZERO
   end subroutine
   !
   subroutine BSS_DICH_free( )
     if (.not.l_BS_dichroism) return
     YAMBO_FREE(BSS_dipoles_dich)
   end subroutine
   !
end module
