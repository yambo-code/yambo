!
! License-Identifier: GPL
!
! Copyright (C) 2011 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
! headers
!
#include<y_memory.h>
!
module DICHROISM
 !
 use pars,    ONLY:SP
 use BS,      ONLY:l_BS_dichroism,l_BS_magnetoelectric
 use y_memory_alloc
 !
 implicit none
 !
 complex(SP), allocatable :: BSS_dipoles_dich(:,:)
 complex(SP), allocatable :: BSS_dipoles_me(:,:)
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

   subroutine BSS_ME_alloc( )
     use pars,       ONLY:cZERO
     use BS,         ONLY:BS_dip_size,BS_H_dim
     if (.not.l_BS_magnetoelectric) return
     YAMBO_ALLOC(BSS_dipoles_me,(BS_dip_size,BS_H_dim))
     BSS_dipoles_me = cZERO
   end subroutine
   !
   subroutine BSS_ME_free( )
     if (.not.l_BS_magnetoelectric) return
     YAMBO_FREE(BSS_dipoles_me)
   end subroutine
   !
end module
