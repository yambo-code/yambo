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
 use BS,      ONLY:l_BS_dichroism,l_BS_mespin,l_BS_meorb
 use y_memory_alloc
 !
 implicit none
 !
 complex(SP), allocatable :: BSS_dipoles_dich(:,:)
 complex(SP), allocatable :: BSS_dipoles_mespin(:,:)
 complex(SP), allocatable :: BSS_dipoles_meorb(:,:)
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

   subroutine BSS_MEspin_alloc( )
     use pars,       ONLY:cZERO
     use BS,         ONLY:BS_dip_size,BS_H_dim
     if (.not.l_BS_mespin) return
     YAMBO_ALLOC(BSS_dipoles_mespin,(BS_dip_size,BS_H_dim))
     BSS_dipoles_mespin = cZERO
   end subroutine
   !
   subroutine BSS_MEspin_free( )
     if (.not.l_BS_mespin) return
     YAMBO_FREE(BSS_dipoles_mespin)
   end subroutine
   !
   subroutine BSS_MEorb_alloc( )
     use pars,       ONLY:cZERO
     use BS,         ONLY:BS_dip_size,BS_H_dim
     if (.not.l_BS_meorb) return
     YAMBO_ALLOC(BSS_dipoles_meorb,(BS_dip_size,BS_H_dim))
     BSS_dipoles_meorb = cZERO
   end subroutine
   !
   subroutine BSS_MEorb_free( )
     if (.not.l_BS_meorb) return
     YAMBO_FREE(BSS_dipoles_meorb)
   end subroutine
   !
end module
