!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
module ALLOC
 !
 implicit none
 !
 interface
   !
#if defined _ELPH
   !
   subroutine ELPH_alloc(what,GKKP,Nb,Nm,Nk_mem,Nq_mem)
     use ELPH,          ONLY:GKKP_me
     character(*)      :: what
     type(GKKP_me), optional:: GKKP
     integer,       optional:: Nb(2),Nk_mem,Nm(2),Nq_mem
   end subroutine
   !
#endif
   !
   subroutine DIPOLE_ALLOC_elemental(what,d)
     character(*)                :: what
     integer, optional           :: d(:)
   end subroutine
   !
   subroutine DIPOLE_ALLOC_global(D,NK)
     use DIPOLES,        ONLY:DIPOLE_t
     type(DIPOLE_t),intent(inout), optional :: D
     integer                     , optional :: NK
   end subroutine
   !
   subroutine X_ALLOC_elemental(WHAT,DIM)
     character(*)      :: WHAT
     integer, optional :: DIM(:)
   end subroutine
   !
 end interface
 !
end module ALLOC
