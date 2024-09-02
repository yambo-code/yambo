!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
module ELPH_intfcs
 !
 implicit none
 !
 interface
   !
   subroutine ELPH_alloc(what,GKKP,Nb,Nm,Nk_mem,Nq_mem)
     use ELPH,          ONLY:GKKP_me
     character(*)      :: what
     type(GKKP_me), optional:: GKKP
     integer,       optional:: Nb(2),Nk_mem,Nm(2),Nq_mem
   end subroutine
   !
 end interface
 !
end module ELPH_intfcs
