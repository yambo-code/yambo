!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine ELPH_databases_check(n_bands)
 ! 
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:REP,OP_RD_CL
 use ELPH,           ONLY:l_GKKP_DB_exists,l_GKKP_expanded_DB_exists,elph_nb
 !
 implicit none
 !
 integer :: n_bands
 !
 ! Work Space
 !
 integer             ::ID,io_err
 integer, external   ::io_ELPH
 !
 ! I/O check
 !===========
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_ELPH(ID,'gkkp')
 if (io_err==0) l_GKKP_DB_exists=n_bands<=elph_nb
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_ELPH(ID,'gkkp_expanded')
 if (io_err==0) l_GKKP_expanded_DB_exists=n_bands<=elph_nb
 !
 if (all((/l_GKKP_DB_exists,l_GKKP_expanded_DB_exists/))) call error("Both expanded and un-expandend GKKP database found")
 !
end subroutine ELPH_databases_check
