!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine ELPH_databases_check(bands)
 ! 
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:REP,OP_RD_CL
 use ELPH,           ONLY:l_GKKP_DB_exists,l_GKKP_expanded_DB_exists,elph_bands
 !
 implicit none
 !
 integer :: bands(2)
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
 if (io_err==0) l_GKKP_DB_exists= bands(1)>elph_bands(1) .and. bands(2)<=elph_bands(2)
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),ID=ID)
 io_err=io_ELPH(ID,'gkkp_expanded')
 if (io_err==0) l_GKKP_expanded_DB_exists= bands(1)>elph_bands(1) .and. bands(2)<=elph_bands(2)
 !
 if (all((/l_GKKP_DB_exists,l_GKKP_expanded_DB_exists/))) &
 & call error("Neither expanded or un-expandend GKKP database found")
 !
end subroutine ELPH_databases_check
