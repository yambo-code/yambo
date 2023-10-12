!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
integer function io_E_SOC_map(En,kpt,ID)
 !
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp
 use IO_m,           ONLY:read_is_on,write_is_on,file_is_present
 use IO_int,         ONLY:io_connect,io_disconnect,io_elemental,io_bulk,io_header
 !
#include<memory.h>
 !
 type(levels) :: En
 type(bz_samp):: kpt
 integer      :: ID 
 !
 ! Work Space
 !
 io_E_SOC_map=-1
 !
 if (file_is_present('E_SOC_map').or.write_is_on(ID)) io_E_SOC_map=io_connect(desc='E_SOC_map',type=2,ID=ID)
 !
 if (io_E_SOC_map/=0) goto 1
 !
 io_E_SOC_map=io_header(ID)
 !
 if (io_E_SOC_map/=0) goto 1
 !
 call io_elemental(ID,VAR="PARS",VAR_SZ=3,MENU=1)
 call io_elemental(ID,I0=En%nb_SOC,    VAR=' Number of SOC bands       ')
 call io_elemental(ID,I0=En%nb,        VAR=' Number of bands           ')
 call io_elemental(ID,I0=En%nk,        VAR=' Number of k-points        ')
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 !
 if (read_is_on(ID)) then
   YAMBO_ALLOC(En%E_SOC,(En%nb_SOC,En%nk))
   YAMBO_ALLOC(En%table_SOC,(2,En%nb,En%nk))
 endif
 !
 ! Eigenvalues
 !
 call io_bulk(ID, VAR="EIGENVALUES", VAR_SZ=shape(En%E_SOC) ) 
 call io_bulk(ID, R2=En%E_SOC )
 !
 call io_bulk(ID, VAR="BLOCK_TABLE", VAR_SZ=shape(En%table_SOC) ) 
 call io_bulk(ID, I3=En%table_SOC )
 !
1 call io_disconnect(ID=ID)
 !
end function io_E_SOC_map
