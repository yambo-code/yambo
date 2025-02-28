!
! License-Identifier: GPL
!
! Copyright (C) 2025 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!
integer function io_full_SYMMs(k,ID)
 !
 use pars,          ONLY:schlen,SP,IP,IP_YIO
 use D_lattice,     ONLY:kpoints_map,old_nsym,old_dl_sop
 use R_lattice,     ONLY:bz_samp
 use IO_m,          ONLY:io_sec,read_is_on,write_is_on,io_extension
 use IO_int,        ONLY:io_connect,io_disconnect,io_bulk,def_variable_elemental, &
&                        io_variable_elemental,io_bulk
 !
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
#include<y_memory.h>
 !
 integer,      intent(in) :: ID
 type(bz_samp),intent(in) :: k
 !
 ! Work Space
 !
#if defined _TIMING
 call timing('io_full_SYMMs',OPR='start')
#endif
 !
 io_extension(ID)="full_SYMMs"
 !
 io_full_SYMMs=io_connect(desc=trim(io_extension(ID)),type=2,ID=ID)
 if (io_full_SYMMs/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   call def_variable_elemental(ID,"old_nsymm",1,IP_YIO,0)
   call io_variable_elemental(ID,VAR="Old number of symmetries",I0=old_nsym)
   !
 endif
 !
 if(read_is_on(ID)) then
   YAMBO_ALLOC(old_dl_sop,(3,3,old_nsym))
   YAMBO_ALLOC(kpoints_map,(2,k%nibz))
 endif
 !
 call io_bulk(ID,VAR="old_DL_dop",VAR_SZ=shape(old_dl_sop))
 call io_bulk(ID,R3=old_dl_sop)
 !
 call io_bulk(ID,VAR="kpoints_map",VAR_SZ=shape(kpoints_map))
 call io_bulk(ID,I2=kpoints_map)
 !
1  call io_disconnect(ID=ID)
 !
#if defined _TIMING
 call timing('io_full_SYMMs',OPR='stop')
#endif
 !
end function
