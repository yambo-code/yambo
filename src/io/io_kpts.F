!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
!> @brief Write info about k-points
!
! io_sec=1   general parameters   
! io_sec=2   k-points
!
! headers
!
#include<y_memory.h>
!
integer function io_kpts(k,Xk,q,io_db)
 !
 use pars,           ONLY:IP_YIO
 use R_lattice,      ONLY:bz_samp
 use IO_m,           ONLY:io_sec,io_status
 use IO_int,         ONLY:io_connect,io_disconnect,&
&                         io_variable_elemental,def_variable_elemental,io_header,&
&                         def_variable_bulk,io_variable_bulk
 use timing_m,       ONLY:timing
 use y_memory_alloc
 !
 implicit none
 !
 type(bz_samp)::k,q,Xk
 integer      ::io_db
 !
 call timing('io_kpts',OPR='start')
 !
 io_kpts=io_connect(desc='kpts',type=1,ID=io_db)
 if (io_kpts/=0) goto 1
 !
 if (any((/io_sec(io_db,:)==1/))) then
   !
   io_kpts=io_header(io_db,IMPOSE_SN=.true.)
   !
   call def_variable_elemental(io_db,"k_nibz",1,IP_YIO,0)
   call io_variable_elemental(io_db,I0=k%nibz,CHECK=.true.,OP=(/"=="/))
   !
   call def_variable_elemental(io_db,"k_nbz",1,IP_YIO,0)
   call io_variable_elemental(io_db,I0=k%nbz,CHECK=.true.,OP=(/"=="/))
   !
   call def_variable_elemental(io_db,"Xk_nibz",1,IP_YIO,0)
   call io_variable_elemental(io_db,VAR="Number of kpts for polarization (ibz)",&
   &                                         I0=Xk%nibz,CHECK=.true.,OP=(/"=="/))
   !
   call def_variable_elemental(io_db,"Xk_nbz",1,IP_YIO,0)
   call io_variable_elemental(io_db,VAR="Number of kpts for polarization (bz)",&
   &                                         I0=Xk%nbz,CHECK=.true.,OP=(/"=="/))
   !
   call def_variable_elemental(io_db,"q_nibz",1,IP_YIO,0)
   call io_variable_elemental(io_db,I0=q%nibz,CHECK=.true.,OP=(/"=="/))
   !
   call def_variable_elemental(io_db,"q_nbz",1,IP_YIO,0)
   call io_variable_elemental(io_db,I0=q%nbz,CHECK=.true.,OP=(/"=="/))
   !
   call def_variable_elemental(io_db,"",0,0,1)
   !
   io_kpts=io_status(io_db)
   !
   if (io_kpts/=0) goto 1
   !
 endif
 !
 if (any((/io_sec(io_db,:)==2/))) then
   !
   call def_variable_bulk(io_db,"Xk_sstar",1,shape(Xk%sstar),IP_YIO)
   call  io_variable_bulk(io_db,1,I2=Xk%sstar)
   !
 endif
 !
1 call io_disconnect(ID=io_db)
 !
 call timing('io_kpts',OPR='stop')
 !
end function io_kpts
