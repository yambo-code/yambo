!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
integer function io_RIM(ID)
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:RIM_ng,RIM_n_rand_pts,RIM_is_diagonal,RL_vol,RIM_RL_vol,&
&                         RIM_id_epsm1_reference,RIM_epsm1,RIM_anisotropy,RIM_qpg,&
&                         nqibz
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,io_header,&
&                         io_elemental,io_status,io_bulk,read_is_on
#include <memory.h>
 integer :: ID
 ! 
 ! Work Space
 !
 real(SP)            ::RL_vol_disk
 integer             ::RIM_ng_disk
 !
 io_RIM=io_connect(desc="RIM",type=2,ID=ID)
 !
 if (io_RIM/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_RIM=io_header(ID,QPTS=.true.,R_LATT=.true.,IMPOSE_SN=.true.)
   if (io_RIM/=0) goto 1   
   !
   RL_vol_disk=RL_vol
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=10,MENU=0)
   !
   call io_elemental(ID,I0=RIM_ng,DB_I0=RIM_ng_disk,&
&       VAR=' Coulombian RL components        ',CHECK=.true.,OP=(/"<="/))
   call io_elemental(ID,L0=RIM_is_diagonal,&
&       VAR=' Coulombian diagonal components  ',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=RIM_n_rand_pts,&
&       VAR=' RIM random points               ',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,R0=RIM_RL_vol,&
&       VAR=' RIM  RL volume             [a.u.]')
   call io_elemental(ID,R0=RL_vol_disk,&
&       VAR=' Real RL volume             [a.u.]')
   call io_elemental(ID,I0=RIM_id_epsm1_reference,&
&       VAR=' Eps^-1 reference component       ',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,R1=RIM_epsm1,&
&       VAR=' Eps^-1 components                ',CHECK=.true.,OP=(/"==","==","=="/))
   call io_elemental(ID,R0=RIM_anisotropy,&
&       VAR=' RIM anysotropy factor            ')
   !
   call io_elemental(ID,VAR="",VAR_SZ=0)
   io_RIM=io_status(ID)
   if (io_RIM/=0) goto 1
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   if (read_is_on(ID)) then
     YAMBO_ALLOC(RIM_qpg,(nqibz,RIM_ng,RIM_ng))
   endif
   !
   call io_bulk(ID,VAR="RIM_qpg",VAR_SZ=(/shape(RIM_qpg)/))
   call io_bulk(ID,R3=RIM_qpg)
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function
