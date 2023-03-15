!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine ELPH_load_indexes(k,E,q,qindx_ID,qindx_ID_frag)
 !
 use units,               ONLY:HA2EV
 use pars,                ONLY:schlen,SP
 use stderr,              ONLY:intc
 use com,                 ONLY:msg
 use YPP_ELPH,            ONLY:l_use_qindxB
 use electrons,           ONLY:levels
 use R_lattice,           ONLY:bz_samp,bse_scattering,coll_scattering
 use parser_m,            ONLY:parser
 use IO_int,              ONLY:io_control
 use IO_m,                ONLY:OP_APP_CL,REP,OP_RD_CL
 use parallel_int,        ONLY:PARALLEL_global_indexes
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 type(levels)  ::E
 integer       ::qindx_ID,qindx_ID_frag
 !
 ! Work Space...
 !
 integer            ::io_err(3),ID
 integer, external  ::io_QINDX,qindx_B_init
 !
 if(l_use_qindxB) then
   !
   if(.not.bse_scattering) call error(' Please run setup with BSEscatt flag ')
   !
   ! here qindx_B is allocated
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/5/),ID=ID)
   io_err(1)=io_QINDX(k,q,ID,'minus_q')
   if (io_err(1)/=0) then
     call msg('s',"Missing k/q scattering database")
     call error("Please run a setup with the BSEscatt flag activated")
   endif
   !
   ! Initialize qindx_B IO
   ! =============================
   io_err(2)=qindx_B_init(qindx_ID,qindx_ID_frag)
   if(io_err(2)/=0) call error("Error reading qindx_B")
   !
 else
   ! otherwise qindx_C is allocated
   !
   if(.not.coll_scattering) call error(' Please run setup with COLLscatt flag ')
   !
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/6/),ID=ID)
   io_err(1)=io_QINDX(k,q,ID,'minus_q')
   if (io_err(1)/=0) then
     call msg('s',"Missing k/q scattering database")
     call error("Please (re)run the setup")
   endif
 endif
 !
end subroutine
!
!
subroutine ELPH_switch_X_indexes_convention(kpts,qpts,convention)
 !
 use com,                 ONLY:msg
 use IO_m,                ONLY:OP_RD_CL,DUMP,NONE
 use IO_int,              ONLY:io_control
 use R_lattice,           ONLY:bz_samp,qindx_free
 !
 implicit none
 !
 type(bz_samp)            :: kpts,qpts
 character(*), intent(in) :: convention
 !
 integer, external :: io_QINDX
 integer           :: ID_qindx,io_err_qindx
 !
 call msg('s',' Switching qindx_X table to '//convention//' convention')
 call qindx_free('X') 
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/3/),MODE=DUMP,ID=ID_qindx)
 io_err_qindx=io_QINDX(kpts,qpts,ID_qindx,convention)
 !
 if (io_err_qindx/=0 .and. convention=="plus_q") &
 & call error("Run yambo_ph setup setting K_grids='X plus_q'")
 !
 if (io_err_qindx/=0 .and. convention=="minus_q") &
 & call error("ndb.kindx file missing, please run yambo setup")
 !
end subroutine
