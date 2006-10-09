!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 integer function ioBS_diago(iq,BS_H_dim,bsE,bsR,ID)
!===================================================
 use pars
 use BS,            ONLY:BS_identifier,BSS_q0
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,&
&                        io_elemental,io_status,io_bulk,&
&                        io_header
 implicit none
 integer     :: iq,ID,BS_H_dim
 real(SP)    :: bsE(2,BS_H_dim),bsR(2,BS_H_dim)
!ws
 character(schlen)   ::db_name

 write (db_name,'(a,i2.2)') 'BS_diago_Q',iq

 ioBS_diago=io_connect(desc=trim(db_name),type=2,ID=ID)
 if (ioBS_diago/=0) goto 1

 if (any((/io_sec(ID,:)==1/))) then
!
! As this DB must respect the BS ideentifier I only need to
! check the K QP corrections
!
   ioBS_diago=io_header(ID,QP_ctl_K=.true.)

   call io_elemental(ID,VAR="PARS",VAR_SZ=4,MENU=0)
   call io_elemental(ID,VAR=&
&       " BSK Identifier                  :",I0=BS_identifier,CHECK=.TRUE.,OP=(/"=="/))
   call io_elemental(ID,VAR=&
&       " Field direction                 :",R1=BSS_q0,CHECK=.true.,OP=(/"==","==","=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
!
! header return
!
   if (ioBS_diago/=0) goto 1

   ioBS_diago=io_status(ID)
   if (ioBS_diago/=0) goto 1
 endif
!
 if (any((/io_sec(ID,:)==2/))) then
   call io_bulk(ID,'BS_Energies',VAR_SZ=(/2,BS_H_dim/))
   call io_bulk(ID,R2=bsE)
   call io_bulk(ID,'BS_Residuals',VAR_SZ=(/2,BS_H_dim/))
   call io_bulk(ID,R2=bsR)
 endif
!
1 call io_disconnect(ID)
!
 end function
