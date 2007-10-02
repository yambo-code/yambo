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
integer function ioBSS_Haydock(ID,it,Af,Bf,reached_treshold,Vnm1,Vn)
 !
 use pars,          ONLY:SP
 use QP_m,          ONLY:QP_ctl_E
 use BS,            ONLY:BS_identifier,BS_K_dim,BSS_q0
 use IO_m,          ONLY:io_connect,io_disconnect,io_elemental,&
&                        io_bulk,io_status,io_sec,io_mode,VERIFY,io_header
 implicit none
 integer  :: ID,it
 real(SP) :: reached_treshold
 real(SP) :: Af(2,it)
 real(SP) :: Bf(it+1)
 real(SP) :: Vnm1(2,BS_K_dim)
 real(SP) :: Vn(2,BS_K_dim)
 ! 
 ! Work Space
 !
 integer  :: it_on_disk
 !
 ioBSS_Haydock=io_connect(desc='Haydock_restart',type=2,ID=ID)
 if (ioBSS_Haydock<0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ioBSS_Haydock=io_header(ID,QP_ctl_K=.true.)
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=6,MENU=0)
   call io_elemental(ID,VAR=&
&       " BSK Identifier                  :",I0=BS_identifier,CHECK=.TRUE.,OP=(/"=="/))
   call io_elemental(ID,VAR=&
&       " Haydock Iteration               :",I0=it,DB_I0=it_on_disk)
   call io_elemental(ID,VAR=&
&       " Haydock reached Treshold        :",R0=reached_treshold,&
&                                            DB_R0=reached_treshold)
   call io_elemental(ID,VAR=&
&       " Field direction                 :",R1=BSS_q0,CHECK=.true.,OP=(/"==","==","=="/))
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   ! ioBSS_Haydock due to the header
   !
   if (ioBSS_Haydock<0) goto 1
   !
   ioBSS_Haydock=io_status(ID)
   if (ioBSS_Haydock<0) goto 1
   !
   ioBSS_Haydock=it_on_disk
   !
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   call io_bulk(ID,'Vnm1',VAR_SZ=(/2,BS_K_dim/))
   call io_bulk(ID,R2=Vnm1)
   call io_bulk(ID,'Vn',VAR_SZ=(/2,BS_K_dim/))
   call io_bulk(ID,R2=Vn)
   call io_bulk(ID,'A_factors',VAR_SZ=(/2,it/))
   call io_bulk(ID,R2=Af)
   call io_bulk(ID,'B_factors',VAR_SZ=(/it+1/))
   call io_bulk(ID,R1=Bf)
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function
