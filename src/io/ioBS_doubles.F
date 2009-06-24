!
! Copyright (C) 2000-2008 D. Sangalli and the YAMBO team 
!              http://www.yambo-code.org
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
integer function ioBS_doubles(ID)
 !
 use pars,          ONLY:SP,schlen
 use stderr,        ONLY:intc
 use BS,            ONLY:BS_identifier,BS_K_dim
 use BS_doubles,    ONLY:BS_mat_doubles,BS_K_doubles_dim,&
                         BS_doubles_table,BS_doubles_E,&
! DOUBLES CORR PROJECT
&                        doubles_corr
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,&
&                        io_elemental,io_status,io_bulk,&
&                        io_header,write_is_on,read_is_on
 use electrons,     ONLY:n_sp_pol
 implicit none
 integer     :: ID
 !
 ! Work Space
 !
 integer          :: is,id2,icin,ncin
 character(schlen):: db_name
 real(SP)         :: BS_mat_doubles_disk(2,BS_K_dim,BS_K_doubles_dim)
 !
 db_name='BS_C_doubles'
 !
 ioBS_doubles=io_connect(desc=trim(db_name),type=2,ID=ID)
 if (ioBS_doubles/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! As this DB must respect the BS ideentifier I only need to
   ! check the K QP corrections                    <----------------- ?????????
   !
   ioBS_doubles=io_header(ID,XC_KIND="K_E force")         !<----------- ??
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=1,MENU=0)
   !
   call io_elemental(ID,VAR=&
&       " BSK Identifier                  :",I0=BS_identifier,CHECK=.TRUE.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   ioBS_doubles=io_status(ID)
   if (ioBS_doubles/=0) goto 1
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   ! BS_doubles_table
   !
   call io_bulk(ID,'BS_DOUBLES_TABLE',VAR_SZ=(/BS_K_doubles_dim,4/))
   call io_bulk(ID,I2=BS_doubles_table(:BS_K_doubles_dim,:))
   !
   ! BS_E_doubles
   !
   call io_bulk(ID,'BS_E_DOUBLES',VAR_SZ=(/BS_K_doubles_dim/))
   call io_bulk(ID,R1=BS_doubles_E(:BS_K_doubles_dim))
   !
   ! Doubles, coupling terms of the kernel
   !
   ncin=2
   if(doubles_corr) ncin=1
   !
   do icin=1,ncin
     !
     if (write_is_on(ID)) BS_mat_doubles_disk(1,:,:)=real(BS_mat_doubles(:,:,icin))
     if (write_is_on(ID)) BS_mat_doubles_disk(2,:,:)=aimag(BS_mat_doubles(:,:,icin))
     !
     call io_bulk(ID,'BS_DOUBLES_REAL_KIN'//trim(intc(icin)),VAR_SZ=(/BS_K_dim,BS_K_doubles_dim/))
     call io_bulk(ID,R2=BS_mat_doubles_disk(1,:,:))
     call io_bulk(ID,'BS_DOUBLES_IMG_KIN'//trim(intc(icin)),VAR_SZ=(/BS_K_dim,BS_K_doubles_dim/))
     call io_bulk(ID,R2=BS_mat_doubles_disk(2,:,:))
     !
     if (read_is_on(ID)) BS_mat_doubles(:,:,icin)=&
&                 BS_mat_doubles_disk(1,:,:)+(0._SP,1._SP)*BS_mat_doubles_disk(2,:,:)
     !
   enddo
   !
 endif
 !
1 call io_disconnect(ID)
 !
end function
