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
integer function ioBSS_diago(iq,BS_H_dim,bsE,bsR,ID)
 !
 use pars,          ONLY:SP,schlen,IP
 use BS,            ONLY:BS_identifier,BSS_q0,BSS_write_eig_2_db,&
&                        BS_eh_table,BS_mat
 use IO_m,          ONLY:io_connect,io_disconnect,io_sec,&
&                        io_elemental,io_status,io_bulk,&
&                        io_header,write_is_on,read_is_on
 use electrons,     ONLY:n_sp_pol
 implicit none
 integer     :: iq,ID,BS_H_dim
 real(SP)    :: bsE(2,BS_H_dim),bsR(2,BS_H_dim)
 !
 ! Work Space
 !
 integer          :: i1
 real(SP)         :: BS_mat_disk(2,BS_H_dim)
 character(schlen):: db_name
 !
 write (db_name,'(a,i2.2)') 'BS_diago_Q',iq
 !
 ioBSS_diago=io_connect(desc=trim(db_name),type=2,ID=ID)
 if (ioBSS_diago/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! As this DB must respect the BS ideentifier I only need to
   ! check the K QP corrections
   !
   ioBSS_diago=io_header(ID,XC_KIND="K_E force")
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=5,MENU=0)
   call io_elemental(ID,VAR=&
&       " BSK Identifier                  :",I0=BS_identifier,CHECK=.TRUE.,OP=(/"=="/))
   call io_elemental(ID,VAR=&
&       " Excitonic eigenstates included  :",L0=BSS_write_eig_2_db,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR=&
&       " Field direction                 :",R1=BSS_q0,CHECK=.true.,OP=(/"==","==","=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   ! header return
   !
   if (ioBSS_diago/=0) goto 1
   !
   ioBSS_diago=io_status(ID)
   if (ioBSS_diago/=0) goto 1
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   call io_bulk(ID,'BS_Energies',VAR_SZ=(/2,BS_H_dim/))
   call io_bulk(ID,R2=bsE)
   call io_bulk(ID,'BS_Residuals',VAR_SZ=(/2,BS_H_dim/))
   call io_bulk(ID,R2=bsR)
 endif
 !
 if (any((/io_sec(ID,:)==3/))) then
   ! 
   ! If not switched on skip this section
   !
   if (.not.BSS_write_eig_2_db) then
     ioBSS_diago=-1
     goto 1
   endif
   !
   ! BS_eh_table
   !
   call io_bulk(ID,'BS_TABLE',VAR_SZ=(/BS_H_dim,3+n_sp_pol-1/))
   call io_bulk(ID,I2=BS_eh_table)
   !
   ! Eigenstates (resonant only matrix)
   !
   call io_bulk(ID,'BS_EIGENSTATES',VAR_SZ=(/2,BS_H_dim,BS_H_dim/))
   !
   do i1=1,BS_H_dim
     !
     if (write_is_on(ID)) BS_mat_disk(1,:)=real(BS_mat(:,i1))
     if (write_is_on(ID)) BS_mat_disk(2,:)=aimag(BS_mat(:,i1))
     !
     call io_bulk(ID,R2=BS_mat_disk,IPOS=(/1,1,i1/))
     !
     if (read_is_on(ID)) BS_mat(:,i1)=BS_mat_disk(1,:)+(0._SP,1._SP)*BS_mat_disk(2,:)
     !
   enddo
   !
 endif
 !
1 call io_disconnect(ID)
 !
end function
