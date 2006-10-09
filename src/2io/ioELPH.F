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
 integer function ioELPH(ID)
!===========================
 use pars,           ONLY:SP,schlen
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,&
&                         io_elemental,io_status,io_bulk,io_header,&
&                         read_is_on,write_is_on,RD_CL,RD_CL_IF_END,io_action,&
&                         io_mode,DUMP
 use ELPH,           ONLY:ph_modes,elph_bands,elph_alloc,ph_freqs_sq,elph_dV,&
&                         QP_PH_n_G_bands
 use R_lattice,      ONLY:nkbz,nqibz
 use matrix_operate, ONLY:mat_c2r,mat_r2c
 implicit none
 integer      ::ID
!ws
 integer                ::iq,dV_dim,im,ik,i1,i2
 real(SP), allocatable  ::elph_dV_disk(:,:,:)
 character(schlen)      ::var_name

 ioELPH=io_connect(desc='elph',type=2,ID=ID)
 if (ioELPH/=0) goto 1

 if (any((/io_sec(ID,:)==1/))) then

   ioELPH=io_header(ID)
   if (ioELPH/=0) goto 1

   call io_elemental(ID,VAR="PARS",VAR_SZ=3,MENU=1)
   call io_elemental(ID,&
&       VAR=" Phonon modes           :",I0=ph_modes,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR=" El-PH bands range      :",I1=elph_bands,CHECK=.true.,OP=(/"==","=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)

   if (io_mode(ID)==DUMP.or.write_is_on(ID)) then
     QP_PH_n_G_bands=elph_bands(2)
   endif

   ioELPH=io_status(ID)
   if (ioELPH/=0) goto 1

 endif

 iq=maxval(io_sec(ID,:))-1
 if (iq<=0) goto 1
!
! Allocation
!
 if (read_is_on(ID)) call elph_alloc()
!
! Manage RD_CL_IF_END
!
 if (io_action(ID)==RD_CL_IF_END.and.iq==nqibz) io_action(ID)=RD_CL
!
! PH frequencies
!
 write (var_name,'(a,i3.3)') "PH_FREQS",iq
 call io_bulk(ID,VAR=trim(var_name),VAR_SZ=(/ph_modes/))
 call io_bulk(ID,R1=ph_freqs_sq(iq,:))
! 
! ELPH_dV
!
 dV_dim=elph_bands(2)-elph_bands(1)+1
 allocate(elph_dV_disk(dV_dim*dV_dim,ph_modes,2))

 write (var_name,'(a,i3.3)') "ELPH_dV_Q",iq
 call io_bulk(ID,VAR=trim(var_name),VAR_SZ=(/dV_dim*dV_dim,ph_modes,2,nkbz/) )

 do ik=1,nkbz
   if (write_is_on(ID)) then
     forall(im=1:ph_modes,i1=1:dV_dim,i2=1:dV_dim) &
&          elph_dV_disk((i1-1)*dV_dim+i2,im,1)=real(elph_dV(ik,im,i1,i2))
     forall(im=1:ph_modes,i1=1:dV_dim,i2=1:dV_dim) &
&          elph_dV_disk((i1-1)*dV_dim+i2,im,2)=aimag(elph_dV(ik,im,i1,i2))
   endif

   call io_bulk(ID,R3=elph_dV_disk,IPOS=(/1,1,1,ik/))

   if (read_is_on(ID)) then
     forall(im=1:ph_modes,i1=1:dV_dim,i2=1:dV_dim) &
&          elph_dV(ik,im,i1,i2)=elph_dV_disk((i1-1)*dV_dim+i2,im,1)+&
&                 (0._SP,1._SP)*elph_dV_disk((i1-1)*dV_dim+i2,im,2)
   endif

 enddo

1 call io_disconnect(ID=ID)

!
  end function
