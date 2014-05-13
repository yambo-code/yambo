!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH
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
 integer function io_loc(loc,upperlim, lowerlim, ID)
   use pars,              ONLY : SP
   use IO_m,              ONLY : io_connect,io_disconnect,io_sec,&
&                                io_elemental,io_status,io_bulk,&
&                                read_is_on,write_is_on,io_header
   use R_lattice,         ONLY : nkibz
   use electrons,         ONLY : n_bands
   implicit none
   integer,     intent(inout) :: ID
   real(SP),    intent(inout) :: upperlim, lowerlim
   real(SP),    intent(inout) :: loc(n_bands, nkibz)

   io_loc = io_connect(desc='wfcloc',type=1,ID=ID)
   if (io_loc/=0) goto 1

   io_loc=io_header(ID)
   call io_elemental(ID,VAR="PARS",VAR_SZ=4,MENU=1)
   call io_elemental(ID,VAR=" Bands             :",I0=n_bands)
   call io_elemental(ID,VAR=" K-points          :",I0=nkibz)
   call io_elemental(ID,VAR=" Lower limit (frac):",R0=lowerlim)
   call io_elemental(ID,VAR=" Upper limit (frac):",R0=upperlim)

   call io_bulk(ID, VAR="LOCALIZATION", VAR_SZ=shape(loc) )
   call io_bulk(ID, R2=loc )

   io_loc=io_status(ID)
1  call io_disconnect(ID=ID)

   return
 end function io_loc

