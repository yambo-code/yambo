!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine io_disconnect(ID,FORCE)
 !
 use IO_m,       ONLY:netcdf_call,PLAIN_file_is_open,NETCDF_file_is_open,io_unit,io_type,&
&                     close_is_on,io_status,IO_file,nf90_close,io_resetable,io_reset
 !
 implicit none
 !
 integer           :: ID,NC_ERR
 logical, optional :: FORCE
 logical           :: I_OPEN
 !
 logical :: FORCE_
 !
 I_OPEN=.FALSE.
 !
 FORCE_=.FALSE.
 if (present(FORCE)) FORCE_=FORCE
 !
 if ( (.not.FORCE_) .and. (.not.close_is_on(ID)) .and. (io_status(ID)>=0) ) return
 !
 if (io_type(ID)==-2) then
   I_OPEN=PLAIN_file_is_open(IO_file(ID))
   if (I_OPEN) close(unit=io_unit(ID))
 else
   I_OPEN=NETCDF_file_is_open(ID)
   if (I_OPEN) call netcdf_call(nf90_close(io_unit(ID)),ID)
 endif
 !
 if (I_OPEN) io_unit(ID)=0
 !
 if (io_resetable(ID)) call io_reset(ID)
 !
end subroutine
