!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
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
