!
! License-Identifier: GPL
!
! Copyright (C) 2025 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS CA
!
subroutine io_disconnect(ID, FORCE)
  !
  use IO_m,       ONLY: hdf5_call, PLAIN_file_is_open, HDF5_file_is_open, io_unit, io_type, &
                        close_is_on, io_status, IO_file, io_resetable, io_reset
  use hdf5,       ONLY: h5fclose_f,HID_T
  !
  implicit none
  !

  integer	    :: ID
  integer(HID_T)    :: ID8
  integer           :: ierr
  logical, optional :: FORCE
  logical           :: I_OPEN
  !
  logical :: FORCE_
  !
  I_OPEN = .FALSE.
  !
  FORCE_ = .FALSE.
  if (present(FORCE)) FORCE_ = FORCE
  !
  ! Check if the file should be closed
  if ((.not. FORCE_) .and. (.not. close_is_on(ID)) .and. (io_status(ID) >= 0)) return
  !
  ! Handle file closure based on file type
  if (io_type(ID) == -2) then
    ! Plain file (non-HDF5)
    I_OPEN = PLAIN_file_is_open(IO_file(ID))
    if (I_OPEN) close(unit=io_unit(ID))
  else
    ! HDF5 file
    ID8=int8(ID)
    I_OPEN = HDF5_file_is_open(ID8)
    if (I_OPEN) then
      call h5fclose_f(io_unit(ID), ierr)
      call hdf5_call(ierr, ID)
    end if
  end if
  !
  ! Reset the file unit if the file was open
  if (I_OPEN) io_unit(ID) = 0
  !
  ! Reset the file if necessary
  if (io_resetable(ID)) call io_reset(ID)
  !
end subroutine io_disconnect
