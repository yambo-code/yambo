!
! License-Identifier: GPL
!
! Copyright (C) 2025 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS CA
!
subroutine def_variable_elemental(ID, VAR, VAR_SZ, VAR_KIND, MENU, dim_names, par_io_kind)
  !
  use hdf5
  use pars,       ONLY: SP, DP, IP, LP, schlen, lchlen, DB_menu_length
  use pars,       ONLY: IP_YIO, LP_YIO, SP_YIO, DP_YIO, CR_YIO
#if !defined _io_lib
  use drivers,    ONLY: list_dbs
#endif
  use com,        ONLY: msg
  use parallel_m, ONLY: ncpu
  use stderr,     ONLY: STRING_pack
  use IO_m,       ONLY: write_is_on, io_unit, hdf5_call, io_hdf5_var, &
                        hdf5_dim, io_rec_pos, read_is_on, io_mode, VERIFY, &
                        io_status, DUMP, REP, LOG, io_com, io_file, &
                        io_code_version, io_serial_number, io_code_revision, IO_INCOMPATIBLE_VAR
  !
  implicit none
  !
  integer,      intent(in) :: ID
  character(*), intent(in) :: VAR
  integer,      intent(in) :: VAR_SZ
  integer,      intent(in) :: VAR_KIND
  integer,      intent(in) :: MENU          ! MENU = 1 (on) 0 (off)
  character(schlen), optional, intent(in) :: dim_names(:)
  character(*),      optional, intent(in) :: par_io_kind
  !
  ! Work Space
  !
  character(lchlen) :: ch
  character(1)      :: msg_where
  character(3)      :: MODE
  integer           :: hdferr, hdf5_par_io_kind, hdf5_var_kind
  integer(hid_t)    :: file_id, dataset_id, dataspace_id, plist_id
  integer(hsize_t), allocatable :: dims(:)
  !
  if (VAR_SZ > 0) then
    !
    io_rec_pos(ID) = 1
    !
    ! Convert VAR_KIND to HDF5 data type
    if (VAR_KIND == SP_YIO)     hdf5_var_kind = H5T_NATIVE_REAL
    if (VAR_KIND == DP_YIO)     hdf5_var_kind = H5T_NATIVE_DOUBLE
    if (VAR_KIND == IP_YIO)     hdf5_var_kind = H5T_NATIVE_INTEGER
    if (VAR_KIND == LP_YIO)     hdf5_var_kind = H5T_NATIVE_B8
    if (VAR_KIND == CR_YIO)     hdf5_var_kind = H5T_NATIVE_CHARACTER
    !
#if defined _PAR_IO
    !
    ! Define parallel I/O kind
    !
    if (present(par_io_kind) .and. ncpu > 1) then
      if (trim(par_io_kind) == "independent") hdf5_par_io_kind = H5FD_MPIO_INDEPENDENT_F
      if (trim(par_io_kind) == "collective")  hdf5_par_io_kind = H5FD_MPIO_COLLECTIVE_F
    endif
#endif
    !
    file_id = io_unit(ID)
    !
    if (write_is_on(ID)) then
      ! Create dataset
      if (VAR_KIND == 0) then
        allocate(dims(2))
        dims = [hdf5_dim(ID, schlen), hdf5_dim(ID, VAR_SZ)]
      else
        allocate(dims(1))
        dims = [hdf5_dim(ID, VAR_SZ)]
      endif
      !
      call h5screate_simple_f(size(dims), dims, dataspace_id, hdferr)
      call hdf5_call(hdferr, ID, VAR=VAR)
      !
      call h5dcreate_f(file_id, VAR, hdf5_var_kind, dataspace_id, dataset_id, hdferr)
      call hdf5_call(hdferr, ID, VAR=VAR)
      !
      call h5sclose_f(dataspace_id, hdferr)
      call h5dclose_f(dataset_id, hdferr)
      deallocate(dims)
      !
#if defined _PAR_IO
      if (present(par_io_kind) .and. ncpu > 1) then
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, hdferr)
        call h5pset_dxpl_mpio_f(plist_id, hdf5_par_io_kind, hdferr)
        call hdf5_call(hdferr, ID, VAR=VAR)
      endif
#endif
      !
    else if (read_is_on(ID)) then
      ! Open dataset
      call h5dopen_f(file_id, VAR, dataset_id, hdferr)
      call hdf5_call(hdferr, ID, VAR=VAR)
      call h5dclose_f(dataset_id, hdferr)
    endif
    !
  endif
  !
  if (MENU == 1) then
    !
    msg_where = ""
    if (io_com(ID) == REP)            msg_where = "r"
#if !defined _io_lib
    if (io_com(ID) == LOG .or. list_dbs) msg_where = "s"
#endif
    !
    if (VAR_SZ > 0) then
      if (write_is_on(ID)) MODE = "[WR"
      if (read_is_on(ID))  MODE = "[RD"
      ch = STRING_pack(MODE, trim(io_file(ID)), "]", repeat('-', max(DB_menu_length - len_trim(io_file(ID)) - 4, 1)))
    else
      !
      write(ch, '(a,i6.6,2a,3(a,i2.2),a,i5.5,a)') '- S/N ', io_serial_number(ID), ' ', &
           repeat('-', DB_menu_length - 35), '- v.', io_code_version(ID, 1), '.', io_code_version(ID, 2), &
           '.', io_code_version(ID, 3), ' r.', io_code_revision(ID, 1), ' -'
      !
    endif
    !
    call msg(msg_where, trim(ch))
    !
  endif
  !
  return
  !
end subroutine def_variable_elemental
