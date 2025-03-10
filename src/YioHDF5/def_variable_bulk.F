!
! License-Identifier: GPL
!
! Copyright (C) 2025 The Yambo Team
!
! Authors (see AUTHORS file for details): AM HM DS CA
!
subroutine def_variable_bulk(ID, VAR, VAR_ID, VAR_SZ, VAR_KIND, dim_names, par_io_kind, silent, chunksize)
  !
  use hdf5
  use com,        ONLY: fat_log
  use pars,       ONLY: SP, DP, IP, LP, schlen, CR_YIO, LP_YIO, IP_YIO
  use parallel_m, ONLY: ncpu, mpi_comm_null
  use IO_m,       ONLY: write_is_on, read_is_on, io_unit, hdf5_call, io_hdf5_var, &
                        hdf5_dim, io_file, io_PAR_comm
  !
  implicit none
  !
  integer,      intent(in)  :: ID
  character(*), intent(in)  :: VAR
  integer,      intent(in)  :: VAR_ID
  integer,      intent(in)  :: VAR_SZ(:)
  integer,      intent(in)  :: VAR_KIND
  character(schlen), optional, intent(in) :: dim_names(:)
  character(*),      optional, intent(in) :: par_io_kind
  logical,           optional, intent(in) :: silent
  integer,           optional, intent(in) :: chunksize(:) ! Chunking parameter
  !
  ! Work Space
  !
  integer(hid_t) :: file_id, dataset_id, dataspace_id, plist_id
  integer(hsize_t), allocatable :: dims(:), chunk_dims(:)
  integer :: hdferr, hdf5_par_io_kind, hdf5_var_kind
  logical :: silent_
#if defined _HDF5_COMPRESSION
  integer :: shuffle, deflate, deflate_level
#endif
  !
  ! Convert VAR_KIND to HDF5 data type
  if (VAR_KIND == SP)     hdf5_var_kind = H5T_NATIVE_REAL
  if (VAR_KIND == DP)     hdf5_var_kind = H5T_NATIVE_DOUBLE
  if (VAR_KIND == IP_YIO) hdf5_var_kind = H5T_NATIVE_INTEGER
  if (VAR_KIND == LP_YIO) hdf5_var_kind = H5T_NATIVE_B8
  if (VAR_KIND == CR_YIO) hdf5_var_kind = H5T_NATIVE_CHARACTER
  !
  silent_ = .not. fat_log
  if (present(silent)) silent_ = silent
  !
#if defined _PAR_IO
  !
  ! Define parallel I/O kind
  !
  if (present(par_io_kind) .and. ncpu > 1) then
    if (trim(par_io_kind) == "independent") hdf5_par_io_kind = H5FD_MPIO_INDEPENDENT_F
    if (trim(par_io_kind) == "collective")  hdf5_par_io_kind = H5FD_MPIO_COLLECTIVE_F
  endif
  !
#endif
  !
  file_id = io_unit(ID)
  !
  if (write_is_on(ID)) then
    !
    ! Create dataset
    allocate(dims(size(VAR_SZ)))
    do j1 = 1, size(VAR_SZ)
      if (present(dim_names)) then
        dims(j1) = hdf5_dim(ID, VAR_SZ(j1), dim_names(j1))
      else
        dims(j1) = hdf5_dim(ID, VAR_SZ(j1))
      endif
    enddo
    !
!    call h5screate_simple_f(size(dims), dims, dataspace_id, hdferr)
!    call hdf5_call(hdferr, ID, VAR=VAR)
    !
    ! Check if dataset already exists
!    hdferr = h5dopen_f(file_id, VAR, dataset_id, hdferr)
    if (hdferr >= 0) then
      ! Dataset exists, check dimensions
      integer(hsize_t), allocatable :: existing_dims(:)
      integer :: ndims
!      call h5dget_space_f(dataset_id, dataspace_id, hdferr)
!      call h5sget_simple_extent_ndims_f(dataspace_id, ndims, hdferr)
      allocate(existing_dims(ndims))
!      call h5sget_simple_extent_dims_f(dataspace_id, existing_dims, hdferr)
      if (any(dims /= existing_dims)) then
        call error(' Trying to overwrite variable '//trim(VAR)//' in '//trim(io_file(ID))//' with wrong dimensions ')
      endif
      if (.not. silent_) call warning(' Overwriting variable '//trim(VAR)//' in file '//trim(io_file(ID)))
      deallocate(existing_dims)
!      call h5dclose_f(dataset_id, hdferr)
    else
      ! Dataset does not exist, create it
      if (.not. silent_) call warning(' Re-defining variable '//trim(VAR)//' in file '//trim(io_file(ID)))
!      call h5dcreate_f(file_id, VAR, hdf5_var_kind, dataspace_id, dataset_id, hdferr)
      call hdf5_call(hdferr, ID, VAR=VAR)
      !
#if defined _PAR_IO
      if (present(par_io_kind) .and. io_PAR_comm(ID) /= mpi_comm_null) then
!        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, hdferr)
!        call h5pset_dxpl_mpio_f(plist_id, hdf5_par_io_kind, hdferr)
        call hdf5_call(hdferr, ID, VAR=VAR)
      endif
      !
      ! Add chunking to the variable
      if (present(chunksize)) then
        if (product(chunksize) > 0) then
          allocate(chunk_dims(size(chunksize)))
          chunk_dims = chunksize
!          call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, hdferr)
!          call h5pset_chunk_f(plist_id, size(chunk_dims), chunk_dims, hdferr)
          call hdf5_call(hdferr, ID, VAR=VAR)
          deallocate(chunk_dims)
        endif
      endif
#endif
      !
#if defined _HDF5_COMPRESSION
      ! Enable compression
      shuffle = 1
      deflate = 1
      deflate_level = 2
!      call h5pset_shuffle_f(plist_id, hdferr)
!      call h5pset_deflate_f(plist_id, deflate_level, hdferr)
!      call hdf5_call(hdferr, ID, VAR=VAR)
#endif
      !
!      call h5dclose_f(dataset_id, hdferr)
    endif
    !
!    call h5sclose_f(dataspace_id, hdferr)
    deallocate(dims)
    !
  else if (read_is_on(ID)) then
    !
    ! Open dataset for reading
!    call h5dopen_f(file_id, VAR, dataset_id, hdferr)
!    call hdf5_call(hdferr, ID, VAR=VAR)
!    call h5dclose_f(dataset_id, hdferr)
    !
#if defined _HDF5_COMPRESSION
    ! Check compression settings
!    call h5dget_create_plist_f(dataset_id, plist_id, hdferr)
!    call h5pget_shuffle_f(plist_id, shuffle, hdferr)
!    call h5pget_deflate_f(plist_id, deflate, deflate_level, hdferr)
!    call hdf5_call(hdferr, ID, VAR=VAR)
#endif
    !
  endif
  !
  return
  !
end subroutine def_variable_bulk
