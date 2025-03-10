!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM HM DS
!
subroutine io_bulk(ID, VAR, VAR_SZ, I0, I1, I2, I3, I4, I5, &
                   R0, R1, R2, R3, R4, R5, &
                   D0, D1, D2, D3, D4, D5, &
                   C0, C1, C2, C3, C4, C5, &
                   Z0, Z1, Z2, Z3, Z4, Z5, &
                   L0, L1, L2, L3, L4, IPOS)
  !
  use hdf5
  use pars,          ONLY: SP, DP, LP
  use com,           ONLY: fat_log
  use iso_c_binding, ONLY: C_LOC, C_F_POINTER
  use IO_m,          ONLY: write_is_on, io_unit, hdf5_call, io_hdf5_var, &
                           hdf5_dim, read_is_on, io_file
  !
  implicit none
  !
  integer :: ID, ierr
  character(*), optional :: VAR
  integer, optional :: VAR_SZ(:), IPOS(:)
  integer, optional :: I0, I1(:), I2(:, :), I3(:, :, :), I4(:, :, :, :), I5(:, :, :, :, :)
  real(SP), optional :: R0, R1(:), R2(:, :), R3(:, :, :), R4(:, :, :, :), R5(:, :, :, :, :)
  real(DP), optional :: D0, D1(:), D2(:, :), D3(:, :, :), D4(:, :, :, :), D5(:, :, :, :, :)
  complex(SP), optional, target :: C0, C1(:), C2(:, :), C3(:, :, :), C4(:, :, :, :), C5(:, :, :, :, :)
  complex(DP), optional, target :: Z0, Z1(:), Z2(:, :), Z3(:, :, :), Z4(:, :, :, :), Z5(:, :, :, :, :)
  !
  ! These types can be used instead of logical that 
  ! are not supported by HDF5
  !
  integer(LP), optional :: L0, L1(:), L2(:, :), L3(:, :, :), L4(:, :, :, :)
  !
  ! Work Space
  !
  real(SP), pointer :: RC0(:), RC1(:, :), RC2(:, :, :), RC3(:, :, :, :), RC4(:, :, :, :, :), RC5(:, :, :, :, :, :)
  real(DP), pointer :: RZ0(:), RZ1(:, :), RZ2(:, :, :), RZ3(:, :, :, :), RZ4(:, :, :, :, :), RZ5(:, :, :, :, :, :)
  integer, allocatable :: var_dim(:), ipos_loc(:), isize_loc(:)
  integer :: j1, dims, dims_db
  integer, allocatable :: var_dim_db(:)
  integer(HID_T) :: file_id, dspace_id, dset_id
  integer(HSIZE_T), allocatable :: hdf5_dims(:)
  !
  ! Initialize HDF5
  call h5open_f(ierr)
  call hdf5_call(ierr, ID, VAR=VAR)
  !
  ! Variable definition and ID
  !
  if (present(VAR) .and. present(VAR_SZ)) then
    if (write_is_on(ID)) then
      ! Create or open the HDF5 file
!      call h5fcreate_f(trim(io_file(ID)), H5F_ACC_TRUNC_F, file_id, ierr)
!      call hdf5_call(ierr, ID, VAR=VAR)
      !
      ! Create dimensions
      allocate(var_dim(size(VAR_SZ)))
      allocate(hdf5_dims(size(VAR_SZ)))
      do j1 = 1, size(VAR_SZ)
!        var_dim(j1) = hdf5_dim(ID, VAR_SZ(j1))
        hdf5_dims(j1) = VAR_SZ(j1)
      end do
      !
      ! Create dataspace
!      call h5screate_simple_f(size(VAR_SZ), hdf5_dims, dspace_id, ierr)
!      call hdf5_call(ierr, ID, VAR=VAR)
      !
      ! Create dataset
!      call h5dcreate_f(file_id, VAR, H5T_NATIVE_FLOAT, dspace_id, dset_id, ierr)
!      call hdf5_call(ierr, ID, VAR=VAR)
      !
      ! Close resources
!      call h5sclose_f(dspace_id, ierr)
!      call h5dclose_f(dset_id, ierr)
!      call h5fclose_f(file_id, ierr)
      deallocate(var_dim, hdf5_dims)
    else if (read_is_on(ID)) then
      ! Open the HDF5 file
!      call h5fopen_f(trim(io_file(ID)), H5F_ACC_RDONLY_F, file_id, ierr)
!      call hdf5_call(ierr, ID, VAR=VAR)
      !
      ! Open dataset
!      call h5dopen_f(file_id, VAR, dset_id, ierr)
!      call hdf5_call(ierr, ID, VAR=VAR)
      !
      ! Close resources
!      call h5dclose_f(dset_id, ierr)
!      call h5fclose_f(file_id, ierr)
    end if
    return
  end if
  !
  ! Actual IO
  !
  ierr = 0
  dims = 0
  !
  if (present(L1) .or. present(I1) .or. present(R1) .or. present(D1)) dims = 1
  if (present(L2) .or. present(I2) .or. present(R2) .or. present(D2) .or. present(C1) .or. present(Z1)) dims = 2
  if (present(L3) .or. present(I3) .or. present(R3) .or. present(D3) .or. present(C2) .or. present(Z2)) dims = 3
  if (present(L4) .or. present(I4) .or. present(R4) .or. present(D4) .or. present(C3) .or. present(Z3)) dims = 4
  if (present(I5) .or. present(R5) .or. present(D5) .or. present(C4) .or. present(Z4)) dims = 5
  if (present(C5) .or. present(Z5)) dims = 6
  !
  if (dims > 0) then
    allocate(ipos_loc(dims), isize_loc(dims))
    if (present(IPOS)) ipos_loc = IPOS
    if (.not. present(IPOS)) ipos_loc = 1
  end if
  !
  ! Logical numbers / vectors / matrices
  !
  if (present(L0)) then
    if (write_is_on(ID)) then
!      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, L0, (/1/), ierr)
    else if (read_is_on(ID)) then
!      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, L0, (/1/), ierr)
    end if
!    call hdf5_call(ierr, ID, VAR=VAR)
  end if
  !
  ! Integer numbers / vectors / matrices
  !
  if (present(I0)) then
    if (write_is_on(ID)) then
!      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, I0, (/1/), ierr)
    else if (read_is_on(ID)) then
!      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, I0, (/1/), ierr)
    end if
!    call hdf5_call(ierr, ID, VAR=VAR)
  end if
  !
  ! Real numbers / vectors / matrices
  !
  if (present(R0)) then
    if (write_is_on(ID)) then
!      call h5dwrite_f(dset_id, H5T_NATIVE_FLOAT, R0, (/1/), ierr)
    else if (read_is_on(ID)) then
!      call h5dread_f(dset_id, H5T_NATIVE_FLOAT, R0, (/1/), ierr)
    end if
!    call hdf5_call(ierr, ID, VAR=VAR)
  end if
  !
  ! Double precision numbers / vectors / matrices
  !
  if (present(D0)) then
    if (write_is_on(ID)) then
!      call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, D0, (/1/), ierr)
    else if (read_is_on(ID)) then
!      call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, D0, (/1/), ierr)
    end if
!    call hdf5_call(ierr, ID, VAR=VAR)
  end if
  !
  ! Complex numbers / vectors / matrices
  !
  if (present(C0)) then
!    call C_F_POINTER(C_LOC(C0), RC0, (/2/))
    if (write_is_on(ID)) then
!      call h5dwrite_f(dset_id, H5T_NATIVE_FLOAT, RC0, (/1/), (/2/), ierr)
    else if (read_is_on(ID)) then
!      call h5dread_f(dset_id, H5T_NATIVE_FLOAT, RC0, (/1/), (/2/), ierr)
    end if
!    call hdf5_call(ierr, ID, VAR=VAR)
  end if
  !
  ! Double precision complex numbers / vectors / matrices
  !
  if (present(Z0)) then
!    call C_F_POINTER(C_LOC(Z0), RZ0, (/2/))
    if (write_is_on(ID)) then
!      call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, RZ0, (/1/), (/2/), ierr)
    else if (read_is_on(ID)) then
!      call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, RZ0, (/1/), (/2/), ierr)
    end if
!    call hdf5_call(ierr, ID, VAR=VAR)
  end if
  !
  ! Close resources
  if (allocated(ipos_loc)) deallocate(ipos_loc)
  if (allocated(isize_loc)) deallocate(isize_loc)
  !
  ! Finalize HDF5
!  call h5close_f(ierr)
!  call hdf5_call(ierr, ID, VAR=VAR)
  !
end subroutine io_bulk
