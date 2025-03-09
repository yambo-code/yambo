!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM HM DS
!
subroutine io_variable_bulk(ID, VAR_ID, I0, I1, I2, I3, I4, I5,    &
                                     &  R0, R1, R2, R3, R4, R5,    &
                                     &  D0, D1, D2, D3, D4, D5,    &
                                     &  C0, C1, C2, C3, C4, C5, C6,&
                                     &  Z0, Z1, Z2, Z3, Z4, Z5,    &
                                     &  L0, L1, L2, L3, L4,        &
                                     & CH0,CH1,CH2,CH3,CH4,        &
                                     & STR0,STR1,              IPOS)
  !
  use pars,          ONLY: SP, DP, LP
  use iso_c_binding, ONLY: C_LOC, C_F_POINTER
  use IO_m,          ONLY: write_is_on, read_is_on, io_unit, hdf5_call, io_hdf5_var
  use hdf5
  !
  implicit none
  !
  integer, intent(in)                  :: ID, VAR_ID
  integer, optional, intent(in)        :: IPOS(:)
  integer, optional, intent(inout)     :: I0, I1(:), I2(:,:), I3(:,:,:), I4(:,:,:,:), I5(:,:,:,:,:)
  real(SP), optional, intent(inout)    :: R0, R1(:), R2(:,:), R3(:,:,:), R4(:,:,:,:), R5(:,:,:,:,:)
  real(DP), optional, intent(inout)    :: D0, D1(:), D2(:,:), D3(:,:,:), D4(:,:,:,:), D5(:,:,:,:,:)
  complex(SP), optional, target, intent(inout) :: C0, C1(:)
  complex(DP), optional, target, intent(inout) :: Z0, Z1(:)
  complex(SP), optional, target, contiguous, intent(inout) :: C2(:,:), C3(:,:,:), C4(:,:,:,:), C5(:,:,:,:,:), C6(:,:,:,:,:,:)
  complex(DP), optional, target, contiguous, intent(inout) :: Z2(:,:), Z3(:,:,:), Z4(:,:,:,:), Z5(:,:,:,:,:)
  !
  ! These types can be used instead of logical that are not supported by HDF5
  !
  integer(LP), optional, intent(inout) :: L0, L1(:), L2(:,:), L3(:,:,:), L4(:,:,:,:)
  !
  ! Characters (H5T_NATIVE_CHAR) in HDF5 occupy the same space as L0 (H5T_NATIVE_INTEGER)
  !
  character, optional, intent(inout)   :: CH0, CH1(:), CH2(:,:), CH3(:,:,:), CH4(:,:,:,:)
  character(*), optional, intent(inout) :: STR0, STR1(:)
  !
  ! Work Space
  !
  real(SP), pointer, contiguous :: RC0(:), RC1(:,:), RC2(:,:,:), RC3(:,:,:,:), RC4(:,:,:,:,:), RC5(:,:,:,:,:,:), RC6(:,:,:,:,:,:,:)
  real(DP), pointer, contiguous :: RZ0(:), RZ1(:,:), RZ2(:,:,:), RZ3(:,:,:,:), RZ4(:,:,:,:,:), RZ5(:,:,:,:,:,:)
  !
  integer, allocatable :: ipos_loc(:), isize_loc(:)
  integer              :: ierr, dims
  integer(HID_T)       :: file_id, dspace_id, dset_id
  integer(HSIZE_T), allocatable :: hdf5_dims(:)
  !
  ierr = 0
  dims = 0
  !
  if (present(CH0) .or. present(L0) .or. present(I0) .or. present(R0) .or. present(D0)) dims = 0
  if (present(CH1) .or. present(L1) .or. present(I1) .or. present(R1) .or. present(D1) .or. present(STR0)) dims = 1
  if (present(CH2) .or. present(L2) .or. present(I2) .or. present(R2) .or. present(D2) .or. present(STR1) .or. present(C1) .or. present(Z1)) dims = 2
  if (present(CH3) .or. present(L3) .or. present(I3) .or. present(R3) .or. present(D3) .or. present(C2) .or. present(Z2)) dims = 3
  if (present(CH4) .or. present(L4) .or. present(I4) .or. present(R4) .or. present(D4) .or. present(C3) .or. present(Z3)) dims = 4
  if (present(I5) .or. present(R5) .or. present(D5) .or. present(C4) .or. present(Z4)) dims = 5
  if (present(C5) .or. present(Z5)) dims = 6
  if (present(C6)) dims = 7
  !
  if (dims > 0) then
    allocate(ipos_loc(dims), isize_loc(dims))
    if (present(IPOS)) ipos_loc = IPOS
    if (.not. present(IPOS)) ipos_loc = 1
  endif
  !
  ! Strings numbers / vectors
  !
  if (present(STR0)) then
    isize_loc = (/len(STR0)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, STR0, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_CHARACTER, STR0, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(STR1)) then
    isize_loc = (/len(STR1(1)), size(STR1)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, STR1, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_CHARACTER, STR1, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  ! Character numbers / vectors / matrices
  !
  if (present(CH0)) then
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, CH0, (/1/), ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_CHARACTER, CH0, (/1/), ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(CH1)) then
    isize_loc = (/size(CH1)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, CH1, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_CHARACTER, CH1, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(CH2)) then
    isize_loc = (/size(CH2, 1), size(CH2, 2)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, CH2, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_CHARACTER, CH2, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(CH3)) then
    isize_loc = (/size(CH3, 1), size(CH3, 2), size(CH3, 3)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, CH3, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_CHARACTER, CH3, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(CH4)) then
    isize_loc = (/size(CH4, 1), size(CH4, 2), size(CH4, 3), size(CH4, 4)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_CHARACTER, CH4, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_CHARACTER, CH4, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  ! Logical numbers / vectors / matrices
  !
  if (present(L0)) then
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, L0, (/1/), ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, L0, (/1/), ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(L1)) then
    isize_loc = (/size(L1)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, L1, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, L1, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(L2)) then
    isize_loc = (/size(L2, 1), size(L2, 2)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, L2, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, L2, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(L3)) then
    isize_loc = (/size(L3, 1), size(L3, 2), size(L3, 3)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, L3, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, L3, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(L4)) then
    isize_loc = (/size(L4, 1), size(L4, 2), size(L4, 3), size(L4, 4)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, L4, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, L4, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  ! Integer numbers / vectors / matrices
  !
  if (present(I0)) then
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, I0, (/1/), ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, I0, (/1/), ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(I1)) then
    isize_loc = (/size(I1)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, I1, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, I1, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(I2)) then
    isize_loc = (/size(I2, 1), size(I2, 2)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, I2, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, I2, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(I3)) then
    isize_loc = (/size(I3, 1), size(I3, 2), size(I3, 3)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, I3, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, I3, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(I4)) then
    isize_loc = (/size(I4, 1), size(I4, 2), size(I4, 3), size(I4, 4)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, I4, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, I4, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  if (present(I5)) then
    isize_loc = (/size(I5, 1), size(I5, 2), size(I5, 3), size(I5, 4), size(I5, 5)/)
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, I5, ipos_loc, isize_loc, ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, I5, ipos_loc, isize_loc, ierr)
    endif
    call hdf5_call(ierr, ID, VAR_ID=VAR_ID)
  endif
  !
  ! Real numbers / vectors / matrices
  !
  if (present(R0)) then
    if (write_is_on(ID)) then
      call h5dwrite_f(dset_id, H5T_NATIVE_FLOAT, R0, (/1/), ierr)
    else if (read_is_on(ID)) then
      call h5dread_f(dset_id, H5T_NATIVE_FLOAT, R0, (/1/), ierr)
    endif
    call hdf5_call
