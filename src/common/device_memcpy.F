!
! Copyright (C) 2002-2018 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Utility functions to perform memcpy and memset on the device with CUDA Fortran
! cuf_memXXX contain a CUF KERNEL to perform the selected operation
! cu_memcpy contain also wrappers for cuda_memcpy (sync and async) functions
!
#include<device_macros.h>
!
!=======================================
!
subroutine sp_dev_memcpy_r1d(array_out, array_in, &
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:)
    real(real32), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    !$cuf kernel do(1)
    do i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    enddo
    !
end subroutine sp_dev_memcpy_r1d
!
subroutine sp_dev_memcpy_r2d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:)
    real(real32), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    !$cuf kernel do(2)
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    enddo
    enddo
    !
end subroutine sp_dev_memcpy_r2d
!
subroutine sp_dev_memcpy_r3d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:)
    real(real32), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    !$cuf kernel do(3)
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memcpy_r3d
!
subroutine sp_dev_memcpy_r4d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3, &
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:,:)
    real(real32), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    !$cuf kernel do(4)
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    enddo
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memcpy_r4d
!
subroutine dp_dev_memcpy_r1d(array_out, array_in, &
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:)
    real(real64), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    !$cuf kernel do(1)
    do i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    enddo
    !
end subroutine dp_dev_memcpy_r1d
!
subroutine dp_dev_memcpy_r2d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:)
    real(real64), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    !$cuf kernel do(2)
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    enddo
    enddo
    !
end subroutine dp_dev_memcpy_r2d
!
subroutine dp_dev_memcpy_r3d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:)
    real(real64), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    !$cuf kernel do(3)
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memcpy_r3d
!
subroutine dp_dev_memcpy_r4d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3, &
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:,:)
    real(real64), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    !$cuf kernel do(4)
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    enddo
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memcpy_r4d
!
subroutine sp_dev_memcpy_c1d(array_out, array_in, &
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:)
    complex(real32), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    !$cuf kernel do(1)
    do i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    enddo
    !
end subroutine sp_dev_memcpy_c1d
!
subroutine sp_dev_memcpy_c2d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:)
    complex(real32), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    !$cuf kernel do(2)
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    enddo
    enddo
    !
end subroutine sp_dev_memcpy_c2d
!
subroutine sp_dev_memcpy_c3d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:)
    complex(real32), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    !$cuf kernel do(3)
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memcpy_c3d
!
subroutine sp_dev_memcpy_c4d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3, &
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:,:)
    complex(real32), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    !$cuf kernel do(4)
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    enddo
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memcpy_c4d
!
subroutine dp_dev_memcpy_c1d(array_out, array_in, &
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:)
    complex(real64), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    !$cuf kernel do(1)
    do i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    enddo
    !
end subroutine dp_dev_memcpy_c1d
!
subroutine dp_dev_memcpy_c2d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:)
    complex(real64), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    !$cuf kernel do(2)
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    enddo
    enddo
    !
end subroutine dp_dev_memcpy_c2d
!
subroutine dp_dev_memcpy_c3d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:)
    complex(real64), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    !$cuf kernel do(3)
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memcpy_c3d
!
subroutine dp_dev_memcpy_c4d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3, &
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:,:)
    complex(real64), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    !$cuf kernel do(4)
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    enddo
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memcpy_c4d
!
subroutine i4_dev_memcpy_i1d(array_out, array_in, &
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:)
    integer(int32), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    !$cuf kernel do(1)
    do i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    enddo
    !
end subroutine i4_dev_memcpy_i1d
!
subroutine i4_dev_memcpy_i2d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:)
    integer(int32), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    !$cuf kernel do(2)
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    enddo
    enddo
    !
end subroutine i4_dev_memcpy_i2d
!
subroutine i4_dev_memcpy_i3d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:)
    integer(int32), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    !$cuf kernel do(3)
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    enddo
    enddo
    enddo
    !
end subroutine i4_dev_memcpy_i3d
!
subroutine i4_dev_memcpy_i4d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3, &
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:,:)
    integer(int32), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out, array_in
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    !$cuf kernel do(4)
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    enddo
    enddo
    enddo
    enddo
    !
end subroutine i4_dev_memcpy_i4d
!
!
!======================
!
subroutine sp_dev_memset_r1d(array_out, val, & 
                                             
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:)
    real(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    !$cuf kernel do(1)
    do i1 = d1s, d1e
        array_out(i1 ) = val
    enddo
    !
end subroutine sp_dev_memset_r1d
!
subroutine sp_dev_memset_r2d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:)
    real(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    !$cuf kernel do(2)
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = val
    enddo
    enddo
    !
end subroutine sp_dev_memset_r2d
!
subroutine sp_dev_memset_r3d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:)
    real(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    !$cuf kernel do(3)
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memset_r3d
!
subroutine sp_dev_memset_r4d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3, & 
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:,:)
    real(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    !$cuf kernel do(4)
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    enddo
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memset_r4d
!
subroutine dp_dev_memset_r1d(array_out, val, & 
                                             
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:)
    real(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    !$cuf kernel do(1)
    do i1 = d1s, d1e
        array_out(i1 ) = val
    enddo
    !
end subroutine dp_dev_memset_r1d
!
subroutine dp_dev_memset_r2d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:)
    real(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    !$cuf kernel do(2)
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = val
    enddo
    enddo
    !
end subroutine dp_dev_memset_r2d
!
subroutine dp_dev_memset_r3d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:)
    real(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    !$cuf kernel do(3)
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memset_r3d
!
subroutine dp_dev_memset_r4d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3, & 
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:,:)
    real(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    !$cuf kernel do(4)
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    enddo
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memset_r4d
!
subroutine sp_dev_memset_c1d(array_out, val, & 
                                             
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:)
    complex(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    !$cuf kernel do(1)
    do i1 = d1s, d1e
        array_out(i1 ) = val
    enddo
    !
end subroutine sp_dev_memset_c1d
!
subroutine sp_dev_memset_c2d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:)
    complex(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    !$cuf kernel do(2)
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = val
    enddo
    enddo
    !
end subroutine sp_dev_memset_c2d
!
subroutine sp_dev_memset_c3d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:)
    complex(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    !$cuf kernel do(3)
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memset_c3d
!
subroutine sp_dev_memset_c4d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3, & 
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:,:)
    complex(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    !$cuf kernel do(4)
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    enddo
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memset_c4d
!
subroutine dp_dev_memset_c1d(array_out, val, & 
                                             
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:)
    complex(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    !$cuf kernel do(1)
    do i1 = d1s, d1e
        array_out(i1 ) = val
    enddo
    !
end subroutine dp_dev_memset_c1d
!
subroutine dp_dev_memset_c2d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:)
    complex(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    !$cuf kernel do(2)
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = val
    enddo
    enddo
    !
end subroutine dp_dev_memset_c2d
!
subroutine dp_dev_memset_c3d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:)
    complex(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    !$cuf kernel do(3)
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memset_c3d
!
subroutine dp_dev_memset_c4d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3, & 
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:,:)
    complex(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    !$cuf kernel do(4)
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    enddo
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memset_c4d
!
subroutine i4_dev_memset_i1d(array_out, val, & 
                                             
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:)
    integer(int32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    !$cuf kernel do(1)
    do i1 = d1s, d1e
        array_out(i1 ) = val
    enddo
    !
end subroutine i4_dev_memset_i1d
!
subroutine i4_dev_memset_i2d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:)
    integer(int32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    !$cuf kernel do(2)
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = val
    enddo
    enddo
    !
end subroutine i4_dev_memset_i2d
!
subroutine i4_dev_memset_i3d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:)
    integer(int32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    !$cuf kernel do(3)
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    enddo
    enddo
    enddo
    !
end subroutine i4_dev_memset_i3d
!
subroutine i4_dev_memset_i4d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3, & 
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:,:)
    integer(int32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    !$cuf kernel do(4)
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    enddo
    enddo
    enddo
    enddo
    !
end subroutine i4_dev_memset_i4d
!
!
!=======================================
!
#if defined(__HAVE_DEVICE)
!
subroutine sp_dev_memcpy_h2h_r1d(array_out, array_in, &
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:)
    real(real32), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    do i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    enddo
    !
end subroutine sp_dev_memcpy_h2h_r1d
!
subroutine sp_dev_memcpy_h2h_r2d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:)
    real(real32), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    enddo
    enddo
    !
end subroutine sp_dev_memcpy_h2h_r2d
!
subroutine sp_dev_memcpy_h2h_r3d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:)
    real(real32), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memcpy_h2h_r3d
!
subroutine sp_dev_memcpy_h2h_r4d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3, &
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:,:)
    real(real32), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    enddo
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memcpy_h2h_r4d
!
subroutine dp_dev_memcpy_h2h_r1d(array_out, array_in, &
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:)
    real(real64), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    do i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    enddo
    !
end subroutine dp_dev_memcpy_h2h_r1d
!
subroutine dp_dev_memcpy_h2h_r2d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:)
    real(real64), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    enddo
    enddo
    !
end subroutine dp_dev_memcpy_h2h_r2d
!
subroutine dp_dev_memcpy_h2h_r3d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:)
    real(real64), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memcpy_h2h_r3d
!
subroutine dp_dev_memcpy_h2h_r4d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3, &
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:,:)
    real(real64), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    enddo
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memcpy_h2h_r4d
!
subroutine sp_dev_memcpy_h2h_c1d(array_out, array_in, &
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:)
    complex(real32), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    do i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    enddo
    !
end subroutine sp_dev_memcpy_h2h_c1d
!
subroutine sp_dev_memcpy_h2h_c2d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:)
    complex(real32), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    enddo
    enddo
    !
end subroutine sp_dev_memcpy_h2h_c2d
!
subroutine sp_dev_memcpy_h2h_c3d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:)
    complex(real32), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memcpy_h2h_c3d
!
subroutine sp_dev_memcpy_h2h_c4d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3, &
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:,:)
    complex(real32), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    enddo
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memcpy_h2h_c4d
!
subroutine dp_dev_memcpy_h2h_c1d(array_out, array_in, &
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:)
    complex(real64), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    do i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    enddo
    !
end subroutine dp_dev_memcpy_h2h_c1d
!
subroutine dp_dev_memcpy_h2h_c2d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:)
    complex(real64), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    enddo
    enddo
    !
end subroutine dp_dev_memcpy_h2h_c2d
!
subroutine dp_dev_memcpy_h2h_c3d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:)
    complex(real64), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memcpy_h2h_c3d
!
subroutine dp_dev_memcpy_h2h_c4d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3, &
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:,:)
    complex(real64), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    enddo
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memcpy_h2h_c4d
!
subroutine i4_dev_memcpy_h2h_i1d(array_out, array_in, &
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:)
    integer(int32), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    do i1 = d1s, d1e
        array_out(i1 ) = array_in(i1 )
    enddo
    !
end subroutine i4_dev_memcpy_h2h_i1d
!
subroutine i4_dev_memcpy_h2h_i2d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:)
    integer(int32), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = array_in(i1,i2 )
    enddo
    enddo
    !
end subroutine i4_dev_memcpy_h2h_i2d
!
subroutine i4_dev_memcpy_h2h_i3d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:)
    integer(int32), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = array_in(i1,i2,i3 )
    enddo
    enddo
    enddo
    !
end subroutine i4_dev_memcpy_h2h_i3d
!
subroutine i4_dev_memcpy_h2h_i4d(array_out, array_in, &
                                            range1, lbound1, &
                                            range2, lbound2, &
                                            range3, lbound3, &
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:,:)
    integer(int32), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    ! the lower bound of the assumed shape array passed to the subroutine is 1
    ! lbound and range instead refer to the indexing in the parent caller.
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = array_in(i1,i2,i3,i4 )
    enddo
    enddo
    enddo
    enddo
    !
end subroutine i4_dev_memcpy_h2h_i4d
!
!
!======================
!
subroutine sp_dev_memset_h_r1d(array_out, val, & 
                                             
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:)
    real(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    do i1 = d1s, d1e
        array_out(i1 ) = val
    enddo
    !
end subroutine sp_dev_memset_h_r1d
!
subroutine sp_dev_memset_h_r2d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:)
    real(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = val
    enddo
    enddo
    !
end subroutine sp_dev_memset_h_r2d
!
subroutine sp_dev_memset_h_r3d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:)
    real(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memset_h_r3d
!
subroutine sp_dev_memset_h_r4d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3, & 
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:,:)
    real(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    enddo
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memset_h_r4d
!
subroutine dp_dev_memset_h_r1d(array_out, val, & 
                                             
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:)
    real(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    do i1 = d1s, d1e
        array_out(i1 ) = val
    enddo
    !
end subroutine dp_dev_memset_h_r1d
!
subroutine dp_dev_memset_h_r2d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:)
    real(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = val
    enddo
    enddo
    !
end subroutine dp_dev_memset_h_r2d
!
subroutine dp_dev_memset_h_r3d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:)
    real(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memset_h_r3d
!
subroutine dp_dev_memset_h_r4d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3, & 
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:,:)
    real(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    enddo
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memset_h_r4d
!
subroutine sp_dev_memset_h_c1d(array_out, val, & 
                                             
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:)
    complex(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    do i1 = d1s, d1e
        array_out(i1 ) = val
    enddo
    !
end subroutine sp_dev_memset_h_c1d
!
subroutine sp_dev_memset_h_c2d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:)
    complex(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = val
    enddo
    enddo
    !
end subroutine sp_dev_memset_h_c2d
!
subroutine sp_dev_memset_h_c3d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:)
    complex(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memset_h_c3d
!
subroutine sp_dev_memset_h_c4d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3, & 
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:,:)
    complex(real32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    enddo
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_memset_h_c4d
!
subroutine dp_dev_memset_h_c1d(array_out, val, & 
                                             
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:)
    complex(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    do i1 = d1s, d1e
        array_out(i1 ) = val
    enddo
    !
end subroutine dp_dev_memset_h_c1d
!
subroutine dp_dev_memset_h_c2d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:)
    complex(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = val
    enddo
    enddo
    !
end subroutine dp_dev_memset_h_c2d
!
subroutine dp_dev_memset_h_c3d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:)
    complex(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memset_h_c3d
!
subroutine dp_dev_memset_h_c4d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3, & 
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:,:)
    complex(real64), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    enddo
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_memset_h_c4d
!
subroutine i4_dev_memset_h_i1d(array_out, val, & 
                                             
                                            range1, lbound1 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:)
    integer(int32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    do i1 = d1s, d1e
        array_out(i1 ) = val
    enddo
    !
end subroutine i4_dev_memset_h_i1d
!
subroutine i4_dev_memset_h_i2d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:)
    integer(int32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2 ) = val
    enddo
    enddo
    !
end subroutine i4_dev_memset_h_i2d
!
subroutine i4_dev_memset_h_i3d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:)
    integer(int32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    !
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3 ) = val
    enddo
    enddo
    enddo
    !
end subroutine i4_dev_memset_h_i3d
!
subroutine i4_dev_memset_h_i4d(array_out, val, & 
                                             
                                            range1, lbound1, & 
                                            range2, lbound2, & 
                                            range3, lbound3, & 
                                            range4, lbound4 )
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:,:)
    integer(int32), intent(in)    :: val
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    integer :: i3, d3s, d3e
    integer :: lbound3_, range3_(2)
    integer :: i4, d4s, d4e
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4s = range4_(1) -lbound4_ +1
    d4e = range4_(2) -lbound4_ +1
    !
    !
    do i4 = d4s, d4e
    do i3 = d3s, d3e
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_out(i1,i2,i3,i4 ) = val
    enddo
    enddo
    enddo
    enddo
    !
end subroutine i4_dev_memset_h_i4d
!
#endif
!
!======================
#if defined(__HAVE_DEVICE)
!
subroutine sp_memcpy_h2d_r1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:)
    real(real32), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyHostToDevice )
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_r1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine sp_memcpy_h2d_r1d
!
subroutine sp_memcpy_h2d_r2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:)
    real(real32), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d1_ld, d1_size, d2_size )
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_r2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine sp_memcpy_h2d_r2d
!
subroutine sp_memcpy_h2d_r3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:)
    real(real32), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    !call dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_r3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine sp_memcpy_h2d_r3d
!
subroutine sp_memcpy_h2d_r4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:,:)
    real(real32), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    !call dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_r4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine sp_memcpy_h2d_r4d
!
subroutine dp_memcpy_h2d_r1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:)
    real(real64), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyHostToDevice )
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_r1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine dp_memcpy_h2d_r1d
!
subroutine dp_memcpy_h2d_r2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:)
    real(real64), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d1_ld, d1_size, d2_size )
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_r2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine dp_memcpy_h2d_r2d
!
subroutine dp_memcpy_h2d_r3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:)
    real(real64), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    !call dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_r3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine dp_memcpy_h2d_r3d
!
subroutine dp_memcpy_h2d_r4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:,:)
    real(real64), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    !call dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_r4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine dp_memcpy_h2d_r4d
!
subroutine sp_memcpy_h2d_c1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:)
    complex(real32), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyHostToDevice )
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_c1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine sp_memcpy_h2d_c1d
!
subroutine sp_memcpy_h2d_c2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:)
    complex(real32), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d1_ld, d1_size, d2_size )
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_c2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine sp_memcpy_h2d_c2d
!
subroutine sp_memcpy_h2d_c3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:)
    complex(real32), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    !call dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_c3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine sp_memcpy_h2d_c3d
!
subroutine sp_memcpy_h2d_c4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:,:)
    complex(real32), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    !call dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_c4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine sp_memcpy_h2d_c4d
!
subroutine dp_memcpy_h2d_c1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:)
    complex(real64), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyHostToDevice )
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_c1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine dp_memcpy_h2d_c1d
!
subroutine dp_memcpy_h2d_c2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:)
    complex(real64), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d1_ld, d1_size, d2_size )
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_c2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine dp_memcpy_h2d_c2d
!
subroutine dp_memcpy_h2d_c3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:)
    complex(real64), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    !call dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_c3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine dp_memcpy_h2d_c3d
!
subroutine dp_memcpy_h2d_c4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:,:)
    complex(real64), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    !call dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_c4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine dp_memcpy_h2d_c4d
!
subroutine i4_memcpy_h2d_i1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:)
    integer(int32), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyHostToDevice )
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_h2d_i1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine i4_memcpy_h2d_i1d
!
subroutine i4_memcpy_h2d_i2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:)
    integer(int32), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d1_ld, d1_size, d2_size )
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_h2d_i2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine i4_memcpy_h2d_i2d
!
subroutine i4_memcpy_h2d_i3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:)
    integer(int32), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    !call dxlib_errore('cu_memsync_','3D arrays not implemented yet',1)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_h2d_i3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine i4_memcpy_h2d_i3d
!
subroutine i4_memcpy_h2d_i4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:,:)
    integer(int32), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    !call dxlib_errore('cu_memsync_','4D arrays not implemented yet',1)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_h2d_i4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine i4_memcpy_h2d_i4d
!
!
#endif
!
!======================
!
#if defined(__HAVE_DEVICE)
!
subroutine sp_memcpy_d2h_r1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:)
    real(real32), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyDeviceToHost )
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_r1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine sp_memcpy_d2h_r1d
!
subroutine sp_memcpy_d2h_r2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:)
    real(real32), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d1_ld, d1_size, d2_size )
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_r2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine sp_memcpy_d2h_r2d
!
subroutine sp_memcpy_d2h_r3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:)
    real(real32), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_r3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine sp_memcpy_d2h_r3d
!
subroutine sp_memcpy_d2h_r4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:,:)
    real(real32), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_r4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine sp_memcpy_d2h_r4d
!
subroutine dp_memcpy_d2h_r1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:)
    real(real64), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyDeviceToHost )
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_r1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine dp_memcpy_d2h_r1d
!
subroutine dp_memcpy_d2h_r2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:)
    real(real64), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d1_ld, d1_size, d2_size )
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_r2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine dp_memcpy_d2h_r2d
!
subroutine dp_memcpy_d2h_r3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:)
    real(real64), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_r3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine dp_memcpy_d2h_r3d
!
subroutine dp_memcpy_d2h_r4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:,:)
    real(real64), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_r4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine dp_memcpy_d2h_r4d
!
subroutine sp_memcpy_d2h_c1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:)
    complex(real32), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyDeviceToHost )
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_c1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine sp_memcpy_d2h_c1d
!
subroutine sp_memcpy_d2h_c2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:)
    complex(real32), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d1_ld, d1_size, d2_size )
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_c2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine sp_memcpy_d2h_c2d
!
subroutine sp_memcpy_d2h_c3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:)
    complex(real32), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_c3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine sp_memcpy_d2h_c3d
!
subroutine sp_memcpy_d2h_c4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:,:)
    complex(real32), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_c4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine sp_memcpy_d2h_c4d
!
subroutine dp_memcpy_d2h_c1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:)
    complex(real64), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyDeviceToHost )
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_c1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine dp_memcpy_d2h_c1d
!
subroutine dp_memcpy_d2h_c2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:)
    complex(real64), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d1_ld, d1_size, d2_size )
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_c2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine dp_memcpy_d2h_c2d
!
subroutine dp_memcpy_d2h_c3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:)
    complex(real64), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_c3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine dp_memcpy_d2h_c3d
!
subroutine dp_memcpy_d2h_c4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:,:)
    complex(real64), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_c4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine dp_memcpy_d2h_c4d
!
subroutine i4_memcpy_d2h_i1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:)
    integer(int32), intent(in)    :: array_in(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy( array_out(d1_start), array_in(d1_start), d1_size, cudaMemcpyDeviceToHost )
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_d2h_i1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine i4_memcpy_d2h_i1d
!
subroutine i4_memcpy_d2h_i2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:)
    integer(int32), intent(in)    :: array_in(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = cudaMemcpy2D( array_out(d1_start, d2_start) , d1_ld, array_in(d1_start, d2_start), d1_ld, d1_size, d2_size )
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_d2h_i2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine i4_memcpy_d2h_i2d
!
subroutine i4_memcpy_d2h_i3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:)
    integer(int32), intent(in)    :: array_in(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_d2h_i3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine i4_memcpy_d2h_i3d
!
subroutine i4_memcpy_d2h_i4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:,:)
    integer(int32), intent(in)    :: array_in(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_d2h_i4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine i4_memcpy_d2h_i4d
!
!
#endif
!======================
!
#if defined(__HAVE_DEVICE)
!
subroutine sp_memcpy_h2d_async_r1d(array_out, array_in, stream, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:)
    real(real32), intent(in)    :: array_in(:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_async_r1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine sp_memcpy_h2d_async_r1d
!
subroutine sp_memcpy_h2d_async_r2d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:)
    real(real32), intent(in)    :: array_in(:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_async_r2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine sp_memcpy_h2d_async_r2d
!
subroutine sp_memcpy_h2d_async_r3d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:)
    real(real32), intent(in)    :: array_in(:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_async_r3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine sp_memcpy_h2d_async_r3d
!
subroutine sp_memcpy_h2d_async_r4d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:,:)
    real(real32), intent(in)    :: array_in(:,:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size*d4_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_async_r4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine sp_memcpy_h2d_async_r4d
!
subroutine dp_memcpy_h2d_async_r1d(array_out, array_in, stream, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:)
    real(real64), intent(in)    :: array_in(:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_async_r1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine dp_memcpy_h2d_async_r1d
!
subroutine dp_memcpy_h2d_async_r2d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:)
    real(real64), intent(in)    :: array_in(:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_async_r2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine dp_memcpy_h2d_async_r2d
!
subroutine dp_memcpy_h2d_async_r3d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:)
    real(real64), intent(in)    :: array_in(:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_async_r3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine dp_memcpy_h2d_async_r3d
!
subroutine dp_memcpy_h2d_async_r4d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:,:)
    real(real64), intent(in)    :: array_in(:,:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size*d4_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_async_r4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine dp_memcpy_h2d_async_r4d
!
subroutine sp_memcpy_h2d_async_c1d(array_out, array_in, stream, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:)
    complex(real32), intent(in)    :: array_in(:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_async_c1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine sp_memcpy_h2d_async_c1d
!
subroutine sp_memcpy_h2d_async_c2d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:)
    complex(real32), intent(in)    :: array_in(:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_async_c2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine sp_memcpy_h2d_async_c2d
!
subroutine sp_memcpy_h2d_async_c3d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:)
    complex(real32), intent(in)    :: array_in(:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_async_c3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine sp_memcpy_h2d_async_c3d
!
subroutine sp_memcpy_h2d_async_c4d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:,:)
    complex(real32), intent(in)    :: array_in(:,:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size*d4_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_h2d_async_c4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine sp_memcpy_h2d_async_c4d
!
subroutine dp_memcpy_h2d_async_c1d(array_out, array_in, stream, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:)
    complex(real64), intent(in)    :: array_in(:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_async_c1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine dp_memcpy_h2d_async_c1d
!
subroutine dp_memcpy_h2d_async_c2d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:)
    complex(real64), intent(in)    :: array_in(:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_async_c2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine dp_memcpy_h2d_async_c2d
!
subroutine dp_memcpy_h2d_async_c3d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:)
    complex(real64), intent(in)    :: array_in(:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_async_c3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine dp_memcpy_h2d_async_c3d
!
subroutine dp_memcpy_h2d_async_c4d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:,:)
    complex(real64), intent(in)    :: array_in(:,:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size*d4_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_h2d_async_c4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine dp_memcpy_h2d_async_c4d
!
subroutine i4_memcpy_h2d_async_i1d(array_out, array_in, stream, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:)
    integer(int32), intent(in)    :: array_in(:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_h2d_async_i1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine i4_memcpy_h2d_async_i1d
!
subroutine i4_memcpy_h2d_async_i2d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:)
    integer(int32), intent(in)    :: array_in(:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_h2d_async_i2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine i4_memcpy_h2d_async_i2d
!
subroutine i4_memcpy_h2d_async_i3d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:)
    integer(int32), intent(in)    :: array_in(:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_h2d_async_i3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine i4_memcpy_h2d_async_i3d
!
subroutine i4_memcpy_h2d_async_i4d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:,:)
    integer(int32), intent(in)    :: array_in(:,:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_out
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size*d4_size,&
                           kdir=cudaMemcpyDeviceToHost,stream=stream)
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_h2d_async_i4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine i4_memcpy_h2d_async_i4d
!
!
#endif
!
!======================
!
subroutine dev_stream_sync(stream)
#if defined(__CUDA)
    use cudafor
#endif
    implicit none
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
    integer :: ierr
    ierr = CudaStreamSynchronize(stream)
#else
    integer, intent(in) :: stream
    return
#endif
end subroutine dev_stream_sync
!
!======================
!
!
subroutine sp_memcpy_d2h_async_r1d(array_out, array_in, stream, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:)
    real(real32), intent(in)    :: array_in(:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end),&
!                           array_in(d1_start:d1_end),&
!                           count=d1_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_async_r1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine sp_memcpy_d2h_async_r1d
!
subroutine sp_memcpy_d2h_async_r2d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:)
    real(real32), intent(in)    :: array_in(:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end),&
!                           count=d1_size*d2_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_async_r2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine sp_memcpy_d2h_async_r2d
!
subroutine sp_memcpy_d2h_async_r3d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:)
    real(real32), intent(in)    :: array_in(:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end),&
!                           count=d1_size*d2_size*d3_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_async_r3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine sp_memcpy_d2h_async_r3d
!
subroutine sp_memcpy_d2h_async_r4d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real32), intent(inout) :: array_out(:,:,:,:)
    real(real32), intent(in)    :: array_in(:,:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end),&
!                           count=d1_size*d2_size*d3_size*d4_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size*d4_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_async_r4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine sp_memcpy_d2h_async_r4d
!
subroutine dp_memcpy_d2h_async_r1d(array_out, array_in, stream, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:)
    real(real64), intent(in)    :: array_in(:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end),&
!                           array_in(d1_start:d1_end),&
!                           count=d1_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_async_r1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine dp_memcpy_d2h_async_r1d
!
subroutine dp_memcpy_d2h_async_r2d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:)
    real(real64), intent(in)    :: array_in(:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end),&
!                           count=d1_size*d2_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_async_r2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine dp_memcpy_d2h_async_r2d
!
subroutine dp_memcpy_d2h_async_r3d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:)
    real(real64), intent(in)    :: array_in(:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end),&
!                           count=d1_size*d2_size*d3_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_async_r3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine dp_memcpy_d2h_async_r3d
!
subroutine dp_memcpy_d2h_async_r4d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    real(real64), intent(inout) :: array_out(:,:,:,:)
    real(real64), intent(in)    :: array_in(:,:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end),&
!                           count=d1_size*d2_size*d3_size*d4_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size*d4_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_async_r4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine dp_memcpy_d2h_async_r4d
!
subroutine sp_memcpy_d2h_async_c1d(array_out, array_in, stream, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:)
    complex(real32), intent(in)    :: array_in(:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end),&
!                           array_in(d1_start:d1_end),&
!                           count=d1_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_async_c1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine sp_memcpy_d2h_async_c1d
!
subroutine sp_memcpy_d2h_async_c2d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:)
    complex(real32), intent(in)    :: array_in(:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end),&
!                           count=d1_size*d2_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_async_c2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine sp_memcpy_d2h_async_c2d
!
subroutine sp_memcpy_d2h_async_c3d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:)
    complex(real32), intent(in)    :: array_in(:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end),&
!                           count=d1_size*d2_size*d3_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_async_c3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine sp_memcpy_d2h_async_c3d
!
subroutine sp_memcpy_d2h_async_c4d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real32), intent(inout) :: array_out(:,:,:,:)
    complex(real32), intent(in)    :: array_in(:,:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end),&
!                           count=d1_size*d2_size*d3_size*d4_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size*d4_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("sp_memcpy_d2h_async_c4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine sp_memcpy_d2h_async_c4d
!
subroutine dp_memcpy_d2h_async_c1d(array_out, array_in, stream, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:)
    complex(real64), intent(in)    :: array_in(:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end),&
!                           array_in(d1_start:d1_end),&
!                           count=d1_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_async_c1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine dp_memcpy_d2h_async_c1d
!
subroutine dp_memcpy_d2h_async_c2d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:)
    complex(real64), intent(in)    :: array_in(:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end),&
!                           count=d1_size*d2_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_async_c2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine dp_memcpy_d2h_async_c2d
!
subroutine dp_memcpy_d2h_async_c3d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:)
    complex(real64), intent(in)    :: array_in(:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end),&
!                           count=d1_size*d2_size*d3_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_async_c3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine dp_memcpy_d2h_async_c3d
!
subroutine dp_memcpy_d2h_async_c4d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    complex(real64), intent(inout) :: array_out(:,:,:,:)
    complex(real64), intent(in)    :: array_in(:,:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end),&
!                           count=d1_size*d2_size*d3_size*d4_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size*d4_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("dp_memcpy_d2h_async_c4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine dp_memcpy_d2h_async_c4d
!
subroutine i4_memcpy_d2h_async_i1d(array_out, array_in, stream, &
                                             range1, lbound1  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:)
    integer(int32), intent(in)    :: array_in(:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end),&
!                           array_in(d1_start:d1_end),&
!                           count=d1_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_d2h_async_i1d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end) = &
              array_in(d1_start:d1_end)
#endif
    !
end subroutine i4_memcpy_d2h_async_i1d
!
subroutine i4_memcpy_d2h_async_i2d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:)
    integer(int32), intent(in)    :: array_in(:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end),&
!                           count=d1_size*d2_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_d2h_async_i2d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end)
#endif
    !
end subroutine i4_memcpy_d2h_async_i2d
!
subroutine i4_memcpy_d2h_async_i3d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:)
    integer(int32), intent(in)    :: array_in(:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end),&
!                           count=d1_size*d2_size*d3_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_d2h_async_i3d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end)
#endif
    !
end subroutine i4_memcpy_d2h_async_i3d
!
subroutine i4_memcpy_d2h_async_i4d(array_out, array_in, stream, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
    use cudafor
#endif
    use iso_fortran_env
    implicit none
    !
    integer(int32), intent(inout) :: array_out(:,:,:,:)
    integer(int32), intent(in)    :: array_in(:,:,:,:)
#if defined(__CUDA)
    integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
    integer, intent(in) :: stream
#endif
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_in
    integer :: ierr = 0
#endif
    !
    integer :: d1_start, d1_end, d1_size, d1_ld
    integer :: lbound1_, range1_(2)
    integer :: d2_start, d2_end, d2_size, d2_ld
    integer :: lbound2_, range2_(2)
    integer :: d3_start, d3_end, d3_size, d3_ld
    integer :: lbound3_, range3_(2)
    integer :: d4_start, d4_end, d4_size, d4_ld
    integer :: lbound4_, range4_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_out, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1_start = range1_(1) -lbound1_ +1
    d1_end   = range1_(2) -lbound1_ +1
    d1_size  = range1_(2) -range1_(1) + 1
    d1_ld    = size(array_out, 1)
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_out, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2_start = range2_(1) -lbound2_ +1
    d2_end   = range2_(2) -lbound2_ +1
    d2_size  = range2_(2) -range2_(1) + 1
    d2_ld    = size(array_out, 2)
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_out, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3_start = range3_(1) -lbound3_ +1
    d3_end   = range3_(2) -lbound3_ +1
    d3_size  = range3_(2) -range3_(1) + 1
    d3_ld    = size(array_out, 3)
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_out, 4)/)
    if (present(range4)) range4_=range4 
    !
    d4_start = range4_(1) -lbound4_ +1
    d4_end   = range4_(2) -lbound4_ +1
    d4_size  = range4_(2) -range4_(1) + 1
    d4_ld    = size(array_out, 4)
    !
#if defined(__CUDA)
!    ierr = CudaMemcpyAsync(array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end),&
!                           array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end),&
!                           count=d1_size*d2_size*d3_size*d4_size,&
!                           kdir=cudaMemcpyHostToDevice,stream=stream)
    ierr = CudaMemcpyAsync(array_out, array_in, &
                           count=d1_size*d2_size*d3_size*d4_size,&
                           kdir=cudaMemcpyHostToDevice,stream=stream)
    if ( ierr /= 0) call dxlib_errore("i4_memcpy_d2h_async_i4d",cudaGetErrorString(ierr),ierr)
#else
    array_out(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end) = &
              array_in(d1_start:d1_end,d2_start:d2_end,d3_start:d3_end,d4_start:d4_end)
#endif
    !
end subroutine i4_memcpy_d2h_async_i4d
!
