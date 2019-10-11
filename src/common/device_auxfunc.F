!
! Copyright (C) 2002-2018 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! Auxiliary functions
!
#include<device_macros.h>
!
!=======================================
!

subroutine dp_dev_conjg_c1d(array_inout, &
                                    
                                   range1, lbound1 )
    implicit none
    !
    integer, parameter :: PRCSN = selected_real_kind(14,200)
    complex(PRCSN), intent(inout) :: array_inout(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_inout
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_inout, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    !$cuf kernel do(1)
    do i1 = d1s, d1e
        array_inout(i1 ) = &
            conjg (  array_inout (i1 ) )
    enddo
    !
end subroutine dp_dev_conjg_c1d
!
subroutine dp_dev_conjg_c2d(array_inout, &
                                    
                                   range1, lbound1, & 
                                   range2, lbound2 )
    implicit none
    !
    integer, parameter :: PRCSN = selected_real_kind(14,200)
    complex(PRCSN), intent(inout) :: array_inout(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_inout
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_inout, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_inout, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    !$cuf kernel do(2)
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_inout(i1,i2 ) = &
            conjg (  array_inout (i1,i2 ) )
    enddo
    enddo
    !
end subroutine dp_dev_conjg_c2d
!
subroutine dp_dev_conjg_c3d(array_inout, &
                                    
                                   range1, lbound1, & 
                                   range2, lbound2, & 
                                   range3, lbound3 )
    implicit none
    !
    integer, parameter :: PRCSN = selected_real_kind(14,200)
    complex(PRCSN), intent(inout) :: array_inout(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_inout
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
    range1_=(/1,size(array_inout, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_inout, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_inout, 3)/)
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
        array_inout(i1,i2,i3 ) = &
            conjg (  array_inout (i1,i2,i3 ) )
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_conjg_c3d
!
subroutine dp_dev_conjg_c4d(array_inout, &
                                    
                                   range1, lbound1, & 
                                   range2, lbound2, & 
                                   range3, lbound3, & 
                                   range4, lbound4 )
    implicit none
    !
    integer, parameter :: PRCSN = selected_real_kind(14,200)
    complex(PRCSN), intent(inout) :: array_inout(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_inout
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
    range1_=(/1,size(array_inout, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_inout, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_inout, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_inout, 4)/)
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
        array_inout(i1,i2,i3,i4 ) = &
            conjg (  array_inout (i1,i2,i3,i4 ) )
    enddo
    enddo
    enddo
    enddo
    !
end subroutine dp_dev_conjg_c4d
!
subroutine sp_dev_conjg_c1d(array_inout, &
                                    
                                   range1, lbound1 )
    implicit none
    !
    integer, parameter :: PRCSN = selected_real_kind(6, 37)
    complex(PRCSN), intent(inout) :: array_inout(:)
    integer, optional, intent(in) ::  range1(2)
    integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
    attributes(device) :: array_inout
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_inout, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    !
    !$cuf kernel do(1)
    do i1 = d1s, d1e
        array_inout(i1 ) = &
            conjg (  array_inout (i1 ) )
    enddo
    !
end subroutine sp_dev_conjg_c1d
!
subroutine sp_dev_conjg_c2d(array_inout, &
                                    
                                   range1, lbound1, & 
                                   range2, lbound2 )
    implicit none
    !
    integer, parameter :: PRCSN = selected_real_kind(6, 37)
    complex(PRCSN), intent(inout) :: array_inout(:,:)
    integer, optional, intent(in) ::  range1(2), range2(2)
    integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
    attributes(device) :: array_inout
#endif
    !
    integer :: i1, d1s, d1e
    integer :: lbound1_, range1_(2)
    integer :: i2, d2s, d2e
    integer :: lbound2_, range2_(2)
    !
    lbound1_=1
    if (present(lbound1)) lbound1_=lbound1 
    range1_=(/1,size(array_inout, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_inout, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    !
    !$cuf kernel do(2)
    do i2 = d2s, d2e
    do i1 = d1s, d1e
        array_inout(i1,i2 ) = &
            conjg (  array_inout (i1,i2 ) )
    enddo
    enddo
    !
end subroutine sp_dev_conjg_c2d
!
subroutine sp_dev_conjg_c3d(array_inout, &
                                    
                                   range1, lbound1, & 
                                   range2, lbound2, & 
                                   range3, lbound3 )
    implicit none
    !
    integer, parameter :: PRCSN = selected_real_kind(6, 37)
    complex(PRCSN), intent(inout) :: array_inout(:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
    attributes(device) :: array_inout
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
    range1_=(/1,size(array_inout, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_inout, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_inout, 3)/)
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
        array_inout(i1,i2,i3 ) = &
            conjg (  array_inout (i1,i2,i3 ) )
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_conjg_c3d
!
subroutine sp_dev_conjg_c4d(array_inout, &
                                    
                                   range1, lbound1, & 
                                   range2, lbound2, & 
                                   range3, lbound3, & 
                                   range4, lbound4 )
    implicit none
    !
    integer, parameter :: PRCSN = selected_real_kind(6, 37)
    complex(PRCSN), intent(inout) :: array_inout(:,:,:,:)
    integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
    integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
    attributes(device) :: array_inout
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
    range1_=(/1,size(array_inout, 1)/)
    if (present(range1)) range1_=range1 
    !
    d1s = range1_(1) -lbound1_ +1
    d1e = range1_(2) -lbound1_ +1
    !
    lbound2_=1
    if (present(lbound2)) lbound2_=lbound2 
    range2_=(/1,size(array_inout, 2)/)
    if (present(range2)) range2_=range2 
    !
    d2s = range2_(1) -lbound2_ +1
    d2e = range2_(2) -lbound2_ +1
    !
    lbound3_=1
    if (present(lbound3)) lbound3_=lbound3 
    range3_=(/1,size(array_inout, 3)/)
    if (present(range3)) range3_=range3 
    !
    d3s = range3_(1) -lbound3_ +1
    d3e = range3_(2) -lbound3_ +1
    !
    lbound4_=1
    if (present(lbound4)) lbound4_=lbound4 
    range4_=(/1,size(array_inout, 4)/)
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
        array_inout(i1,i2,i3,i4 ) = &
            conjg (  array_inout (i1,i2,i3,i4 ) )
    enddo
    enddo
    enddo
    enddo
    !
end subroutine sp_dev_conjg_c4d
!


subroutine dxlib_errore( calling_routine, message, ierr )
    implicit none
    character(len=*), intent(in) :: calling_routine, message
    ! the name of the calling calling_routine
    ! the output message
    integer,          intent(in) :: ierr
    !
    print *, trim(calling_routine), trim(message), ierr
    !
end subroutine dxlib_errore
!
!======================
!

subroutine dp_dev_vec_upd_remap_r1d(ndim, vout, v1, map1, scal)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(14,200)
    integer,      intent(in)    :: ndim
    real(PRCSN), intent(inout) :: vout(:) 
    real(PRCSN), intent(in)    :: v1(:) 
    integer,      intent(in)    :: map1(:) 
    real(PRCSN), optional, intent(in)    :: scal
#if defined(__CUDA)
    attributes(device) :: vout, v1, map1
#endif
    integer :: i
    !   
    if (present(scal)) then
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))*scal
      enddo
    else
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))
      enddo
    endif
end subroutine dp_dev_vec_upd_remap_r1d
!
subroutine dp_dev_vec_upd_remap_c1d(ndim, vout, v1, map1, scal)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(14,200)
    integer,      intent(in)    :: ndim
    complex(PRCSN), intent(inout) :: vout(:) 
    complex(PRCSN), intent(in)    :: v1(:) 
    integer,      intent(in)    :: map1(:) 
    complex(PRCSN), optional, intent(in)    :: scal
#if defined(__CUDA)
    attributes(device) :: vout, v1, map1
#endif
    integer :: i
    !   
    if (present(scal)) then
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))*scal
      enddo
    else
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))
      enddo
    endif
end subroutine dp_dev_vec_upd_remap_c1d
!
subroutine sp_dev_vec_upd_remap_r1d(ndim, vout, v1, map1, scal)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(6, 37)
    integer,      intent(in)    :: ndim
    real(PRCSN), intent(inout) :: vout(:) 
    real(PRCSN), intent(in)    :: v1(:) 
    integer,      intent(in)    :: map1(:) 
    real(PRCSN), optional, intent(in)    :: scal
#if defined(__CUDA)
    attributes(device) :: vout, v1, map1
#endif
    integer :: i
    !   
    if (present(scal)) then
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))*scal
      enddo
    else
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))
      enddo
    endif
end subroutine sp_dev_vec_upd_remap_r1d
!
subroutine sp_dev_vec_upd_remap_c1d(ndim, vout, v1, map1, scal)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(6, 37)
    integer,      intent(in)    :: ndim
    complex(PRCSN), intent(inout) :: vout(:) 
    complex(PRCSN), intent(in)    :: v1(:) 
    integer,      intent(in)    :: map1(:) 
    complex(PRCSN), optional, intent(in)    :: scal
#if defined(__CUDA)
    attributes(device) :: vout, v1, map1
#endif
    integer :: i
    !   
    if (present(scal)) then
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))*scal
      enddo
    else
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))
      enddo
    endif
end subroutine sp_dev_vec_upd_remap_c1d
!
!
!======================
!

subroutine dp_dev_vec_upd_v_remap_v_r1d(ndim, vout, v1, map1, v2, scal)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(14,200)
    integer,      intent(in)    :: ndim
    real(PRCSN), intent(inout) :: vout(:) 
    real(PRCSN), intent(in)    :: v1(:) 
    integer,      intent(in)    :: map1(:) 
    real(PRCSN), intent(in)    :: v2(:) 
    real(PRCSN), optional, intent(in)    :: scal
#if defined(__CUDA)
    attributes(device) :: vout, v1, v2, map1
#endif
    integer :: i
    !   
    if (present(scal)) then
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))*v2(i)*scal
      enddo
    else
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))*v2(i)
      enddo
    endif
end subroutine dp_dev_vec_upd_v_remap_v_r1d
!
subroutine dp_dev_vec_upd_v_remap_v_c1d(ndim, vout, v1, map1, v2, scal)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(14,200)
    integer,      intent(in)    :: ndim
    complex(PRCSN), intent(inout) :: vout(:) 
    complex(PRCSN), intent(in)    :: v1(:) 
    integer,      intent(in)    :: map1(:) 
    complex(PRCSN), intent(in)    :: v2(:) 
    complex(PRCSN), optional, intent(in)    :: scal
#if defined(__CUDA)
    attributes(device) :: vout, v1, v2, map1
#endif
    integer :: i
    !   
    if (present(scal)) then
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))*v2(i)*scal
      enddo
    else
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))*v2(i)
      enddo
    endif
end subroutine dp_dev_vec_upd_v_remap_v_c1d
!
subroutine sp_dev_vec_upd_v_remap_v_r1d(ndim, vout, v1, map1, v2, scal)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(6, 37)
    integer,      intent(in)    :: ndim
    real(PRCSN), intent(inout) :: vout(:) 
    real(PRCSN), intent(in)    :: v1(:) 
    integer,      intent(in)    :: map1(:) 
    real(PRCSN), intent(in)    :: v2(:) 
    real(PRCSN), optional, intent(in)    :: scal
#if defined(__CUDA)
    attributes(device) :: vout, v1, v2, map1
#endif
    integer :: i
    !   
    if (present(scal)) then
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))*v2(i)*scal
      enddo
    else
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))*v2(i)
      enddo
    endif
end subroutine sp_dev_vec_upd_v_remap_v_r1d
!
subroutine sp_dev_vec_upd_v_remap_v_c1d(ndim, vout, v1, map1, v2, scal)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(6, 37)
    integer,      intent(in)    :: ndim
    complex(PRCSN), intent(inout) :: vout(:) 
    complex(PRCSN), intent(in)    :: v1(:) 
    integer,      intent(in)    :: map1(:) 
    complex(PRCSN), intent(in)    :: v2(:) 
    complex(PRCSN), optional, intent(in)    :: scal
#if defined(__CUDA)
    attributes(device) :: vout, v1, v2, map1
#endif
    integer :: i
    !   
    if (present(scal)) then
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))*v2(i)*scal
      enddo
    else
      !$cuf kernel do(1)
      do i = 1, ndim
          vout(i) = v1(map1(i))*v2(i)
      enddo
    endif
end subroutine sp_dev_vec_upd_v_remap_v_c1d
!
!
!======================
!


subroutine dp_dev_vec_upd_v_remap_v_x_c1d(ndim, vout, v1,op1, map1, v2,op2, scal)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(14,200)
    integer,      intent(in)    :: ndim
    complex(PRCSN), intent(inout) :: vout(:) 
    complex(PRCSN), intent(in)    :: v1(:) 
    integer,      intent(in)    :: map1(:) 
    complex(PRCSN), intent(in)    :: v2(:) 
    character(1), intent(in)    :: op1, op2
    complex(PRCSN), optional, intent(in)    :: scal
#if defined(__CUDA)
    attributes(device) :: vout, v1, v2, map1
#endif
    integer :: i
    !   
    if (op1=="N".and.op2=="N") then
       if (present(scal)) then
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = v1(map1(i))*v2(i)*scal
         enddo
       else
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = v1(map1(i))*v2(i)
         enddo
       endif
    elseif (op1=="C".and.op2=="N") then
       if (present(scal)) then
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = conjg(v1(map1(i)))*v2(i)*scal
         enddo
       else
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = conjg(v1(map1(i)))*v2(i)
         enddo
       endif
    elseif (op1=="N".and.op2=="C") then
       if (present(scal)) then
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = v1(map1(i))*conjg(v2(i))*scal
         enddo
       else
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = v1(map1(i))*conjg(v2(i))
         enddo
       endif
    elseif (op1=="N".and.op2=="C") then
       if (present(scal)) then
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = conjg(v1(map1(i)))*conjg(v2(i))*scal
         enddo
       else
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = conjg(v1(map1(i)))*conjg(v2(i))
         enddo
       endif
    else
      call dxlib_errore("dp_dev_vec_upd_v_remap_v_x_c1d","invalid op1/op2",10)
    endif
    !
end subroutine dp_dev_vec_upd_v_remap_v_x_c1d
!
subroutine sp_dev_vec_upd_v_remap_v_x_c1d(ndim, vout, v1,op1, map1, v2,op2, scal)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(6, 37)
    integer,      intent(in)    :: ndim
    complex(PRCSN), intent(inout) :: vout(:) 
    complex(PRCSN), intent(in)    :: v1(:) 
    integer,      intent(in)    :: map1(:) 
    complex(PRCSN), intent(in)    :: v2(:) 
    character(1), intent(in)    :: op1, op2
    complex(PRCSN), optional, intent(in)    :: scal
#if defined(__CUDA)
    attributes(device) :: vout, v1, v2, map1
#endif
    integer :: i
    !   
    if (op1=="N".and.op2=="N") then
       if (present(scal)) then
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = v1(map1(i))*v2(i)*scal
         enddo
       else
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = v1(map1(i))*v2(i)
         enddo
       endif
    elseif (op1=="C".and.op2=="N") then
       if (present(scal)) then
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = conjg(v1(map1(i)))*v2(i)*scal
         enddo
       else
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = conjg(v1(map1(i)))*v2(i)
         enddo
       endif
    elseif (op1=="N".and.op2=="C") then
       if (present(scal)) then
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = v1(map1(i))*conjg(v2(i))*scal
         enddo
       else
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = v1(map1(i))*conjg(v2(i))
         enddo
       endif
    elseif (op1=="N".and.op2=="C") then
       if (present(scal)) then
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = conjg(v1(map1(i)))*conjg(v2(i))*scal
         enddo
       else
         !$cuf kernel do(1)
         do i = 1, ndim
             vout(i) = conjg(v1(map1(i)))*conjg(v2(i))
         enddo
       endif
    else
      call dxlib_errore("sp_dev_vec_upd_v_remap_v_x_c1d","invalid op1/op2",10)
    endif
    !
end subroutine sp_dev_vec_upd_v_remap_v_x_c1d
!
!
!======================
!
subroutine dp_dev_mat_upd_dMd_r2d(ndim1, ndim2, mat, v1,op1, v2,op2, scal)
    !
    ! performs: mat(i,j) = scal * op1(v1(i)) * mat(i,j) * op2(v2(j))
    ! op = 'N', 'R', 'C',       'RC'
    !       x   1/x  conjg(x)   conjg(1/x)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(14,200)
    integer,      intent(in)    :: ndim1,ndim2
    real(PRCSN), intent(inout) :: mat(:,:)
    real(PRCSN), intent(in)    :: v1(:)
    real(PRCSN), intent(in)    :: v2(:)
    character(1), intent(in)    :: op1, op2
    real(PRCSN), optional, intent(in)  :: scal
#if defined(__CUDA)
    attributes(device) :: mat, v1, v2
#endif
    integer :: i,j
    !   
    if (op1=="N".and.op2=="N") then
       if (present(scal)) then
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = scal * v1(i) * mat(i,j) * v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       else
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = v1(i) * mat(i,j) * v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       endif
    elseif (op1=="R".and.op2=="R") then
       if (present(scal)) then
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = scal / v1(i) * mat(i,j) / v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       else
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = mat(i,j) /v1(i) / v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       endif
    else
      call dxlib_errore("dp_mat_upd_dMd_r2d","invalid op1/op2",10)
    endif
    !
end subroutine dp_dev_mat_upd_dMd_r2d
!
subroutine dp_dev_mat_upd_dMd_c2d(ndim1, ndim2, mat, v1,op1, v2,op2, scal)
    !
    ! performs: mat(i,j) = scal * op1(v1(i)) * mat(i,j) * op2(v2(j))
    ! op = 'N', 'R', 'C',       'RC'
    !       x   1/x  conjg(x)   conjg(1/x)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(14,200)
    integer,      intent(in)    :: ndim1,ndim2
    complex(PRCSN), intent(inout) :: mat(:,:)
    complex(PRCSN), intent(in)    :: v1(:)
    complex(PRCSN), intent(in)    :: v2(:)
    character(1), intent(in)    :: op1, op2
    complex(PRCSN), optional, intent(in)  :: scal
#if defined(__CUDA)
    attributes(device) :: mat, v1, v2
#endif
    integer :: i,j
    !   
    if (op1=="N".and.op2=="N") then
       if (present(scal)) then
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = scal * v1(i) * mat(i,j) * v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       else
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = v1(i) * mat(i,j) * v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       endif
    elseif (op1=="R".and.op2=="R") then
       if (present(scal)) then
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = scal / v1(i) * mat(i,j) / v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       else
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = mat(i,j) /v1(i) / v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       endif
    elseif (op1=="C".and.op2=="C") then
       if (present(scal)) then
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = scal * conjg(v1(i)) * mat(i,j) * conjg(v2(j))
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       else
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = conjg(v1(i)) * mat(i,j) * conjg(v2(j))
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       endif
    else
      call dxlib_errore("dp_mat_upd_dMd_c2d","invalid op1/op2",10)
    endif
    !
end subroutine dp_dev_mat_upd_dMd_c2d
!
subroutine sp_dev_mat_upd_dMd_r2d(ndim1, ndim2, mat, v1,op1, v2,op2, scal)
    !
    ! performs: mat(i,j) = scal * op1(v1(i)) * mat(i,j) * op2(v2(j))
    ! op = 'N', 'R', 'C',       'RC'
    !       x   1/x  conjg(x)   conjg(1/x)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(6, 37)
    integer,      intent(in)    :: ndim1,ndim2
    real(PRCSN), intent(inout) :: mat(:,:)
    real(PRCSN), intent(in)    :: v1(:)
    real(PRCSN), intent(in)    :: v2(:)
    character(1), intent(in)    :: op1, op2
    real(PRCSN), optional, intent(in)  :: scal
#if defined(__CUDA)
    attributes(device) :: mat, v1, v2
#endif
    integer :: i,j
    !   
    if (op1=="N".and.op2=="N") then
       if (present(scal)) then
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = scal * v1(i) * mat(i,j) * v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       else
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = v1(i) * mat(i,j) * v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       endif
    elseif (op1=="R".and.op2=="R") then
       if (present(scal)) then
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = scal / v1(i) * mat(i,j) / v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       else
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = mat(i,j) /v1(i) / v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       endif
    else
      call dxlib_errore("sp_mat_upd_dMd_r2d","invalid op1/op2",10)
    endif
    !
end subroutine sp_dev_mat_upd_dMd_r2d
!
subroutine sp_dev_mat_upd_dMd_c2d(ndim1, ndim2, mat, v1,op1, v2,op2, scal)
    !
    ! performs: mat(i,j) = scal * op1(v1(i)) * mat(i,j) * op2(v2(j))
    ! op = 'N', 'R', 'C',       'RC'
    !       x   1/x  conjg(x)   conjg(1/x)
    implicit none
    !   
    integer, parameter :: PRCSN = selected_real_kind(6, 37)
    integer,      intent(in)    :: ndim1,ndim2
    complex(PRCSN), intent(inout) :: mat(:,:)
    complex(PRCSN), intent(in)    :: v1(:)
    complex(PRCSN), intent(in)    :: v2(:)
    character(1), intent(in)    :: op1, op2
    complex(PRCSN), optional, intent(in)  :: scal
#if defined(__CUDA)
    attributes(device) :: mat, v1, v2
#endif
    integer :: i,j
    !   
    if (op1=="N".and.op2=="N") then
       if (present(scal)) then
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = scal * v1(i) * mat(i,j) * v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       else
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = v1(i) * mat(i,j) * v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       endif
    elseif (op1=="R".and.op2=="R") then
       if (present(scal)) then
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = scal / v1(i) * mat(i,j) / v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       else
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = mat(i,j) /v1(i) / v2(j)
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       endif
    elseif (op1=="C".and.op2=="C") then
       if (present(scal)) then
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = scal * conjg(v1(i)) * mat(i,j) * conjg(v2(j))
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       else
#ifndef _CUDA
         !$omp parallel do(2)
#else
         !$cuf kernel do(2)
#endif
         do j = 1, ndim2
         do i = 1, ndim1
             mat(i,j) = conjg(v1(i)) * mat(i,j) * conjg(v2(j))
         enddo
         enddo
#ifndef _CUDA
         !$omp end parallel do
#endif
       endif
    else
      call dxlib_errore("sp_mat_upd_dMd_c2d","invalid op1/op2",10)
    endif
    !
end subroutine sp_dev_mat_upd_dMd_c2d
!
!
!======================
!