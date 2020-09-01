!
#include<device_macros.h>
!
interface dev_memcpy
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
    end subroutine i4_dev_memcpy_i4d
    !
    !
#if defined(__HAVE_DEVICE)
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
    end subroutine i4_dev_memcpy_h2h_i4d
    !
#endif
    !
#if defined(__HAVE_DEVICE)
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
#endif
       !
    end subroutine i4_memcpy_h2d_i4d
    !
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
#endif
       !
    end subroutine i4_memcpy_d2h_i4d
    !
#endif
    !
end interface dev_memcpy
!
interface dev_memcpy_async
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
       integer, optional, intent(in) :: range1(2)
       integer, optional, intent(in) :: lbound1
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2)
       integer, optional, intent(in) :: lbound1,lbound2
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2),range4(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3,lbound4
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2)
       integer, optional, intent(in) :: lbound1
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2)
       integer, optional, intent(in) :: lbound1,lbound2
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2),range4(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3,lbound4
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2)
       integer, optional, intent(in) :: lbound1
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2)
       integer, optional, intent(in) :: lbound1,lbound2
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2),range4(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3,lbound4
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2)
       integer, optional, intent(in) :: lbound1
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2)
       integer, optional, intent(in) :: lbound1,lbound2
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2),range4(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3,lbound4
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2)
       integer, optional, intent(in) :: lbound1
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2)
       integer, optional, intent(in) :: lbound1,lbound2
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3
#if defined(__CUDA)
       attributes(device) :: array_in
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2),range4(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3,lbound4
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine i4_memcpy_d2h_async_i4d
    !
    !
#if defined(__HAVE_DEVICE)
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
       integer, optional, intent(in) :: range1(2)
       integer, optional, intent(in) :: lbound1
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2)
       integer, optional, intent(in) :: lbound1,lbound2
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2),range4(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3,lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2)
       integer, optional, intent(in) :: lbound1
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2)
       integer, optional, intent(in) :: lbound1,lbound2
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2),range4(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3,lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2)
       integer, optional, intent(in) :: lbound1
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2)
       integer, optional, intent(in) :: lbound1,lbound2
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2),range4(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3,lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2)
       integer, optional, intent(in) :: lbound1
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2)
       integer, optional, intent(in) :: lbound1,lbound2
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2),range4(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3,lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2)
       integer, optional, intent(in) :: lbound1
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2)
       integer, optional, intent(in) :: lbound1,lbound2
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3
#if defined(__CUDA)
       attributes(device) :: array_out
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
       integer, optional, intent(in) :: range1(2),range2(2),range3(2),range4(2)
       integer, optional, intent(in) :: lbound1,lbound2,lbound3,lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine i4_memcpy_h2d_async_i4d
    !
    !
#endif
    !
end interface dev_memcpy_async
!
interface 
    !
    subroutine dev_stream_sync(stream)
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
#if defined(__CUDA)
       integer(kind=cuda_Stream_Kind), intent(in) :: stream
#else
       integer, intent(in) :: stream
#endif
    !
    end subroutine dev_stream_sync
    !
end interface 
!
interface dev_memset
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
    end subroutine i4_dev_memset_i4d
    !
    !
#if defined(__HAVE_DEVICE)
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
    end subroutine i4_dev_memset_h_i4d
    !
    !
#endif
    !
end interface dev_memset
!