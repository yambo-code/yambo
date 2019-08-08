!
#include<device_macros.h>
!
interface dev_memcpy
    !
    subroutine dp_dev_memcpy_r1d(array_out, array_in, &
                                            range1, lbound1 )
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(14,200)
      real(PRCSN), intent(inout) :: array_out(:)
      real(PRCSN), intent(in)    :: array_in(:)
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
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(14,200)
      real(PRCSN), intent(inout) :: array_out(:,:)
      real(PRCSN), intent(in)    :: array_in(:,:)
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
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(14,200)
      real(PRCSN), intent(inout) :: array_out(:,:,:)
      real(PRCSN), intent(in)    :: array_in(:,:,:)
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
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(14,200)
      real(PRCSN), intent(inout) :: array_out(:,:,:,:)
      real(PRCSN), intent(in)    :: array_in(:,:,:,:)
      integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
      integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
      attributes(device) :: array_out, array_in
#endif
       !
    end subroutine dp_dev_memcpy_r4d
    !
    subroutine dp_dev_memcpy_c1d(array_out, array_in, &
                                            range1, lbound1 )
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(14,200)
      complex(PRCSN), intent(inout) :: array_out(:)
      complex(PRCSN), intent(in)    :: array_in(:)
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
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(14,200)
      complex(PRCSN), intent(inout) :: array_out(:,:)
      complex(PRCSN), intent(in)    :: array_in(:,:)
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
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(14,200)
      complex(PRCSN), intent(inout) :: array_out(:,:,:)
      complex(PRCSN), intent(in)    :: array_in(:,:,:)
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
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(14,200)
      complex(PRCSN), intent(inout) :: array_out(:,:,:,:)
      complex(PRCSN), intent(in)    :: array_in(:,:,:,:)
      integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
      integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
      attributes(device) :: array_out, array_in
#endif
       !
    end subroutine dp_dev_memcpy_c4d
    !
    subroutine sp_dev_memcpy_r1d(array_out, array_in, &
                                            range1, lbound1 )
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(6, 37)
      real(PRCSN), intent(inout) :: array_out(:)
      real(PRCSN), intent(in)    :: array_in(:)
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
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(6, 37)
      real(PRCSN), intent(inout) :: array_out(:,:)
      real(PRCSN), intent(in)    :: array_in(:,:)
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
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(6, 37)
      real(PRCSN), intent(inout) :: array_out(:,:,:)
      real(PRCSN), intent(in)    :: array_in(:,:,:)
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
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(6, 37)
      real(PRCSN), intent(inout) :: array_out(:,:,:,:)
      real(PRCSN), intent(in)    :: array_in(:,:,:,:)
      integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
      integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
      attributes(device) :: array_out, array_in
#endif
       !
    end subroutine sp_dev_memcpy_r4d
    !
    subroutine sp_dev_memcpy_c1d(array_out, array_in, &
                                            range1, lbound1 )
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(6, 37)
      complex(PRCSN), intent(inout) :: array_out(:)
      complex(PRCSN), intent(in)    :: array_in(:)
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
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(6, 37)
      complex(PRCSN), intent(inout) :: array_out(:,:)
      complex(PRCSN), intent(in)    :: array_in(:,:)
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
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(6, 37)
      complex(PRCSN), intent(inout) :: array_out(:,:,:)
      complex(PRCSN), intent(in)    :: array_in(:,:,:)
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
      implicit none
      !   
      integer, parameter :: PRCSN = selected_real_kind(6, 37)
      complex(PRCSN), intent(inout) :: array_out(:,:,:,:)
      complex(PRCSN), intent(in)    :: array_in(:,:,:,:)
      integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
      integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
      attributes(device) :: array_out, array_in
#endif
       !
    end subroutine sp_dev_memcpy_c4d
    !
end interface dev_memcpy
!
!
interface dev_memset
    !
    subroutine dp_dev_memset_r1d(array_out, val, &
                                            
                                            range1, lbound1 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       real(PRCSN), intent(inout) :: array_out(:)
       real(PRCSN), intent(in)    :: val
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
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       real(PRCSN), intent(inout) :: array_out(:,:)
       real(PRCSN), intent(in)    :: val
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
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       real(PRCSN), intent(inout) :: array_out(:,:,:)
       real(PRCSN), intent(in)    :: val
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
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       real(PRCSN), intent(inout) :: array_out(:,:,:,:)
       real(PRCSN), intent(in)    :: val
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine dp_dev_memset_r4d
    !
    subroutine dp_dev_memset_c1d(array_out, val, &
                                            
                                            range1, lbound1 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       complex(PRCSN), intent(inout) :: array_out(:)
       complex(PRCSN), intent(in)    :: val
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
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       complex(PRCSN), intent(inout) :: array_out(:,:)
       complex(PRCSN), intent(in)    :: val
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
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       complex(PRCSN), intent(inout) :: array_out(:,:,:)
       complex(PRCSN), intent(in)    :: val
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
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       complex(PRCSN), intent(inout) :: array_out(:,:,:,:)
       complex(PRCSN), intent(in)    :: val
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine dp_dev_memset_c4d
    !
    subroutine sp_dev_memset_r1d(array_out, val, &
                                            
                                            range1, lbound1 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       real(PRCSN), intent(inout) :: array_out(:)
       real(PRCSN), intent(in)    :: val
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
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       real(PRCSN), intent(inout) :: array_out(:,:)
       real(PRCSN), intent(in)    :: val
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
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       real(PRCSN), intent(inout) :: array_out(:,:,:)
       real(PRCSN), intent(in)    :: val
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
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       real(PRCSN), intent(inout) :: array_out(:,:,:,:)
       real(PRCSN), intent(in)    :: val
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine sp_dev_memset_r4d
    !
    subroutine sp_dev_memset_c1d(array_out, val, &
                                            
                                            range1, lbound1 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       complex(PRCSN), intent(inout) :: array_out(:)
       complex(PRCSN), intent(in)    :: val
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
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       complex(PRCSN), intent(inout) :: array_out(:,:)
       complex(PRCSN), intent(in)    :: val
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
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       complex(PRCSN), intent(inout) :: array_out(:,:,:)
       complex(PRCSN), intent(in)    :: val
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
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       complex(PRCSN), intent(inout) :: array_out(:,:,:,:)
       complex(PRCSN), intent(in)    :: val
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine sp_dev_memset_c4d
    !
    !
end interface dev_memset
!
!
interface dev_memsync
    !
    subroutine dp_h2d_memsync_r1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       real(PRCSN), intent(inout) :: array_out(:) 
       real(PRCSN), intent(in)    :: array_in(:) 
       integer, optional, intent(in) ::  range1(2)
       integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine dp_h2d_memsync_r1d
    !
    subroutine dp_h2d_memsync_r2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       real(PRCSN), intent(inout) :: array_out(:,:) 
       real(PRCSN), intent(in)    :: array_in(:,:) 
       integer, optional, intent(in) ::  range1(2), range2(2)
       integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine dp_h2d_memsync_r2d
    !
    subroutine dp_h2d_memsync_r3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       real(PRCSN), intent(inout) :: array_out(:,:,:) 
       real(PRCSN), intent(in)    :: array_in(:,:,:) 
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine dp_h2d_memsync_r3d
    !
    subroutine dp_h2d_memsync_r4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       real(PRCSN), intent(inout) :: array_out(:,:,:,:) 
       real(PRCSN), intent(in)    :: array_in(:,:,:,:) 
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine dp_h2d_memsync_r4d
    !
    subroutine dp_h2d_memsync_c1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       complex(PRCSN), intent(inout) :: array_out(:) 
       complex(PRCSN), intent(in)    :: array_in(:) 
       integer, optional, intent(in) ::  range1(2)
       integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine dp_h2d_memsync_c1d
    !
    subroutine dp_h2d_memsync_c2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       complex(PRCSN), intent(inout) :: array_out(:,:) 
       complex(PRCSN), intent(in)    :: array_in(:,:) 
       integer, optional, intent(in) ::  range1(2), range2(2)
       integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine dp_h2d_memsync_c2d
    !
    subroutine dp_h2d_memsync_c3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       complex(PRCSN), intent(inout) :: array_out(:,:,:) 
       complex(PRCSN), intent(in)    :: array_in(:,:,:) 
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine dp_h2d_memsync_c3d
    !
    subroutine dp_h2d_memsync_c4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       complex(PRCSN), intent(inout) :: array_out(:,:,:,:) 
       complex(PRCSN), intent(in)    :: array_in(:,:,:,:) 
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine dp_h2d_memsync_c4d
    !
    subroutine sp_h2d_memsync_r1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       real(PRCSN), intent(inout) :: array_out(:) 
       real(PRCSN), intent(in)    :: array_in(:) 
       integer, optional, intent(in) ::  range1(2)
       integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine sp_h2d_memsync_r1d
    !
    subroutine sp_h2d_memsync_r2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       real(PRCSN), intent(inout) :: array_out(:,:) 
       real(PRCSN), intent(in)    :: array_in(:,:) 
       integer, optional, intent(in) ::  range1(2), range2(2)
       integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine sp_h2d_memsync_r2d
    !
    subroutine sp_h2d_memsync_r3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       real(PRCSN), intent(inout) :: array_out(:,:,:) 
       real(PRCSN), intent(in)    :: array_in(:,:,:) 
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine sp_h2d_memsync_r3d
    !
    subroutine sp_h2d_memsync_r4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       real(PRCSN), intent(inout) :: array_out(:,:,:,:) 
       real(PRCSN), intent(in)    :: array_in(:,:,:,:) 
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine sp_h2d_memsync_r4d
    !
    subroutine sp_h2d_memsync_c1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       complex(PRCSN), intent(inout) :: array_out(:) 
       complex(PRCSN), intent(in)    :: array_in(:) 
       integer, optional, intent(in) ::  range1(2)
       integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine sp_h2d_memsync_c1d
    !
    subroutine sp_h2d_memsync_c2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       complex(PRCSN), intent(inout) :: array_out(:,:) 
       complex(PRCSN), intent(in)    :: array_in(:,:) 
       integer, optional, intent(in) ::  range1(2), range2(2)
       integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine sp_h2d_memsync_c2d
    !
    subroutine sp_h2d_memsync_c3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       complex(PRCSN), intent(inout) :: array_out(:,:,:) 
       complex(PRCSN), intent(in)    :: array_in(:,:,:) 
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine sp_h2d_memsync_c3d
    !
    subroutine sp_h2d_memsync_c4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
       use cudafor
#endif
       implicit none
       !   
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       complex(PRCSN), intent(inout) :: array_out(:,:,:,:) 
       complex(PRCSN), intent(in)    :: array_in(:,:,:,:) 
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_out
#endif
       !
    end subroutine sp_h2d_memsync_c4d
    !
    !
#if defined(__HAVE_DEVICE)
    subroutine dp_d2h_memsync_r1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       real(PRCSN), intent(inout) :: array_out(:)
       real(PRCSN), intent(in)    :: array_in(:)
       integer, optional, intent(in) ::  range1(2)
       integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine dp_d2h_memsync_r1d
    !
    subroutine dp_d2h_memsync_r2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       real(PRCSN), intent(inout) :: array_out(:,:)
       real(PRCSN), intent(in)    :: array_in(:,:)
       integer, optional, intent(in) ::  range1(2), range2(2)
       integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine dp_d2h_memsync_r2d
    !
    subroutine dp_d2h_memsync_r3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       real(PRCSN), intent(inout) :: array_out(:,:,:)
       real(PRCSN), intent(in)    :: array_in(:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine dp_d2h_memsync_r3d
    !
    subroutine dp_d2h_memsync_r4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       real(PRCSN), intent(inout) :: array_out(:,:,:,:)
       real(PRCSN), intent(in)    :: array_in(:,:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine dp_d2h_memsync_r4d
    !
    subroutine dp_d2h_memsync_c1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       complex(PRCSN), intent(inout) :: array_out(:)
       complex(PRCSN), intent(in)    :: array_in(:)
       integer, optional, intent(in) ::  range1(2)
       integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine dp_d2h_memsync_c1d
    !
    subroutine dp_d2h_memsync_c2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       complex(PRCSN), intent(inout) :: array_out(:,:)
       complex(PRCSN), intent(in)    :: array_in(:,:)
       integer, optional, intent(in) ::  range1(2), range2(2)
       integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine dp_d2h_memsync_c2d
    !
    subroutine dp_d2h_memsync_c3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       complex(PRCSN), intent(inout) :: array_out(:,:,:)
       complex(PRCSN), intent(in)    :: array_in(:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine dp_d2h_memsync_c3d
    !
    subroutine dp_d2h_memsync_c4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       complex(PRCSN), intent(inout) :: array_out(:,:,:,:)
       complex(PRCSN), intent(in)    :: array_in(:,:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine dp_d2h_memsync_c4d
    !
    subroutine sp_d2h_memsync_r1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       real(PRCSN), intent(inout) :: array_out(:)
       real(PRCSN), intent(in)    :: array_in(:)
       integer, optional, intent(in) ::  range1(2)
       integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine sp_d2h_memsync_r1d
    !
    subroutine sp_d2h_memsync_r2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       real(PRCSN), intent(inout) :: array_out(:,:)
       real(PRCSN), intent(in)    :: array_in(:,:)
       integer, optional, intent(in) ::  range1(2), range2(2)
       integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine sp_d2h_memsync_r2d
    !
    subroutine sp_d2h_memsync_r3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       real(PRCSN), intent(inout) :: array_out(:,:,:)
       real(PRCSN), intent(in)    :: array_in(:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine sp_d2h_memsync_r3d
    !
    subroutine sp_d2h_memsync_r4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       real(PRCSN), intent(inout) :: array_out(:,:,:,:)
       real(PRCSN), intent(in)    :: array_in(:,:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine sp_d2h_memsync_r4d
    !
    subroutine sp_d2h_memsync_c1d(array_out, array_in, &
                                             range1, lbound1  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       complex(PRCSN), intent(inout) :: array_out(:)
       complex(PRCSN), intent(in)    :: array_in(:)
       integer, optional, intent(in) ::  range1(2)
       integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine sp_d2h_memsync_c1d
    !
    subroutine sp_d2h_memsync_c2d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       complex(PRCSN), intent(inout) :: array_out(:,:)
       complex(PRCSN), intent(in)    :: array_in(:,:)
       integer, optional, intent(in) ::  range1(2), range2(2)
       integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine sp_d2h_memsync_c2d
    !
    subroutine sp_d2h_memsync_c3d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       complex(PRCSN), intent(inout) :: array_out(:,:,:)
       complex(PRCSN), intent(in)    :: array_in(:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine sp_d2h_memsync_c3d
    !
    subroutine sp_d2h_memsync_c4d(array_out, array_in, &
                                             range1, lbound1 , &
                                             range2, lbound2 , &
                                             range3, lbound3 , &
                                             range4, lbound4  )
#if defined(__CUDA)
       use cudafor
#endif 
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       complex(PRCSN), intent(inout) :: array_out(:,:,:,:)
       complex(PRCSN), intent(in)    :: array_in(:,:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_in
#endif
       !
    end subroutine sp_d2h_memsync_c4d
    !
#endif
    !
end interface dev_memsync
!
!
interface dev_conjg
    !

    subroutine dp_dev_conjg_c1d(array_inout, &
                                   
                                   range1, lbound1 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       COMPLEX(PRCSN), intent(inout) :: array_inout(:)
       integer, optional, intent(in) ::  range1(2)
       integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
       attributes(device) :: array_inout
#endif
       !
    end subroutine dp_dev_conjg_c1d
    !
    subroutine dp_dev_conjg_c2d(array_inout, &
                                   
                                   range1, lbound1, &
                                   range2, lbound2 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       COMPLEX(PRCSN), intent(inout) :: array_inout(:,:)
       integer, optional, intent(in) ::  range1(2), range2(2)
       integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
       attributes(device) :: array_inout
#endif
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
       COMPLEX(PRCSN), intent(inout) :: array_inout(:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
       attributes(device) :: array_inout
#endif
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
       COMPLEX(PRCSN), intent(inout) :: array_inout(:,:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_inout
#endif
       !
    end subroutine dp_dev_conjg_c4d
    !
    subroutine sp_dev_conjg_c1d(array_inout, &
                                   
                                   range1, lbound1 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       COMPLEX(PRCSN), intent(inout) :: array_inout(:)
       integer, optional, intent(in) ::  range1(2)
       integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
       attributes(device) :: array_inout
#endif
       !
    end subroutine sp_dev_conjg_c1d
    !
    subroutine sp_dev_conjg_c2d(array_inout, &
                                   
                                   range1, lbound1, &
                                   range2, lbound2 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       COMPLEX(PRCSN), intent(inout) :: array_inout(:,:)
       integer, optional, intent(in) ::  range1(2), range2(2)
       integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
       attributes(device) :: array_inout
#endif
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
       COMPLEX(PRCSN), intent(inout) :: array_inout(:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
       attributes(device) :: array_inout
#endif
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
       COMPLEX(PRCSN), intent(inout) :: array_inout(:,:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
       attributes(device) :: array_inout
#endif
       !
    end subroutine sp_dev_conjg_c4d
    !
    !
end interface dev_conjg
