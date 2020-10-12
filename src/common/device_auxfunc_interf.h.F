
#include<device_macros.h>
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
#if defined(__HAVE_DEVICE)
    !
    subroutine dp_dev_conjg_h_c1d(array_inout, &
                                   
                                   range1, lbound1 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       COMPLEX(PRCSN), intent(inout) :: array_inout(:)
       integer, optional, intent(in) ::  range1(2)
       integer, optional, intent(in) ::  lbound1
       !
    end subroutine dp_dev_conjg_h_c1d
    !
    subroutine dp_dev_conjg_h_c2d(array_inout, &
                                   
                                   range1, lbound1, &
                                   range2, lbound2 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       COMPLEX(PRCSN), intent(inout) :: array_inout(:,:)
       integer, optional, intent(in) ::  range1(2), range2(2)
       integer, optional, intent(in) ::  lbound1, lbound2
       !
    end subroutine dp_dev_conjg_h_c2d
    !
    subroutine dp_dev_conjg_h_c3d(array_inout, &
                                   
                                   range1, lbound1, &
                                   range2, lbound2, &
                                   range3, lbound3 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(14,200)
       COMPLEX(PRCSN), intent(inout) :: array_inout(:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3
       !
    end subroutine dp_dev_conjg_h_c3d
    !
    subroutine dp_dev_conjg_h_c4d(array_inout, &
                                   
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
       !
    end subroutine dp_dev_conjg_h_c4d
    !
    subroutine sp_dev_conjg_h_c1d(array_inout, &
                                   
                                   range1, lbound1 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       COMPLEX(PRCSN), intent(inout) :: array_inout(:)
       integer, optional, intent(in) ::  range1(2)
       integer, optional, intent(in) ::  lbound1
       !
    end subroutine sp_dev_conjg_h_c1d
    !
    subroutine sp_dev_conjg_h_c2d(array_inout, &
                                   
                                   range1, lbound1, &
                                   range2, lbound2 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       COMPLEX(PRCSN), intent(inout) :: array_inout(:,:)
       integer, optional, intent(in) ::  range1(2), range2(2)
       integer, optional, intent(in) ::  lbound1, lbound2
       !
    end subroutine sp_dev_conjg_h_c2d
    !
    subroutine sp_dev_conjg_h_c3d(array_inout, &
                                   
                                   range1, lbound1, &
                                   range2, lbound2, &
                                   range3, lbound3 )
       implicit none
       !
       integer, parameter :: PRCSN = selected_real_kind(6, 37)
       COMPLEX(PRCSN), intent(inout) :: array_inout(:,:,:)
       integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
       integer, optional, intent(in) ::  lbound1, lbound2, lbound3
       !
    end subroutine sp_dev_conjg_h_c3d
    !
    subroutine sp_dev_conjg_h_c4d(array_inout, &
                                   
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
       !
    end subroutine sp_dev_conjg_h_c4d
    !
    !
#endif
    !
end interface dev_conjg

interface dev_vec_upd_remap
    !

    subroutine dp_dev_vec_upd_remap_v_r1d(ndim, vout, v1, map1, v2, scal)
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
       !
    end subroutine dp_dev_vec_upd_remap_v_r1d
    !
    subroutine dp_dev_vec_upd_remap_v_c1d(ndim, vout, v1, map1, v2, scal)
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
       !
    end subroutine dp_dev_vec_upd_remap_v_c1d
    !
    subroutine sp_dev_vec_upd_remap_v_r1d(ndim, vout, v1, map1, v2, scal)
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
       !
    end subroutine sp_dev_vec_upd_remap_v_r1d
    !
    subroutine sp_dev_vec_upd_remap_v_c1d(ndim, vout, v1, map1, v2, scal)
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
       !
    end subroutine sp_dev_vec_upd_remap_v_c1d
    !
    !
end interface dev_vec_upd_remap

interface dev_vec_upd_v_remap_v
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
    !
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
    !
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
    !
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
    !
    end subroutine sp_dev_vec_upd_v_remap_v_c1d
    !
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
       !
    end subroutine sp_dev_vec_upd_v_remap_v_x_c1d
    !
    !
end interface dev_vec_upd_v_remap_v

interface dev_mat_upd_dMd
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
       !
    end subroutine sp_dev_mat_upd_dMd_c2d
    !
    !
end interface dev_mat_upd_dMd

interface dev_mem_addscal
    !
    subroutine dp_dev_mem_addscal_r1d(array_out, array_in, scal, &
                                                range1, lbound1 )
        use iso_fortran_env
        implicit none
        !
        real(selected_real_kind(14,200)), intent(inout) :: array_out(:)
        real(selected_real_kind(14,200)), intent(in)    :: array_in(:)
        real(selected_real_kind(14,200)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2)
        integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine dp_dev_mem_addscal_r1d
    !
    subroutine dp_dev_mem_addscal_r2d(array_out, array_in, scal, &
                                                range1, lbound1, &
                                                range2, lbound2 )
        use iso_fortran_env
        implicit none
        !
        real(selected_real_kind(14,200)), intent(inout) :: array_out(:,:)
        real(selected_real_kind(14,200)), intent(in)    :: array_in(:,:)
        real(selected_real_kind(14,200)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2), range2(2)
        integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine dp_dev_mem_addscal_r2d
    !
    subroutine dp_dev_mem_addscal_r3d(array_out, array_in, scal, &
                                                range1, lbound1, &
                                                range2, lbound2, &
                                                range3, lbound3 )
        use iso_fortran_env
        implicit none
        !
        real(selected_real_kind(14,200)), intent(inout) :: array_out(:,:,:)
        real(selected_real_kind(14,200)), intent(in)    :: array_in(:,:,:)
        real(selected_real_kind(14,200)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
        integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine dp_dev_mem_addscal_r3d
    !
    subroutine dp_dev_mem_addscal_r4d(array_out, array_in, scal, &
                                                range1, lbound1, &
                                                range2, lbound2, &
                                                range3, lbound3, &
                                                range4, lbound4 )
        use iso_fortran_env
        implicit none
        !
        real(selected_real_kind(14,200)), intent(inout) :: array_out(:,:,:,:)
        real(selected_real_kind(14,200)), intent(in)    :: array_in(:,:,:,:)
        real(selected_real_kind(14,200)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
        integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine dp_dev_mem_addscal_r4d
    !
    subroutine sp_dev_mem_addscal_r1d(array_out, array_in, scal, &
                                                range1, lbound1 )
        use iso_fortran_env
        implicit none
        !
        real(selected_real_kind(6, 37)), intent(inout) :: array_out(:)
        real(selected_real_kind(6, 37)), intent(in)    :: array_in(:)
        real(selected_real_kind(6, 37)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2)
        integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine sp_dev_mem_addscal_r1d
    !
    subroutine sp_dev_mem_addscal_r2d(array_out, array_in, scal, &
                                                range1, lbound1, &
                                                range2, lbound2 )
        use iso_fortran_env
        implicit none
        !
        real(selected_real_kind(6, 37)), intent(inout) :: array_out(:,:)
        real(selected_real_kind(6, 37)), intent(in)    :: array_in(:,:)
        real(selected_real_kind(6, 37)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2), range2(2)
        integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine sp_dev_mem_addscal_r2d
    !
    subroutine sp_dev_mem_addscal_r3d(array_out, array_in, scal, &
                                                range1, lbound1, &
                                                range2, lbound2, &
                                                range3, lbound3 )
        use iso_fortran_env
        implicit none
        !
        real(selected_real_kind(6, 37)), intent(inout) :: array_out(:,:,:)
        real(selected_real_kind(6, 37)), intent(in)    :: array_in(:,:,:)
        real(selected_real_kind(6, 37)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
        integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine sp_dev_mem_addscal_r3d
    !
    subroutine sp_dev_mem_addscal_r4d(array_out, array_in, scal, &
                                                range1, lbound1, &
                                                range2, lbound2, &
                                                range3, lbound3, &
                                                range4, lbound4 )
        use iso_fortran_env
        implicit none
        !
        real(selected_real_kind(6, 37)), intent(inout) :: array_out(:,:,:,:)
        real(selected_real_kind(6, 37)), intent(in)    :: array_in(:,:,:,:)
        real(selected_real_kind(6, 37)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
        integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine sp_dev_mem_addscal_r4d
    !
    subroutine dp_dev_mem_addscal_c1d(array_out, array_in, scal, &
                                                range1, lbound1 )
        use iso_fortran_env
        implicit none
        !
        complex(selected_real_kind(14,200)), intent(inout) :: array_out(:)
        complex(selected_real_kind(14,200)), intent(in)    :: array_in(:)
        complex(selected_real_kind(14,200)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2)
        integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine dp_dev_mem_addscal_c1d
    !
    subroutine dp_dev_mem_addscal_c2d(array_out, array_in, scal, &
                                                range1, lbound1, &
                                                range2, lbound2 )
        use iso_fortran_env
        implicit none
        !
        complex(selected_real_kind(14,200)), intent(inout) :: array_out(:,:)
        complex(selected_real_kind(14,200)), intent(in)    :: array_in(:,:)
        complex(selected_real_kind(14,200)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2), range2(2)
        integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine dp_dev_mem_addscal_c2d
    !
    subroutine dp_dev_mem_addscal_c3d(array_out, array_in, scal, &
                                                range1, lbound1, &
                                                range2, lbound2, &
                                                range3, lbound3 )
        use iso_fortran_env
        implicit none
        !
        complex(selected_real_kind(14,200)), intent(inout) :: array_out(:,:,:)
        complex(selected_real_kind(14,200)), intent(in)    :: array_in(:,:,:)
        complex(selected_real_kind(14,200)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
        integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine dp_dev_mem_addscal_c3d
    !
    subroutine dp_dev_mem_addscal_c4d(array_out, array_in, scal, &
                                                range1, lbound1, &
                                                range2, lbound2, &
                                                range3, lbound3, &
                                                range4, lbound4 )
        use iso_fortran_env
        implicit none
        !
        complex(selected_real_kind(14,200)), intent(inout) :: array_out(:,:,:,:)
        complex(selected_real_kind(14,200)), intent(in)    :: array_in(:,:,:,:)
        complex(selected_real_kind(14,200)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
        integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine dp_dev_mem_addscal_c4d
    !
    subroutine sp_dev_mem_addscal_c1d(array_out, array_in, scal, &
                                                range1, lbound1 )
        use iso_fortran_env
        implicit none
        !
        complex(selected_real_kind(6, 37)), intent(inout) :: array_out(:)
        complex(selected_real_kind(6, 37)), intent(in)    :: array_in(:)
        complex(selected_real_kind(6, 37)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2)
        integer, optional, intent(in) ::  lbound1
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine sp_dev_mem_addscal_c1d
    !
    subroutine sp_dev_mem_addscal_c2d(array_out, array_in, scal, &
                                                range1, lbound1, &
                                                range2, lbound2 )
        use iso_fortran_env
        implicit none
        !
        complex(selected_real_kind(6, 37)), intent(inout) :: array_out(:,:)
        complex(selected_real_kind(6, 37)), intent(in)    :: array_in(:,:)
        complex(selected_real_kind(6, 37)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2), range2(2)
        integer, optional, intent(in) ::  lbound1, lbound2
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine sp_dev_mem_addscal_c2d
    !
    subroutine sp_dev_mem_addscal_c3d(array_out, array_in, scal, &
                                                range1, lbound1, &
                                                range2, lbound2, &
                                                range3, lbound3 )
        use iso_fortran_env
        implicit none
        !
        complex(selected_real_kind(6, 37)), intent(inout) :: array_out(:,:,:)
        complex(selected_real_kind(6, 37)), intent(in)    :: array_in(:,:,:)
        complex(selected_real_kind(6, 37)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2), range2(2), range3(2)
        integer, optional, intent(in) ::  lbound1, lbound2, lbound3
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine sp_dev_mem_addscal_c3d
    !
    subroutine sp_dev_mem_addscal_c4d(array_out, array_in, scal, &
                                                range1, lbound1, &
                                                range2, lbound2, &
                                                range3, lbound3, &
                                                range4, lbound4 )
        use iso_fortran_env
        implicit none
        !
        complex(selected_real_kind(6, 37)), intent(inout) :: array_out(:,:,:,:)
        complex(selected_real_kind(6, 37)), intent(in)    :: array_in(:,:,:,:)
        complex(selected_real_kind(6, 37)), optional, intent(in) :: scal
        integer, optional, intent(in) ::  range1(2), range2(2), range3(2), range4(2)
        integer, optional, intent(in) ::  lbound1, lbound2, lbound3, lbound4
#if defined(__CUDA)
        attributes(device) :: array_out, array_in
#endif
    end subroutine sp_dev_mem_addscal_c4d
    !
    !
end interface dev_mem_addscal
