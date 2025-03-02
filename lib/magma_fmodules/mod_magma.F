!
!   -- MAGMA (version 2.7.1) --
!      Univ. of Tennessee, Knoxville
!      Univ. of California, Berkeley
!      Univ. of Colorado, Denver
!      @date February 2023
!

module magma

use iso_c_binding

use magma_common
use magma_sfortran
use magma_dfortran
use magma_cfortran
use magma_zfortran

implicit none

!! =============================================================================
!! Parameter constants from magma_types.h
integer(c_int), parameter ::   &
    MagmaFalse         = 0,    &
    MagmaTrue          = 1,    &
    
    MagmaRowMajor      = 101,  &
    MagmaColMajor      = 102,  &
    
    MagmaNoTrans       = 111,  &
    MagmaTrans         = 112,  &
    MagmaConjTrans     = 113,  &
    
    MagmaUpper         = 121,  &
    MagmaLower         = 122,  &
    MagmaGeneral       = 123,  &
    MagmaFull          = 123,  &  !! deprecated, use MagmaGeneral
    
    MagmaNonUnit       = 131,  &
    MagmaUnit          = 132,  &
    
    MagmaLeft          = 141,  &
    MagmaRight         = 142,  &
    MagmaBothSides     = 143,  &

    MagmaNoVec         = 301,  & !/* geev, syev, gesvd */
    MagmaVec           = 302,  & !/* geev, syev */
    MagmaIVec          = 303,  & !/* stedc */
    MagmaAllVec        = 304,  & !/* gesvd, trevc */
    MagmaSomeVec       = 305,  & !/* gesvd, trevc */
    MagmaOverwriteVec  = 306,  & !/* gesvd */
    MagmaBacktransVec  = 307     !/* trevc */
!! todo all the rest


!! =============================================================================
!! Fortran interfaces to C functions
interface

    !! -------------------------------------------------------------------------
    !! initialize
    subroutine magmaf_init() &
    bind(C, name="magma_init")
        use iso_c_binding
    end subroutine

    subroutine magmaf_finalize() &
    bind(C, name="magma_finalize")
        use iso_c_binding
    end subroutine

    !! -------------------------------------------------------------------------
    !! version
    subroutine magmaf_version( major, minor, micro ) &
    bind(C, name="magma_version")
        use iso_c_binding
        integer(c_int), target :: major, minor, micro
    end subroutine

    subroutine magmaf_print_environment() &
    bind(C, name="magma_print_environment")
        use iso_c_binding
    end subroutine

    !! -------------------------------------------------------------------------
    !! timing
    real(c_double) function magmaf_wtime() &
    bind(C, name="magma_wtime")
        use iso_c_binding
    end function

    real(c_double) function magmaf_sync_wtime( queue ) &
    bind(C, name="magma_wtime")
        use iso_c_binding
        type(c_ptr), value :: queue
    end function

    !! -------------------------------------------------------------------------
    !! device support
    integer(c_int) function magmaf_num_gpus() &
    bind(C, name="magma_num_gpus")
        use iso_c_binding
    end function

    integer(c_int) function magmaf_get_device_arch() &
    bind(C, name="magma_getdevice_arch")
        use iso_c_binding
    end function

    subroutine magmaf_get_device( dev ) &
    bind(C, name="magma_getdevice")
        use iso_c_binding
        integer(c_int), target :: dev
    end subroutine

    subroutine magmaf_set_device( dev ) &
    bind(C, name="magma_setdevice")
        use iso_c_binding
        integer(c_int), value :: dev
    end subroutine

    integer(c_size_t) function magmaf_mem_size( queue ) &
    bind(C, name="magma_mem_size")
        use iso_c_binding
        type(c_ptr), value :: queue
    end function

    !! -------------------------------------------------------------------------
    !! queue support
    subroutine magmaf_queue_create_internal( dev, queue_ptr, func, file, line ) &
    bind(C, name="magma_queue_create_internal")
        use iso_c_binding
        integer(c_int), value :: dev
        type(c_ptr), target :: queue_ptr  !! queue_t*
        character(c_char) :: func, file
        integer(c_int), value :: line
    end subroutine

    subroutine magmaf_queue_destroy_internal( queue, func, file, line ) &
    bind(C, name="magma_queue_destroy_internal")
        use iso_c_binding
        type(c_ptr), value :: queue  !! queue_t
        character(c_char) :: func, file
        integer(c_int), value :: line
    end subroutine

    subroutine magmaf_queue_sync_internal( queue, func, file, line ) &
    bind(C, name="magma_queue_sync_internal")
        use iso_c_binding
        type(c_ptr), value :: queue  !! queue_t
        character(c_char) :: func, file
        integer(c_int), value :: line
    end subroutine

    integer(c_int) function magmaf_queue_get_device( queue ) &
    bind(C, name="magma_queue_get_device")
        use iso_c_binding
        type(c_ptr), value :: queue  !! queue_t
    end function

    !! -------------------------------------------------------------------------
    !! offsets pointers -- 1D vectors with inc
    !! see offset.c
    type(c_ptr) function magmaf_soffset_1d( ptr, inc, i ) &
    bind(C, name="magma_soffset_1d")
        use iso_c_binding
        type(c_ptr),    value :: ptr
        integer(c_int), value :: inc, i
    end function

    type(c_ptr) function magmaf_doffset_1d( ptr, inc, i ) &
    bind(C, name="magma_doffset_1d")
        use iso_c_binding
        type(c_ptr),    value :: ptr
        integer(c_int), value :: inc, i
    end function

    type(c_ptr) function magmaf_coffset_1d( ptr, inc, i ) &
    bind(C, name="magma_coffset_1d")
        use iso_c_binding
        type(c_ptr),    value :: ptr
        integer(c_int), value :: inc, i
    end function

    type(c_ptr) function magmaf_zoffset_1d( ptr, inc, i ) &
    bind(C, name="magma_zoffset_1d")
        use iso_c_binding
        type(c_ptr),    value :: ptr
        integer(c_int), value :: inc, i
    end function

    type(c_ptr) function magmaf_ioffset_1d( ptr, inc, i ) &
    bind(C, name="magma_ioffset_1d")
        use iso_c_binding
        type(c_ptr),    value :: ptr
        integer(c_int), value :: inc, i
    end function

    !! -------------------------------------------------------------------------
    !! offsets pointers -- 2D matrices with lda
    !! see offset.c
    type(c_ptr) function magmaf_soffset_2d( ptr, lda, i, j ) &
    bind(C, name="magma_soffset_2d")
        use iso_c_binding
        type(c_ptr),    value:: ptr
        integer(c_int), value :: lda, i, j
    end function

    type(c_ptr) function magmaf_doffset_2d( ptr, lda, i, j ) &
    bind(C, name="magma_doffset_2d")
        use iso_c_binding
        type(c_ptr),    value:: ptr
        integer(c_int), value :: lda, i, j
    end function

    type(c_ptr) function magmaf_coffset_2d( ptr, lda, i, j ) &
    bind(C, name="magma_coffset_2d")
        use iso_c_binding
        type(c_ptr),    value:: ptr
        integer(c_int), value :: lda, i, j
    end function

    type(c_ptr) function magmaf_zoffset_2d( ptr, lda, i, j ) &
    bind(C, name="magma_zoffset_2d")
        use iso_c_binding
        type(c_ptr),    value:: ptr
        integer(c_int), value :: lda, i, j
    end function

    type(c_ptr) function magmaf_ioffset_2d( ptr, lda, i, j ) &
    bind(C, name="magma_ioffset_2d")
        use iso_c_binding
        type(c_ptr),    value:: ptr
        integer(c_int), value :: lda, i, j
    end function

end interface

!! =============================================================================
!! Fortran routines & functions
contains

    !! -------------------------------------------------------------------------
    !! queue support
    subroutine magmaf_queue_create( dev, queue_ptr )
        use iso_c_binding
        integer(c_int), value :: dev
        type(c_ptr), target :: queue_ptr  !! queue_t*
        
        call magmaf_queue_create_internal( &
                dev, queue_ptr, &
                "magma_queue_create" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

    subroutine magmaf_queue_destroy( queue )
        use iso_c_binding
        type(c_ptr), value :: queue  !! queue_t
        
        call magmaf_queue_destroy_internal( &
                queue, &
                "magma_queue_destroy" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

    subroutine magmaf_queue_sync( queue )
        use iso_c_binding
        type(c_ptr), value :: queue  !! queue_t
        
        call magmaf_queue_sync_internal( &
                queue, &
                "magma_queue_sync" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

end module
