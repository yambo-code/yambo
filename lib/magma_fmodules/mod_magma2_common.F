module magma2_common

use iso_c_binding
implicit none

!! =====================================================================
!! Parameter constants
real(c_float),             parameter :: sdummy = 0
real(c_double),            parameter :: ddummy = 0
complex(c_float_complex),  parameter :: cdummy = 0
complex(c_double_complex), parameter :: zdummy = 0
integer(c_int),            parameter :: idummy = 0
type(c_ptr),               parameter :: ptr_dummy = c_null_ptr

!! Intel ifort chokes on c_sizeof here, so use extension sizeof
!! see https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/495001
integer(c_size_t), parameter :: &
    sizeof_real      = sizeof(sdummy), &
    sizeof_double    = sizeof(ddummy), &
    sizeof_complex   = sizeof(cdummy), &
    sizeof_complex16 = sizeof(zdummy), &
    sizeof_int       = sizeof(idummy), &
    sizeof_ptr       = sizeof(ptr_dummy)


!! =============================================================================
!! Fortran interfaces to C functions
interface

    !! -------------------------------------------------------------------------
    !! magma_malloc (GPU memory)
    integer(c_int) function magma_malloc( ptr, bytes ) &
    bind(C, name="magma_malloc")
        use iso_c_binding
        type(c_ptr), target :: ptr  !! void**
        integer(c_size_t), value :: bytes
    end function

    !! todo imalloc

    integer(c_int) function magma_free_internal( ptr, func, file, line ) &
    bind(C, name="magma_free_internal")
        use iso_c_binding
        type(c_ptr), value :: ptr  !! void*
        character(c_char) :: func, file
        integer(c_int), value :: line
    end function

    !! -------------------------------------------------------------------------
    !! magma_malloc_cpu (CPU main memory)
    !! these are aligned to 32-byte boundary
    integer(c_int) function magma_malloc_cpu( ptr, bytes ) &
    bind(C, name="magma_malloc_cpu")
        use iso_c_binding
        type(c_ptr), target :: ptr  !! void**
        integer(c_size_t), value :: bytes
    end function

    !! todo imalloc_cpu

    integer(c_int) function magma_free_cpu( ptr ) &
    bind(C, name="magma_free_cpu")
        use iso_c_binding
        type(c_ptr), value :: ptr  !! void*
    end function

    !! -------------------------------------------------------------------------
    !! magma_malloc_pinned (pinned CPU main memory)
    integer(c_int) function magma_malloc_pinned( ptr, bytes ) &
    bind(C, name="magma_malloc_pinned")
        use iso_c_binding
        type(c_ptr), target :: ptr  !! void**
        integer(c_size_t), value :: bytes
    end function

    !! todo imalloc_pinned

    integer(c_int) function magma_free_pinned_internal( ptr, func, file, line ) &
    bind(C, name="magma_free_pinned_internal")
        use iso_c_binding
        type(c_ptr), value :: ptr  !! void*
        character(c_char), value :: func, file
        integer(c_int), value :: line
    end function

    !! -------------------------------------------------------------------------
    !! set/get
    subroutine magma_setmatrix_internal( &
        m, n, elemsize, hA_src, lda, dB_dst, ldb, queue, func, file, line ) &
    bind(C, name="magma_setmatrix_internal")
        use iso_c_binding
        integer(c_int),    value  :: m, n, elemsize, lda, ldb
        type(c_ptr),       value  :: hA_src
        type(c_ptr),       value  :: dB_dst
        type(c_ptr),       value  :: queue
        character(c_char), value  :: func, file
        integer(c_int),    value  :: line
    end subroutine

    subroutine magma_getmatrix_internal( &
        m, n, elemsize, dA_src, lda, hB_dst, ldb, queue, func, file, line ) &
    bind(C, name="magma_getmatrix_internal")
        use iso_c_binding
        integer(c_int),    value  :: m, n, elemsize, lda, ldb
        type(c_ptr),       value  :: dA_src
        type(c_ptr),       value  :: hB_dst
        type(c_ptr),       value  :: queue
        character(c_char), value  :: func, file
        integer(c_int),    value  :: line
    end subroutine
    
    subroutine magma_setvector_internal( &
        n, elemsize, hx_src, incx, dy_dst, incy, queue, func, file, line ) &
    bind(C, name="magma_setvector_internal")
        use iso_c_binding
        integer(c_int),    value  :: n, elemsize, incx, incy
        type(c_ptr),       value  :: hx_src
        type(c_ptr),       value  :: dy_dst
        type(c_ptr),       value  :: queue
        character(c_char), value  :: func, file
        integer(c_int),    value  :: line
    end subroutine

    subroutine magma_getvector_internal( &
        n, elemsize, dx_src, incx, hy_dst, incy, queue, func, file, line ) &
    bind(C, name="magma_getvector_internal")
        use iso_c_binding
        integer(c_int),    value  :: n, elemsize, incx, incy
        type(c_ptr),       value  :: dx_src
        type(c_ptr),       value  :: hy_dst
        type(c_ptr),       value  :: queue
        character(c_char), value  :: func, file
        integer(c_int),    value  :: line
    end subroutine

end interface

!! =============================================================================
!! Fortran routines & functions
contains

    !! -------------------------------------------------------------------------
    !! malloc wrappers
    integer(c_int) function magma_imalloc( ptr, n )
        use iso_c_binding
        type(c_ptr),       target :: ptr  !! void**
        integer(c_size_t), value  :: n
        
        magma_imalloc = magma_malloc( ptr, n*sizeof_int )
    end function

    integer(c_int) function magma_imalloc_cpu( ptr, n )
        use iso_c_binding
        type(c_ptr),       target :: ptr  !! void**
        integer(c_size_t), value  :: n
        
        magma_imalloc_cpu = magma_malloc_cpu( ptr, n*sizeof_int )
    end function

    integer(c_int) function magma_imalloc_pinned( ptr, n )
        use iso_c_binding
        type(c_ptr),       target :: ptr  !! void**
        integer(c_size_t), value  :: n
        
        magma_imalloc_pinned = magma_malloc_pinned( ptr, n*sizeof_int )
    end function

    !! -------------------------------------------------------------------------
    !! magma_free wrappers
    integer(c_int) function magma_free( ptr )
        type(c_ptr) :: ptr
        
        magma_free = magma_free_internal( &
                ptr, &
                "magma_free" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end function

    integer(c_int) function magma_free_pinned( ptr )
        type(c_ptr) :: ptr
        
        magma_free_pinned = magma_free_internal( &
                ptr, &
                "magma_free_pinned" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end function

    !! -------------------------------------------------------------------------
    !! set/get wrappers
    subroutine magma_setmatrix( &
        m, n, elemsize, hA_src, lda, dB_dst, ldb, queue )
        use iso_c_binding
        integer(c_int),    value  :: m, n, elemsize, lda, ldb
        type(c_ptr),       value  :: hA_src
        type(c_ptr),       value  :: dB_dst
        type(c_ptr),       value  :: queue
        
        call magma_setmatrix_internal( &
                m, n, elemsize, hA_src, lda, dB_dst, ldb, queue, &
                "magma_setmatrix" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

    subroutine magma_getmatrix( &
        m, n, elemsize, dA_src, lda, hB_dst, ldb, queue )
        use iso_c_binding
        integer(c_int),    value  :: m, n, elemsize, lda, ldb
        type(c_ptr),       value  :: dA_src
        type(c_ptr),       value  :: hB_dst
        type(c_ptr),       value  :: queue
        
        call magma_getmatrix_internal( &
                m, n, elemsize, dA_src, lda, hB_dst, ldb, queue, &
                "magma_getmatrix" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine
    
    subroutine magma_setvector( &
        n, elemsize, hx_src, incx, dy_dst, incy, queue )
        use iso_c_binding
        integer(c_int),    value  :: n, elemsize, incx, incy
        type(c_ptr),       value  :: hx_src
        type(c_ptr),       value  :: dy_dst
        type(c_ptr),       value  :: queue
        
        call magma_setvector_internal( &
                n, elemsize, hx_src, incx, dy_dst, incy, queue, &
                "magma_setvector" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

    subroutine magma_getvector( &
        n, elemsize, dx_src, incx, hy_dst, incy, queue )
        use iso_c_binding
        integer(c_int),    value  :: n, elemsize, incx, incy
        type(c_ptr),       value  :: dx_src
        type(c_ptr),       value  :: hy_dst
        type(c_ptr),       value  :: queue
        
        call magma_getvector_internal( &
                n, elemsize, dx_src, incx, hy_dst, incy, queue, &
                "magma_getvector" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

    !! -------------------------------------------------------------------------
    !! set/get wrappers
    !! matrices & vectors of integers
    subroutine magma_isetmatrix( &
        m, n, hA_src, lda, dB_dst, ldb, queue )
        use iso_c_binding
        integer(c_int), value  :: m, n, lda, ldb
        integer(c_int), target :: hA_src(lda,*)
        type(c_ptr),    value  :: dB_dst
        type(c_ptr),    value  :: queue
        
        call magma_setmatrix_internal( &
                m, n, int(sizeof_int), c_loc(hA_src), lda, dB_dst, ldb, queue, &
                "magma_isetmatrix" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

    subroutine magma_igetmatrix( &
        m, n, dA_src, lda, hB_dst, ldb, queue )
        use iso_c_binding
        integer(c_int), value  :: m, n, lda, ldb
        type(c_ptr),    value  :: dA_src
        integer(c_int), target :: hB_dst(ldb,*)
        type(c_ptr),    value  :: queue
        
        call magma_getmatrix_internal( &
                m, n, int(sizeof_int), dA_src, lda, c_loc(hB_dst), ldb, queue, &
                "magma_igetmatrix" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine
    
    subroutine magma_isetvector( &
        n, hx_src, incx, dy_dst, incy, queue )
        use iso_c_binding
        integer(c_int), value  :: n, incx, incy
        integer(c_int), target :: hx_src(*)
        type(c_ptr),    value  :: dy_dst
        type(c_ptr),    value  :: queue
        
        call magma_setvector_internal( &
                n, int(sizeof_int), c_loc(hx_src), incx, dy_dst, incy, queue, &
                "magma_isetvector" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

    subroutine magma_igetvector( &
        n, dx_src, incx, hy_dst, incy, queue )
        use iso_c_binding
        integer(c_int), value  :: n, incx, incy
        type(c_ptr),    value  :: dx_src
        integer(c_int), target :: hy_dst(*)
        type(c_ptr),    value  :: queue
        
        call magma_getvector_internal( &
                n, int(sizeof_int), dx_src, incx, c_loc(hy_dst), incy, queue, &
                "magma_igetvector" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

    !! -------------------------------------------------------------------------
    !! set/get wrappers
    !! matrices & vectors of c_ptr pointers
    subroutine magma_psetmatrix( &
        m, n, hA_src, lda, dB_dst, ldb, queue )
        use iso_c_binding
        integer(c_int), value  :: m, n, lda, ldb
        type(c_ptr),    target :: hA_src(lda,*)
        type(c_ptr),    value  :: dB_dst
        type(c_ptr),    value  :: queue
        
        call magma_setmatrix_internal( &
                m, n, int(sizeof_ptr), c_loc(hA_src), lda, dB_dst, ldb, queue, &
                "magma_psetmatrix" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

    subroutine magma_pgetmatrix( &
        m, n, dA_src, lda, hB_dst, ldb, queue )
        use iso_c_binding
        integer(c_int), value  :: m, n, lda, ldb
        type(c_ptr),    value  :: dA_src
        type(c_ptr),    target :: hB_dst(ldb,*)
        type(c_ptr),    value  :: queue
        
        call magma_getmatrix_internal( &
                m, n, int(sizeof_ptr), dA_src, lda, c_loc(hB_dst), ldb, queue, &
                "magma_pgetmatrix" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine
    
    subroutine magma_psetvector( &
        n, hx_src, incx, dy_dst, incy, queue )
        use iso_c_binding
        integer(c_int), value  :: n, incx, incy
        type(c_ptr),    target :: hx_src(*)
        type(c_ptr),    value  :: dy_dst
        type(c_ptr),    value  :: queue
        
        call magma_setvector_internal( &
                n, int(sizeof_ptr), c_loc(hx_src), incx, dy_dst, incy, queue, &
                "magma_psetvector" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

    subroutine magma_pgetvector( &
        n, dx_src, incx, hy_dst, incy, queue )
        use iso_c_binding
        integer(c_int), value  :: n, incx, incy
        type(c_ptr),    value  :: dx_src
        type(c_ptr),    target :: hy_dst(*)
        type(c_ptr),    value  :: queue
        
        call magma_getvector_internal( &
                n, int(sizeof_ptr), dx_src, incx, c_loc(hy_dst), incy, queue, &
                "magma_pgetvector" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

end module
