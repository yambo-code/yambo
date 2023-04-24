!! @precisions fortran z -> s d c

module magma2_zfortran

use magma2_common
implicit none

!! =============================================================================
!! Fortran interfaces to C functions
interface

    !! -------------------------------------------------------------------------
    !! CPU interfaces (matrix in CPU memory)
    subroutine magma_zgetrf( m, n, A, lda, ipiv, info ) &
    bind(C, name="magma_zgetrf")
        use iso_c_binding
        integer(c_int),            value  :: m, n, lda
        complex(c_double_complex), target :: A(lda,*)
        integer(c_int),            target :: ipiv(*)
        integer(c_int),            target :: info  !! int*
    end subroutine

    subroutine magma_zpotrf( uplo, n, A, lda, info ) &
    bind(C, name="magma_zpotrf")
        use iso_c_binding
        integer(c_int),            value  :: uplo
        integer(c_int),            value  :: n, lda
        complex(c_double_complex), target :: A(lda,*)
        integer(c_int),            target :: info  !! int*
    end subroutine

    subroutine magma_zgeev( jobvl, jobvr, n, A, lda, w, VL, ldvl, VR, ldvr, &
                            work,  lwork, rwork, info) &
    bind(C, name="magma_zgeev")
        use iso_c_binding
        integer(c_int),            value  :: jobvl, jobvr
        integer(c_int),            value  :: n, lda, ldvl, ldvr, lwork
        complex(c_double_complex), target :: A(lda,*)
        complex(c_double_complex), target :: w(*)
        complex(c_double_complex), target :: VR(ldvr,*),  VL(ldvl,*)
        complex(c_double_complex), target :: work(*)
        real(c_double),            target :: rwork(*)
        integer(c_int),            target :: info  !! int*
    end subroutine

    subroutine magma_zgeev_m( jobvl, jobvr, n, A, lda, w, VL, ldvl, VR, ldvr, &
                            work,  lwork, rwork, info) &
    bind(C, name="magma_zgeev_m")
        use iso_c_binding
        integer(c_int),            value  :: jobvl, jobvr
        integer(c_int),            value  :: n, lda, ldvl, ldvr, lwork
        complex(c_double_complex), target :: A(lda,*)
        complex(c_double_complex), target :: w(*)
        complex(c_double_complex), target :: VR(ldvr,*),  VL(ldvl,*)
        complex(c_double_complex), target :: work(*)
        real(c_double),            target :: rwork(*)
        integer(c_int),            target :: info  !! int*
    end subroutine

    subroutine magma_zheevd_m( ngpu, jobz, uplo, n, A, lda, w, work,  lwork, &
                               rwork, lrwork, iwork, liwork, info) &
    bind(C, name="magma_zheevd_m")
        use iso_c_binding
        integer(c_int),            value  :: ngpu
        integer(c_int),            value  :: jobz, uplo
        integer(c_int),            value  :: n, lda, lwork, lrwork, liwork
        complex(c_double_complex), target :: A(lda,*)
        real(c_double),            target :: w(*)
        complex(c_double_complex), target :: work(*)
        real(c_double),            target :: rwork(*)
        integer(c_int),            target :: iwork(*)
        integer(c_int),            target :: info  !! int*
    end subroutine

    !! -------------------------------------------------------------------------
    !! -------------------------------------------------------------------------
    !! GPU interfaces (matrix in GPU memory)
    subroutine magma_zgetrf_gpu( m, n, dA, lda, ipiv, info ) &
    bind(C, name="magma_zgetrf_gpu")
        use iso_c_binding
        integer(c_int), value  :: m, n, lda
        type(c_ptr),    value  :: dA
        integer(c_int), target :: ipiv(*)
        integer(c_int), target :: info  !! int*
    end subroutine

    subroutine magma_zpotrf_gpu( uplo, n, dA, lda, info ) &
    bind(C, name="magma_zpotrf_gpu")
        use iso_c_binding
        integer(c_int), value  :: uplo, n, lda
        type(c_ptr),    value  :: dA
        integer(c_int), target :: info  !! int*
    end subroutine

    subroutine magma_zheevd_gpu( jobz, uplo, n, dA, ldda, w, wA, ldwa, work, lwork, &
                                 rwork, lrwork, iwork, liwork, info) &
    bind(C, name="magma_zheevd_gpu")
        use iso_c_binding
        integer(c_int),            value  :: jobz, uplo
        integer(c_int),            value  :: n, ldda, ldwa, lwork, lrwork, liwork
        type(c_ptr),               value  :: dA  !! double complex**
        real(c_double),            target :: w(*)
        complex(c_double_complex), target :: wA(*)
        complex(c_double_complex), target :: work(*)
        real(c_double),            target :: rwork(*)
        integer(c_int),            target :: iwork(*)
        integer(c_int),            target :: info  !! int*
    end subroutine

    !! -------------------------------------------------------------------------
    !! batched GPU interfaces (all arrays in GPU memory)
    subroutine magma_zgetrf_batched( &
        m, n, dA_array, lda, ipiv_array, info_array, batchcount, queue ) &
    bind(C, name="magma_zgetrf_batched")
        use iso_c_binding
        integer(c_int), value  :: m, n, lda, batchcount
        type(c_ptr),    value  :: dA_array    !! double_complex**
        type(c_ptr),    value  :: ipiv_array  !! int**
        type(c_ptr),    value  :: info_array  !! int*
        type(c_ptr),    value  :: queue
    end subroutine

    !! -------------------------------------------------------------------------
    !! BLAS (matrices in GPU memory)
    subroutine magma_zaxpy( &
        n, &
        alpha, dx, incx, &
               dy, incy, &
        queue ) &
    bind(C, name="magma_zaxpy")
        use iso_c_binding
        integer(c_int),             value :: n, incx, incy
        complex(c_double_complex),  value :: alpha
        type(c_ptr),                value :: dx, dy
        type(c_ptr),                value :: queue  !! queue_t
    end subroutine

    subroutine magma_zgemv( &
        transA, m, n, &
        alpha, dA, lda, &
               dx, incx, &
        beta,  dy, incy, &
        queue ) &
    bind(C, name="magma_zgemv")
        use iso_c_binding
        integer(c_int),             value :: transA, m, n, lda, incx, incy
        complex(c_double_complex),  value :: alpha, beta
        type(c_ptr),                value :: dA, dx, dy
        type(c_ptr),                value :: queue  !! queue_t
    end subroutine

    subroutine magma_zgemm( &
        transA, transB, m, n, k, &
        alpha, dA, lda, &
               dB, ldb, &
        beta,  dC, ldc, &
        queue ) &
    bind(C, name="magma_zgemm")
        use iso_c_binding
        integer(c_int),             value :: transA, transB, m, n, k, lda, ldb, ldc
        complex(c_double_complex),  value :: alpha, beta
        type(c_ptr),                value :: dA, dB, dC
        type(c_ptr),                value :: queue  !! queue_t
    end subroutine

end interface

!! =============================================================================
!! Fortran routines & functions
contains

    !! -------------------------------------------------------------------------
    !! malloc wrappers
    integer(c_int) function magma_zmalloc( ptr, n )
        use iso_c_binding
        type(c_ptr),       target :: ptr  !! void**
        integer(c_size_t), value  :: n
        
        magma_zmalloc = magma_malloc( ptr, n*sizeof_complex16 )
    end function

    integer(c_int) function magma_zmalloc_cpu( ptr, n )
        use iso_c_binding
        type(c_ptr),       target :: ptr  !! void**
        integer(c_size_t), value  :: n
        
        magma_zmalloc_cpu = magma_malloc_cpu( ptr, n*sizeof_complex16 )
    end function

    integer(c_int) function magma_zmalloc_pinned( ptr, n )
        use iso_c_binding
        type(c_ptr),       target :: ptr  !! void**
        integer(c_size_t), value  :: n
        
        magma_zmalloc_pinned = magma_malloc_pinned( ptr, n*sizeof_complex16 )
    end function

    !! -------------------------------------------------------------------------
    !! set/get wrappers
    subroutine magma_zsetmatrix( &
        m, n, hA_src, lda, dB_dst, ldb, queue )
        use iso_c_binding
        integer(c_int),            value  :: m, n, lda, ldb
        complex(c_double_complex), target :: hA_src(lda,*)
        type(c_ptr),               value  :: dB_dst
        type(c_ptr),               value  :: queue
        
        call magma_setmatrix_internal( &
                m, n, int(sizeof_complex16), c_loc(hA_src), lda, dB_dst, ldb, queue, &
                "magma_zsetmatrix" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

    subroutine magma_zgetmatrix( &
        m, n, dA_src, lda, hB_dst, ldb, queue )
        use iso_c_binding
        integer(c_int),            value  :: m, n, lda, ldb
        type(c_ptr),               value  :: dA_src
        complex(c_double_complex), target :: hB_dst(ldb,*)
        type(c_ptr),               value  :: queue
        
        call magma_getmatrix_internal( &
                m, n, int(sizeof_complex16), dA_src, lda, c_loc(hB_dst), ldb, queue, &
                "magma_zgetmatrix" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

end module
