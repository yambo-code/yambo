!! @generated from magma_zfortran.F90, fortran z -> s, Sat Apr 22 18:54:26 2023

module magma_sfortran

use magma_common
implicit none

!! =============================================================================
!! Fortran interfaces to C functions
interface

    !! -------------------------------------------------------------------------
    !! CPU interfaces (matrix in CPU memory)
    subroutine magmaf_sgetrf( m, n, A, lda, ipiv, info ) &
    bind(C, name="magma_sgetrf")
        use iso_c_binding
        integer(c_int),            value  :: m, n, lda
        real(c_float), target :: A(lda,*)
        integer(c_int),            target :: ipiv(*)
        integer(c_int),            target :: info  !! int*
    end subroutine

    subroutine magmaf_spotrf( uplo, n, A, lda, info ) &
    bind(C, name="magma_spotrf")
        use iso_c_binding
        integer(c_int),            value  :: uplo
        integer(c_int),            value  :: n, lda
        real(c_float), target :: A(lda,*)
        integer(c_int),            target :: info  !! int*
    end subroutine

    subroutine magmaf_sgeev( jobvl, jobvr, n, A, lda, w, VL, ldvl, VR, ldvr, &
                            work,  lwork, rwork, info) &
    bind(C, name="magma_sgeev")
        use iso_c_binding
        integer(c_int),            value  :: jobvl, jobvr
        integer(c_int),            value  :: n, lda, ldvl, ldvr, lwork
        real(c_float), target :: A(lda,*)
        real(c_float), target :: w(*)
        real(c_float), target :: VR(ldvr,*),  VL(ldvl,*)
        real(c_float), target :: work(*)
        real(c_float),            target :: rwork(*)
        integer(c_int),            target :: info  !! int*
    end subroutine

    subroutine magmaf_sgeev_m( jobvl, jobvr, n, A, lda, w, VL, ldvl, VR, ldvr, &
                            work,  lwork, rwork, info) &
    bind(C, name="magma_sgeev_m")
        use iso_c_binding
        integer(c_int),            value  :: jobvl, jobvr
        integer(c_int),            value  :: n, lda, ldvl, ldvr, lwork
        real(c_float), target :: A(lda,*)
        real(c_float), target :: w(*)
        real(c_float), target :: VR(ldvr,*),  VL(ldvl,*)
        real(c_float), target :: work(*)
        real(c_float),            target :: rwork(*)
        integer(c_int),            target :: info  !! int*
    end subroutine

    subroutine magmaf_ssyevd_m( ngpu, jobz, uplo, n, A, lda, w, work,  lwork, &
                               rwork, lrwork, iwork, liwork, info) &
    bind(C, name="magma_ssyevd_m")
        use iso_c_binding
        integer(c_int),            value  :: ngpu
        integer(c_int),            value  :: jobz, uplo
        integer(c_int),            value  :: n, lda, lwork, lrwork, liwork
        real(c_float), target :: A(lda,*)
        real(c_float),             target :: w(*)
        real(c_float), target :: work(*)
        real(c_float),             target :: rwork(*)
        integer(c_int),            target :: iwork(*)
        integer(c_int),            target :: info  !! int*
    end subroutine

    !! -------------------------------------------------------------------------
    !! -------------------------------------------------------------------------
    !! GPU interfaces (matrix in GPU memory)
    subroutine magmaf_sgetrf_gpu( m, n, dA, lda, ipiv, info ) &
    bind(C, name="magma_sgetrf_gpu")
        use iso_c_binding
        integer(c_int), value  :: m, n, lda
        type(c_ptr),    value  :: dA
        integer(c_int), target :: ipiv(*)
        integer(c_int), target :: info  !! int*
    end subroutine

    subroutine magmaf_spotrf_gpu( uplo, n, dA, lda, info ) &
    bind(C, name="magma_spotrf_gpu")
        use iso_c_binding
        integer(c_int), value  :: uplo, n, lda
        type(c_ptr),    value  :: dA
        integer(c_int), target :: info  !! int*
    end subroutine

    subroutine magmaf_ssyevd_gpu( jobz, uplo, n, dA, ldda, w, wA, ldwa, work, lwork, &
                                 rwork, lrwork, iwork, liwork, info) &
    bind(C, name="magma_ssyevd_gpu")
        use iso_c_binding
        integer(c_int),            value  :: jobz, uplo
        integer(c_int),            value  :: n, ldda, ldwa, lwork, lrwork, liwork
        type(c_ptr),               value  :: dA  !! double real**
        real(c_double),            target :: w(*)
        real(c_float), target :: wA(*)
        real(c_float), target :: work(*)
        real(c_double),            target :: rwork(*)
        integer(c_int),            target :: iwork(*)
        integer(c_int),            target :: info  !! int*
    end subroutine

    !! -------------------------------------------------------------------------
    !! batched GPU interfaces (all arrays in GPU memory)
    subroutine magmaf_sgetrf_batched( &
        m, n, dA_array, lda, ipiv_array, info_array, batchcount, queue ) &
    bind(C, name="magma_sgetrf_batched")
        use iso_c_binding
        integer(c_int), value  :: m, n, lda, batchcount
        type(c_ptr),    value  :: dA_array    !! double_real**
        type(c_ptr),    value  :: ipiv_array  !! int**
        type(c_ptr),    value  :: info_array  !! int*
        type(c_ptr),    value  :: queue
    end subroutine

    !! -------------------------------------------------------------------------
    !! BLAS (matrices in GPU memory)
    subroutine magmaf_saxpy( &
        n, &
        alpha, dx, incx, &
               dy, incy, &
        queue ) &
    bind(C, name="magma_saxpy")
        use iso_c_binding
        integer(c_int),             value :: n, incx, incy
        real(c_float),  value :: alpha
        type(c_ptr),                value :: dx, dy
        type(c_ptr),                value :: queue  !! queue_t
    end subroutine

    subroutine magmaf_sgemv( &
        transA, m, n, &
        alpha, dA, lda, &
               dx, incx, &
        beta,  dy, incy, &
        queue ) &
    bind(C, name="magma_sgemv")
        use iso_c_binding
        integer(c_int),             value :: transA, m, n, lda, incx, incy
        real(c_float),  value :: alpha, beta
        type(c_ptr),                value :: dA, dx, dy
        type(c_ptr),                value :: queue  !! queue_t
    end subroutine

    subroutine magmaf_sgemm( &
        transA, transB, m, n, k, &
        alpha, dA, lda, &
               dB, ldb, &
        beta,  dC, ldc, &
        queue ) &
    bind(C, name="magma_sgemm")
        use iso_c_binding
        integer(c_int),             value :: transA, transB, m, n, k, lda, ldb, ldc
        real(c_float),  value :: alpha, beta
        type(c_ptr),                value :: dA, dB, dC
        type(c_ptr),                value :: queue  !! queue_t
    end subroutine

end interface

!! =============================================================================
!! Fortran routines & functions
contains

    !! -------------------------------------------------------------------------
    !! malloc wrappers
    integer(c_int) function magmaf_smalloc( ptr, n )
        use iso_c_binding
        type(c_ptr),       target :: ptr  !! void**
        integer(c_size_t), value  :: n
        
        magmaf_smalloc = magmaf_malloc( ptr, n*sizeof_real )
    end function

    integer(c_int) function magmaf_smalloc_cpu( ptr, n )
        use iso_c_binding
        type(c_ptr),       target :: ptr  !! void**
        integer(c_size_t), value  :: n
        
        magmaf_smalloc_cpu = magmaf_malloc_cpu( ptr, n*sizeof_real )
    end function

    integer(c_int) function magmaf_smalloc_pinned( ptr, n )
        use iso_c_binding
        type(c_ptr),       target :: ptr  !! void**
        integer(c_size_t), value  :: n
        
        magmaf_smalloc_pinned = magmaf_malloc_pinned( ptr, n*sizeof_real )
    end function

    !! -------------------------------------------------------------------------
    !! set/get wrappers
    subroutine magmaf_ssetmatrix( &
        m, n, hA_src, lda, dB_dst, ldb, queue )
        use iso_c_binding
        integer(c_int),            value  :: m, n, lda, ldb
        real(c_float), target :: hA_src(lda,*)
        type(c_ptr),               value  :: dB_dst
        type(c_ptr),               value  :: queue
        
        call magmaf_setmatrix_internal( &
                m, n, int(sizeof_real), c_loc(hA_src), lda, dB_dst, ldb, queue, &
                "magma_ssetmatrix" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

    subroutine magmaf_sgetmatrix( &
        m, n, dA_src, lda, hB_dst, ldb, queue )
        use iso_c_binding
        integer(c_int),            value  :: m, n, lda, ldb
        type(c_ptr),               value  :: dA_src
        real(c_float), target :: hB_dst(ldb,*)
        type(c_ptr),               value  :: queue
        
        call magmaf_getmatrix_internal( &
                m, n, int(sizeof_real), dA_src, lda, c_loc(hB_dst), ldb, queue, &
                "magma_sgetmatrix" // c_null_char, &
                __FILE__ // c_null_char, &
                __LINE__ )
    end subroutine

end module
