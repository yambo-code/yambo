!
!   -- MAGMA (version 2.8.0) --
!      Univ. of Tennessee, Knoxville
!      Univ. of California, Berkeley
!      Univ. of Colorado, Denver
!      @date March 2024
!

module magma

    use magma_param
    use iso_fortran_env, ONLY: int64

#define magma_devptr_t integer(int64)

    use magma_zfortran
    use magma_cfortran
!    use magma_dfortran
!    use magma_sfortran

!    use magmablas_zfortran
!    use magmablas_dfortran
!    use magmablas_cfortran
!    use magmablas_sfortran

    !---- Fortran interfaces to MAGMA subroutines ----
    interface

    !! -------------------------------------------------------------------------
    !! initialize
    subroutine magmaf_init( )
    end subroutine

    subroutine magmaf_finalize(  )
    end subroutine

    !! -------------------------------------------------------------------------
    !! version
    subroutine magmaf_version( major, minor, micro )
        integer         :: major, minor, micro
    end subroutine

    subroutine magmaf_print_environment()
    end subroutine

    !! -------------------------------------------------------------------------
    !! device support
    integer function magmaf_num_gpus()
    end function

    integer function magmaf_getdevice_arch()
    end function

    subroutine magmaf_getdevice( dev )
        integer         :: dev
    end subroutine

    subroutine magmaf_setdevice( dev )
        integer         :: dev
    end subroutine

    function magmaf_mem_size( queue )
        import int64
        integer(kind=8) :: magmaf_mem_size
        magma_devptr_t  :: queue
    end function

    !! -------------------------------------------------------------------------
    !! queue support
    subroutine magmaf_queue_create( dev, queue )
        import int64
        integer        :: dev
        magma_devptr_t :: queue
    end subroutine

    subroutine magmaf_queue_destroy( queue )
        import int64
        magma_devptr_t :: queue
    end subroutine

    subroutine magmaf_queue_sync( queue )
        import int64
        magma_devptr_t :: queue
    end subroutine

    integer function magmaf_queue_get_device( queue )
        import int64
        magma_devptr_t :: queue
    end function

    !! -------------------------------------------------------------------------
    !! GPU allocation
    integer function magmaf_malloc( ptr, bytes )
        import int64
        magma_devptr_t  :: ptr
        integer         :: bytes
    end function

    integer function magmaf_smalloc( ptr, n )
        import int64
        magma_devptr_t  :: ptr
        integer         :: n
    end function

    integer function magmaf_dmalloc( ptr, n )
        import int64
        magma_devptr_t  :: ptr
        integer         :: n
    end function

    integer function magmaf_cmalloc( ptr, n )
        import int64
        magma_devptr_t  :: ptr
        integer         :: n
    end function

    integer function magmaf_zmalloc( ptr, n )
        import int64
        magma_devptr_t  :: ptr
        integer         :: n
    end function

    integer function magmaf_free( ptr )
        import int64
        magma_devptr_t  :: ptr
    end function

    !! -------------------------------------------------------------------------
    !! CPU regular (non-pinned) allocation
    integer function magmaf_malloc_cpu( ptr, bytes )
        import int64
        magma_devptr_t  :: ptr
        integer         :: bytes
    end function

    integer function magmaf_smalloc_cpu( ptr, n )
        import int64
        magma_devptr_t  :: ptr
        integer         :: n
    end function

    integer function magmaf_dmalloc_cpu( ptr, n )
        import int64
        magma_devptr_t  :: ptr
        integer         :: n
    end function

    integer function magmaf_cmalloc_cpu( ptr, n )
        import int64
        magma_devptr_t  :: ptr
        integer         :: n
    end function

    integer function magmaf_zmalloc_cpu( ptr, n )
        import int64
        magma_devptr_t  :: ptr
        integer         :: n
    end function

    integer function magmaf_free_cpu( ptr )
        import int64
        magma_devptr_t  :: ptr
    end function

    !! -------------------------------------------------------------------------
    !! CPU pinned allocation
    integer function magmaf_malloc_pinned( ptr, bytes )
        import int64
        magma_devptr_t  :: ptr
        integer         :: bytes
    end function

    integer function magmaf_smalloc_pinned( ptr, n )
        import int64
        magma_devptr_t  :: ptr
        integer         :: n
    end function

    integer function magmaf_dmalloc_pinned( ptr, n )
        import int64
        magma_devptr_t  :: ptr
        integer         :: n
    end function

    integer function magmaf_cmalloc_pinned( ptr, n )
        import int64
        magma_devptr_t  :: ptr
        integer         :: n
    end function

    integer function magmaf_zmalloc_pinned( ptr, n )
        import int64
        magma_devptr_t  :: ptr
        integer         :: n
    end function

    integer function magmaf_free_pinned( ptr )
        import int64
        magma_devptr_t  :: ptr
    end function

    !! -------------------------------------------------------------------------
    !! timing; see magma_timer.cpp
    subroutine magmaf_wtime( time )
        double precision :: time
    end subroutine

    end interface

    !! -------------------------------------------------------------------------
    ! parameter constants from magma_types.h
    ! currently MAGMA's Fortran interface uses characters, not integers
    character, parameter :: &
        MagmaFalse         = 'n',  &
        MagmaTrue          = 'y',  &
        MagmaRowMajor      = 'r',  &
        MagmaColMajor      = 'c',  &
        MagmaNoTrans       = 'n',  &
        MagmaTrans         = 't',  &
        MagmaConjTrans     = 'c',  &
        MagmaUpper         = 'u',  &
        MagmaLower         = 'l',  &
        MagmaFull          = 'f',  &
        MagmaNonUnit       = 'n',  &
        MagmaUnit          = 'u',  &
        MagmaLeft          = 'l',  &
        MagmaRight         = 'r',  &
        MagmaBothSides     = 'b',  &
        MagmaOneNorm       = '1',  &
        MagmaTwoNorm       = '2',  &
        MagmaFrobeniusNorm = 'f',  &
        MagmaInfNorm       = 'i',  &
        MagmaMaxNorm       = 'm',  &
        MagmaDistUniform   = 'u',  &
        MagmaDistSymmetric = 's',  &
        MagmaDistNormal    = 'n',  &
        MagmaHermGeev      = 'h',  &
        MagmaHermPoev      = 'p',  &
        MagmaNonsymPosv    = 'n',  &
        MagmaSymPosv       = 's',  &
        MagmaNoPacking     = 'n',  &
        MagmaPackSubdiag   = 'u',  &
        MagmaPackSupdiag   = 'l',  &
        MagmaPackColumn    = 'c',  &
        MagmaPackRow       = 'r',  &
        MagmaPackLowerBand = 'b',  &
        MagmaPackUpeprBand = 'q',  &
        MagmaPackAll       = 'z',  &
        MagmaNoVec         = 'n',  &
        MagmaVec           = 'v',  &
        MagmaIVec          = 'i',  &
        MagmaAllVec        = 'a',  &
        MagmaSomeVec       = 's',  &
        MagmaOverwriteVec  = 'o',  &
        MagmaBacktransVec  = 'b',  &
        MagmaRangeAll      = 'a',  &
        MagmaRangeV        = 'v',  &
        MagmaRangeI        = 'i',  &
        MagmaQ             = 'q',  &
        MagmaP             = 'p',  &
        MagmaForward       = 'f',  &
        MagmaBackward      = 'b',  &
        MagmaColumnwise    = 'c',  &
        MagmaRowwise       = 'r'

contains

! --------------------
!> Sets ptrNew = ptrOld( i ), with stride inc.
!! Useful because CUDA pointers are opaque types in Fortran.
subroutine magmaf_soff1d( ptrNew, ptrOld, inc, i )
    magma_devptr_t   :: ptrNew
    magma_devptr_t   :: ptrOld
    integer          :: inc, i

    ptrNew = ptrOld + (i-1) * inc * sizeof_real
end subroutine magmaf_soff1d

!> Sets ptrNew = ptrOld( i, j ), with leading dimension lda.
!! Useful because CUDA pointers are opaque types in Fortran.
subroutine magmaf_soff2d( ptrNew, ptrOld, lda, i, j )
    magma_devptr_t   :: ptrNew
    magma_devptr_t   :: ptrOld
    integer          :: lda, i, j

    ptrNew = ptrOld + ((j-1) * lda + (i-1)) * sizeof_real
end subroutine magmaf_soff2d

! --------------------
!> Sets ptrNew = ptrOld( i ), with stride inc.
!! Useful because CUDA pointers are opaque types in Fortran.
subroutine magmaf_doff1d( ptrNew, ptrOld, inc, i )
    magma_devptr_t   :: ptrNew
    magma_devptr_t   :: ptrOld
    integer          :: inc, i

    ptrNew = ptrOld + (i-1) * inc * sizeof_double
end subroutine magmaf_doff1d

!> Sets ptrNew = ptrOld( i, j ), with leading dimension lda.
!! Useful because CUDA pointers are opaque types in Fortran.
subroutine magmaf_doff2d( ptrNew, ptrOld, lda, i, j )
    magma_devptr_t   :: ptrNew
    magma_devptr_t   :: ptrOld
    integer          :: lda, i, j

    ptrNew = ptrOld + ((j-1) * lda + (i-1)) * sizeof_double
end subroutine magmaf_doff2d

! --------------------
!> Sets ptrNew = ptrOld( i ), with stride inc.
!! Useful because CUDA pointers are opaque types in Fortran.
subroutine magmaf_coff1d( ptrNew, ptrOld, inc, i )
    magma_devptr_t   :: ptrNew
    magma_devptr_t   :: ptrOld
    integer          :: inc, i

    ptrNew = ptrOld + (i-1) * inc * sizeof_complex
end subroutine magmaf_coff1d

!> Sets ptrNew = ptrOld( i, j ), with leading dimension lda.
!! Useful because CUDA pointers are opaque types in Fortran.
subroutine magmaf_coff2d( ptrNew, ptrOld, lda, i, j )
    magma_devptr_t   :: ptrNew
    magma_devptr_t   :: ptrOld
    integer          :: lda, i, j

    ptrNew = ptrOld + ((j-1) * lda + (i-1)) * sizeof_complex
end subroutine magmaf_coff2d

! --------------------
!> Sets ptrNew = ptrOld( i ), with stride inc.
!! Useful because CUDA pointers are opaque types in Fortran.
subroutine magmaf_zoff1d( ptrNew, ptrOld, inc, i )
    magma_devptr_t   :: ptrNew
    magma_devptr_t   :: ptrOld
    integer          :: inc, i

    ptrNew = ptrOld + (i-1) * inc * sizeof_complex_16
end subroutine magmaf_zoff1d

!> Sets ptrNew = ptrOld( i, j ), with leading dimension lda.
!! Useful because CUDA pointers are opaque types in Fortran.
subroutine magmaf_zoff2d( ptrNew, ptrOld, lda, i, j )
    magma_devptr_t   :: ptrNew
    magma_devptr_t   :: ptrOld
    integer          :: lda, i, j

    ptrNew = ptrOld + ((j-1) * lda + (i-1)) * sizeof_complex_16
end subroutine magmaf_zoff2d

! --------------------
!> Sets ptrNew = ptrOld( i ), with stride inc.
!! Useful because CUDA pointers are opaque types in Fortran.
subroutine magmaf_ioff1d( ptrNew, ptrOld, inc, i )
    magma_devptr_t   :: ptrNew
    magma_devptr_t   :: ptrOld
    integer          :: inc, i

    ptrNew = ptrOld + (i-1) * inc * sizeof_integer
end subroutine magmaf_ioff1d

!> Sets ptrNew = ptrOld( i, j ), with leading dimension lda.
!! Useful because CUDA pointers are opaque types in Fortran.
subroutine magmaf_ioff2d( ptrNew, ptrOld, lda, i, j )
    magma_devptr_t   :: ptrNew
    magma_devptr_t   :: ptrOld
    integer          :: lda, i, j

    ptrNew = ptrOld + ((j-1) * lda + (i-1)) * sizeof_integer
end subroutine magmaf_ioff2d

end module magma
