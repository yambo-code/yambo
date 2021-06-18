!!  Fortran CUDA Library interface -- CUBLAS (Legacy) module
!!
!!  Copyright (C) 2017-2018 Mentor, A Siemens Business
!!
!!  This software is provided 'as-is', without any express or implied
!!  warranty.  In no event will the authors be held liable for any damages
!!  arising from the use of this software.
!!
!!  Permission is granted to anyone to use this software for any purpose,
!!  including commercial applications, and to alter it and redistribute it
!!  freely, subject to the following restrictions:
!!
!!  1. The origin of this software must not be misrepresented; you must not
!!     claim that you wrote the original software. If you use this software
!!     in a product, an acknowledgment in the product documentation would be
!!     appreciated but is not required.
!!  2. Altered source versions must be plainly marked as such, and must not be
!!     misrepresented as being the original software.
!!  3. This notice may not be removed or altered from any source distribution.

module cublas_m
  use iso_c_binding

  enum, bind(c)
     enumerator :: CUBLAS_STATUS_SUCCESS          = 0
     enumerator :: CUBLAS_STATUS_NOT_INITIALIZED  = 1
     enumerator :: CUBLAS_STATUS_ALLOC_FAILED     = 3
     enumerator :: CUBLAS_STATUS_INVALID_VALUE    = 7
     enumerator :: CUBLAS_STATUS_ARCH_MISMATCH    = 8
     enumerator :: CUBLAS_STATUS_MAPPING_ERROR    = 11
     enumerator :: CUBLAS_STATUS_EXECUTION_FAILED = 13
     enumerator :: CUBLAS_STATUS_INTERNAL_ERROR   = 14
     enumerator :: CUBLAS_STATUS_NOT_SUPPORTED    = 15
     enumerator :: CUBLAS_STATUS_LICENSE_ERROR    = 16
  end enum

  interface
    integer(c_int) function cublasInit () &
        bind (c, name="cublasInit")
      import
    end function cublasInit

    integer(c_int) function cublasShutdown () &
        bind (c, name="cublasShutdown")
      import
    end function cublasShutdown

    integer(c_int) function cublasGetError () &
        bind (c, name="cublasGetError")
      import
    end function cublasGetError

    integer(c_int) function cublasGetVersion (version) &
        bind (c, name="cublasGetVersion")
      import
      integer(c_int) :: version
    end function cublasGetVersion

    integer(c_int) function cublasSetKernelStream (stream) &
        bind (c, name="cublasSetKernelStream")
      import
      type(c_ptr), value :: stream
    end function cublasSetKernelStream

    real(c_float) function cublasSnrm2 (n, x, incx) &
        bind (c, name="cublasSnrm2")
      import
      integer(c_int), value :: n, incx
      real(c_float), dimension(*) :: x
    end function cublasSnrm2

    real(c_double) function cublasDnrm2 (n, x, incx) &
        bind (c, name="cublasDnrm2")
      import
      real(c_double), dimension(*) :: x
      integer(c_int), value :: n, incx
    end function cublasDnrm2

    real(c_float) function cublasScnrm2 (n, x, incx) &
        bind (c, name="cublasScnrm2")
      import
      complex(c_float), dimension(*) :: x
      integer(c_int), value :: n, incx
    end function cublasScnrm2

    real(c_double) function cublasDznrm2 (n, x, incx) &
        bind (c, name="cublasDznrm2")
      import
      integer(c_int), value :: n, incx
      complex(c_double), dimension(*) :: x
    end function cublasDznrm2

    real(c_float) function cublasSdot (n, x, incx, y, incy) &
        bind (c, name="cublasSdot")
      import
      integer(c_int), value :: n, incx, incy
      real(c_float), dimension(*) :: x, y
    end function cublasSdot

    real(c_double) function cublasDdot (n, x, incx, y, incy) &
        bind (c, name="cublasDdot")
      import
      real(c_double), dimension(*) :: x, y
      integer(c_int), value :: n, incx, incy
    end function cublasDdot

    complex(c_float) function cublasCdotu (n, x, incx, y, incy) &
        bind (c, name="cublasCdotu")
      import
      complex(c_float), dimension(*) :: x, y
      integer(c_int), value :: n, incx, incy
    end function cublasCdotu

    complex(c_double) function cublasZdotu (n, x, incx, y, incy) &
        bind (c, name="cublasZdotu")
      import
      integer(c_int), value :: n, incx, incy
      complex(c_double), dimension(*) :: x, y
    end function cublasZdotu

    complex(c_float) function cublasCdotc (n, x, incx, y, incy) &
        bind (c, name="cublasCdotc")
      import
      complex(c_float), dimension(*) :: x, y
      integer(c_int), value :: n, incx, incy
    end function cublasCdotc

    complex(c_double) function cublasZdotc (n, x, incx, y, incy) &
        bind (c, name="cublasZdotc")
      import
      integer(c_int), value :: n, incx, incy
      complex(c_double), dimension(*) :: x, y
    end function cublasZdotc

    subroutine cublasSscal (n, alpha, x, incx) &
        bind (c, name="cublasSscal")
      import
      integer(c_int), value :: n, incx
      real(c_float), value :: alpha
      real(c_float), dimension(*) :: x
    end subroutine cublasSscal

    subroutine cublasDscal (n, alpha, x, incx) &
        bind (c, name="cublasDscal")
      import
      real(c_double), dimension(*) :: x
      integer(c_int), value :: n, incx
      real(c_double), value :: alpha
    end subroutine cublasDscal

    subroutine cublasCscal (n, alpha, x, incx) &
        bind (c, name="cublasCscal")
      import
      complex(c_float), dimension(*) :: x
      integer(c_int), value :: n, incx
      complex(c_float), value :: alpha
    end subroutine cublasCscal

    subroutine cublasZscal (n, alpha, x, incx) &
        bind (c, name="cublasZscal")
      import
      complex(c_double), value :: alpha
      integer(c_int), value :: n, incx
      complex(c_double), dimension(*) :: x
    end subroutine cublasZscal

    subroutine cublasCsscal (n, alpha, x, incx) &
        bind (c, name="cublasCsscal")
      import
      complex(c_float), dimension(*) :: x
      integer(c_int), value :: n, incx
      real(c_float), value :: alpha
    end subroutine cublasCsscal

    subroutine cublasZdscal (n, alpha, x, incx) &
        bind (c, name="cublasZdscal")
      import
      integer(c_int), value :: n, incx
      real(c_double), value :: alpha
      complex(c_double), dimension(*) :: x
    end subroutine cublasZdscal

    subroutine cublasSaxpy (n, alpha, x, incx, y, incy) &
        bind (c, name="cublasSaxpy")
      import
      integer(c_int), value :: n, incx, incy
      real(c_float), value :: alpha
      real(c_float), dimension(*) :: x, y
    end subroutine cublasSaxpy

    subroutine cublasDaxpy (n, alpha, x, incx, y, incy) &
        bind (c, name="cublasDaxpy")
      import
      real(c_double), dimension(*) :: x, y
      integer(c_int), value :: n, incx, incy
      real(c_double), value :: alpha
    end subroutine cublasDaxpy

    subroutine cublasCaxpy (n, alpha, x, incx, y, incy) &
        bind (c, name="cublasCaxpy")
      import
      complex(c_float), dimension(*) :: x, y
      integer(c_int), value :: n, incx, incy
      complex(c_float), value :: alpha
    end subroutine cublasCaxpy

    subroutine cublasZaxpy (n, alpha, x, incx, y, incy) &
        bind (c, name="cublasZaxpy")
      import
      complex(c_double), value :: alpha
      integer(c_int), value :: n, incx, incy
      complex(c_double), dimension(*) :: x, y
    end subroutine cublasZaxpy

    subroutine cublasScopy (n, x, incx, y, incy) &
        bind (c, name="cublasScopy")
      import
      integer(c_int), value :: n, incx, incy
      real(c_float), dimension(*) :: x, y
    end subroutine cublasScopy

    subroutine cublasDcopy (n, x, incx, y, incy) &
        bind (c, name="cublasDcopy")
      import
      real(c_double), dimension(*) :: x, y
      integer(c_int), value :: n, incx, incy
    end subroutine cublasDcopy

    subroutine cublasCcopy (n, x, incx, y, incy) &
        bind (c, name="cublasCcopy")
      import
      complex(c_float), dimension(*) :: x, y
      integer(c_int), value :: n, incx, incy
    end subroutine cublasCcopy

    subroutine cublasZcopy (n, x, incx, y, incy) &
        bind (c, name="cublasZcopy")
      import
      integer(c_int), value :: n, incx, incy
      complex(c_double), dimension(*) :: x, y
    end subroutine cublasZcopy

    subroutine cublasSswap (n, x, incx, y, incy) &
        bind (c, name="cublasSswap")
      import
      integer(c_int), value :: n, incx, incy
      real(c_float), dimension(*) :: x, y
    end subroutine cublasSswap

    subroutine cublasDswap (n, x, incx, y, incy) &
        bind (c, name="cublasDswap")
      import
      real(c_double), dimension(*) :: x, y
      integer(c_int), value :: n, incx, incy
    end subroutine cublasDswap

    subroutine cublasCswap (n, x, incx, y, incy) &
        bind (c, name="cublasCswap")
      import
      complex(c_float), dimension(*) :: x, y
      integer(c_int), value :: n, incx, incy
    end subroutine cublasCswap

    subroutine cublasZswap (n, x, incx, y, incy) &
        bind (c, name="cublasZswap")
      import
      integer(c_int), value :: n, incx, incy
      complex(c_double), dimension(*) :: x, y
    end subroutine cublasZswap

    integer(c_int) function cublasIsamax (n, x, incx) &
        bind (c, name="cublasIsamax")
      import
      integer(c_int), value :: n, incx
      real(c_float), dimension(*) :: x
    end function cublasIsamax

    integer(c_int) function cublasIdamax (n, x, incx) &
        bind (c, name="cublasIdamax")
      import
      real(c_double), dimension(*) :: x
      integer(c_int), value :: n, incx
    end function cublasIdamax

    integer(c_int) function cublasIcamax (n, x, incx) &
        bind (c, name="cublasIcamax")
      import
      complex(c_float), dimension(*) :: x
      integer(c_int), value :: n, incx
    end function cublasIcamax

    integer(c_int) function cublasIzamax (n, x, incx) &
        bind (c, name="cublasIzamax")
      import
      integer(c_int), value :: n, incx
      complex(c_double), dimension(*) :: x
    end function cublasIzamax

    integer(c_int) function cublasIsamin (n, x, incx) &
        bind (c, name="cublasIsamin")
      import
      integer(c_int), value :: n, incx
      real(c_float), dimension(*) :: x
    end function cublasIsamin

    integer(c_int) function cublasIdamin (n, x, incx) &
        bind (c, name="cublasIdamin")
      import
      real(c_double), dimension(*) :: x
      integer(c_int), value :: n, incx
    end function cublasIdamin

    integer(c_int) function cublasIcamin (n, x, incx) &
        bind (c, name="cublasIcamin")
      import
      complex(c_float), dimension(*) :: x
      integer(c_int), value :: n, incx
    end function cublasIcamin

    integer(c_int) function cublasIzamin (n, x, incx) &
        bind (c, name="cublasIzamin")
      import
      integer(c_int), value :: n, incx
      complex(c_double), dimension(*) :: x
    end function cublasIzamin

    real(c_float) function cublasSasum (n, x, incx) &
        bind (c, name="cublasSasum")
      import
      integer(c_int), value :: n, incx
      real(c_float), dimension(*) :: x
    end function cublasSasum

    real(c_double) function cublasDasum (n, x, incx) &
        bind (c, name="cublasDasum")
      import
      real(c_double), dimension(*) :: x
      integer(c_int), value :: n, incx
    end function cublasDasum

    real(c_float) function cublasScasum (n, x, incx) &
        bind (c, name="cublasScasum")
      import
      complex(c_float), dimension(*) :: x
      integer(c_int), value :: n, incx
    end function cublasScasum

    real(c_double) function cublasDzasum (n, x, incx) &
        bind (c, name="cublasDzasum")
      import
      integer(c_int), value :: n, incx
      complex(c_double), dimension(*) :: x
    end function cublasDzasum

    subroutine cublasSrot (n, x, incx, y, incy, sc, ss) &
        bind (c, name="cublasSrot")
      import
      integer(c_int), value :: n, incx, incy
      real(c_float), value :: sc, ss
      real(c_float), dimension(*) :: x, y
    end subroutine cublasSrot

    subroutine cublasDrot (n, x, incx, y, incy, sc, ss) &
        bind (c, name="cublasDrot")
      import
      real(c_double), dimension(*) :: x, y
      integer(c_int), value :: n, incx, incy
      real(c_double), value :: sc, ss
    end subroutine cublasDrot

    subroutine cublasCrot (n, x, incx, y, incy, sc, ss) &
        bind (c, name="cublasCrot")
      import
      complex(c_float), dimension(*) :: x, y
      integer(c_int), value :: n, incx, incy
      complex(c_float), value :: sc, ss
    end subroutine cublasCrot

    subroutine cublasZrot (n, x, incx, y, incy, sc, ss) &
        bind (c, name="cublasZrot")
      import
      complex(c_double), value :: sc, ss
      integer(c_int), value :: n, incx, incy
      complex(c_double), dimension(*) :: x, y
    end subroutine cublasZrot

    subroutine cublasCsrot (n, x, incx, y, incy, c, s) &
        bind (c, name="cublasCsrot")
      import
      complex(c_float), dimension(*) :: x, y
      integer(c_int), value :: n, incx, incy
      real(c_float), value :: c, s
    end subroutine cublasCsrot

    subroutine cublasZdrot (n, x, incx, y, incy, c, s) &
        bind (c, name="cublasZdrot")
      import
      integer(c_int), value :: n, incx, incy
      real(c_double), value :: c, s
      complex(c_double), dimension(*) :: x, y
    end subroutine cublasZdrot

    subroutine cublasSrotg (sa, sb, sc, ss) &
        bind (c, name="cublasSrotg")
      import
      real(c_float) :: sa, sb, sc, ss
    end subroutine cublasSrotg

    subroutine cublasDrotg (sa, sb, sc, ss) &
        bind (c, name="cublasDrotg")
      import
      real(c_double) :: sa, sb, sc, ss
    end subroutine cublasDrotg

    subroutine cublasCrotg (sa, sb, sc, ss) &
        bind (c, name="cublasCrotg")
      import
      complex(c_float) :: sa, sb, sc, ss
    end subroutine cublasCrotg

    subroutine cublasZrotg (sa, sb, sc, ss) &
        bind (c, name="cublasZrotg")
      import
      complex(c_double) :: sa, sb, sc, ss
    end subroutine cublasZrotg

    subroutine cublasSrotm (n, x, incx, y, incy, param) &
        bind (c, name="cublasSrotm")
      import
      integer(c_int), value :: n, incx, incy
      real(c_float), dimension(*) :: x, y, param
    end subroutine cublasSrotm

    subroutine cublasDrotm (n, x, incx, y, incy, param) &
        bind (c, name="cublasDrotm")
      import
      real(c_double), dimension(*) :: x, y, param
      integer(c_int), value :: n, incx, incy
    end subroutine cublasDrotm

    subroutine cublasSrotmg (d1, d2, x1, y1, param) &
        bind (c, name="cublasSrotmg")
      import
      real(c_float) :: d1, d2, x1, y1
      real(c_float), dimension(*) :: param
    end subroutine cublasSrotmg

    subroutine cublasDrotmg (d1, d2, x1, y1, param) &
        bind (c, name="cublasDrotmg")
      import
      real(c_double), dimension(*) :: param
      real(c_double) :: d1, d2, x1, y1
    end subroutine cublasDrotmg

    subroutine cublasSgemv (trans, m, n, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasSgemv")
      import
      integer(c_int), value :: m, n, lda, incx, incy
      real(c_float), dimension(*) :: x, y
      real(c_float), value :: alpha, beta
      character(c_char), value :: trans
      real(c_float), dimension(lda, *) :: A
    end subroutine cublasSgemv

    subroutine cublasDgemv (trans, m, n, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasDgemv")
      import
      integer(c_int), value :: m, n, lda, incx, incy
      real(c_double), dimension(lda, *) :: A
      real(c_double), value :: alpha, beta
      character(c_char), value :: trans
      real(c_double), dimension(*) :: x, y
    end subroutine cublasDgemv

    subroutine cublasCgemv (trans, m, n, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasCgemv")
      import
      integer(c_int), value :: m, n, lda, incx, incy
      complex(c_float), dimension(*) :: x, y
      character(c_char), value :: trans
      complex(c_float), dimension(lda, *) :: A
      complex(c_float), value :: alpha, beta
    end subroutine cublasCgemv

    subroutine cublasZgemv (trans, m, n, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasZgemv")
      import
      integer(c_int), value :: m, n, lda, incx, incy
      complex(c_double), dimension(*) :: x, y
      complex(c_double), value :: alpha, beta
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: trans
    end subroutine cublasZgemv

    subroutine cublasSgbmv &
        (trans, m, n, kl, ku, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasSgbmv")
      import
      integer(c_int), value :: m, n, kl, ku, lda, incx, incy
      real(c_float), dimension(*) :: x, y
      real(c_float), value :: alpha, beta
      character(c_char), value :: trans
      real(c_float), dimension(lda, *) :: A
    end subroutine cublasSgbmv

    subroutine cublasDgbmv &
        (trans, m, n, kl, ku, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasDgbmv")
      import
      integer(c_int), value :: m, n, kl, ku, lda, incx, incy
      real(c_double), dimension(lda, *) :: A
      real(c_double), value :: alpha, beta
      character(c_char), value :: trans
      real(c_double), dimension(*) :: x, y
    end subroutine cublasDgbmv

    subroutine cublasCgbmv &
        (trans, m, n, kl, ku, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasCgbmv")
      import
      integer(c_int), value :: m, n, kl, ku, lda, incx, incy
      complex(c_float), dimension(*) :: x, y
      character(c_char), value :: trans
      complex(c_float), dimension(lda, *) :: A
      complex(c_float), value :: alpha, beta
    end subroutine cublasCgbmv

    subroutine cublasZgbmv &
        (trans, m, n, kl, ku, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasZgbmv")
      import
      integer(c_int), value :: m, n, kl, ku, lda, incx, incy
      complex(c_double), dimension(*) :: x, y
      complex(c_double), value :: alpha, beta
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: trans
    end subroutine cublasZgbmv

    subroutine cublasStrmv (uplo, trans, diag, n, A, lda, x, incx) &
        bind (c, name="cublasStrmv")
      import
      integer(c_int), value :: n, lda, incx
      real(c_float), dimension(*) :: x
      character(c_char), value :: uplo, trans, diag
      real(c_float), dimension(lda, *) :: A
    end subroutine cublasStrmv

    subroutine cublasDtrmv (uplo, trans, diag, n, A, lda, x, incx) &
        bind (c, name="cublasDtrmv")
      import
      integer(c_int), value :: n, lda, incx
      real(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans, diag
      real(c_double), dimension(*) :: x
    end subroutine cublasDtrmv

    subroutine cublasCtrmv (uplo, trans, diag, n, A, lda, x, incx) &
        bind (c, name="cublasCtrmv")
      import
      integer(c_int), value :: n, lda, incx
      complex(c_float), dimension(*) :: x
      character(c_char), value :: uplo, trans, diag
      complex(c_float), dimension(lda, *) :: A
    end subroutine cublasCtrmv

    subroutine cublasZtrmv (uplo, trans, diag, n, A, lda, x, incx) &
        bind (c, name="cublasZtrmv")
      import
      integer(c_int), value :: n, lda, incx
      complex(c_double), dimension(*) :: x
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans, diag
    end subroutine cublasZtrmv

    subroutine cublasStbmv (uplo, trans, diag, n, k, A, lda, x, incx) &
        bind (c, name="cublasStbmv")
      import
      integer(c_int), value :: n, k, lda, incx
      real(c_float), dimension(*) :: x
      character(c_char), value :: uplo, trans, diag
      real(c_float), dimension(lda, *) :: A
    end subroutine cublasStbmv

    subroutine cublasDtbmv (uplo, trans, diag, n, k, A, lda, x, incx) &
        bind (c, name="cublasDtbmv")
      import
      integer(c_int), value :: n, k, lda, incx
      real(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans, diag
      real(c_double), dimension(*) :: x
    end subroutine cublasDtbmv

    subroutine cublasCtbmv (uplo, trans, diag, n, k, A, lda, x, incx) &
        bind (c, name="cublasCtbmv")
      import
      integer(c_int), value :: n, k, lda, incx
      complex(c_float), dimension(*) :: x
      character(c_char), value :: uplo, trans, diag
      complex(c_float), dimension(lda, *) :: A
    end subroutine cublasCtbmv

    subroutine cublasZtbmv (uplo, trans, diag, n, k, A, lda, x, incx) &
        bind (c, name="cublasZtbmv")
      import
      integer(c_int), value :: n, k, lda, incx
      complex(c_double), dimension(*) :: x
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans, diag
    end subroutine cublasZtbmv

    subroutine cublasStpmv (uplo, trans, diag, n, AP, x, incx) &
        bind (c, name="cublasStpmv")
      import
      integer(c_int), value :: n, incx
      character(c_char), value :: uplo, trans, diag
      real(c_float), dimension(*) :: AP, x
    end subroutine cublasStpmv

    subroutine cublasDtpmv (uplo, trans, diag, n, AP, x, incx) &
        bind (c, name="cublasDtpmv")
      import
      real(c_double), dimension(*) :: AP, x
      integer(c_int), value :: n, incx
      character(c_char), value :: uplo, trans, diag
    end subroutine cublasDtpmv

    subroutine cublasCtpmv (uplo, trans, diag, n, AP, x, incx) &
        bind (c, name="cublasCtpmv")
      import
      complex(c_float), dimension(*) :: AP, x
      integer(c_int), value :: n, incx
      character(c_char), value :: uplo, trans, diag
    end subroutine cublasCtpmv

    subroutine cublasZtpmv (uplo, trans, diag, n, AP, x, incx) &
        bind (c, name="cublasZtpmv")
      import
      integer(c_int), value :: n, incx
      complex(c_double), dimension(*) :: AP, x
      character(c_char), value :: uplo, trans, diag
    end subroutine cublasZtpmv

    subroutine cublasStrsv (uplo, trans, diag, n, A, lda, x, incx) &
        bind (c, name="cublasStrsv")
      import
      integer(c_int), value :: n, lda, incx
      real(c_float), dimension(*) :: x
      character(c_char), value :: uplo, trans, diag
      real(c_float), dimension(lda, *) :: A
    end subroutine cublasStrsv

    subroutine cublasDtrsv (uplo, trans, diag, n, A, lda, x, incx) &
        bind (c, name="cublasDtrsv")
      import
      integer(c_int), value :: n, lda, incx
      real(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans, diag
      real(c_double), dimension(*) :: x
    end subroutine cublasDtrsv

    subroutine cublasCtrsv (uplo, trans, diag, n, A, lda, x, incx) &
        bind (c, name="cublasCtrsv")
      import
      integer(c_int), value :: n, lda, incx
      complex(c_float), dimension(*) :: x
      character(c_char), value :: uplo, trans, diag
      complex(c_float), dimension(lda, *) :: A
    end subroutine cublasCtrsv

    subroutine cublasZtrsv (uplo, trans, diag, n, A, lda, x, incx) &
        bind (c, name="cublasZtrsv")
      import
      integer(c_int), value :: n, lda, incx
      complex(c_double), dimension(*) :: x
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans, diag
    end subroutine cublasZtrsv

    subroutine cublasStpsv (uplo, trans, diag, n, AP, x, incx) &
        bind (c, name="cublasStpsv")
      import
      integer(c_int), value :: n, incx
      character(c_char), value :: uplo, trans, diag
      real(c_float), dimension(*) :: AP, x
    end subroutine cublasStpsv

    subroutine cublasDtpsv (uplo, trans, diag, n, AP, x, incx) &
        bind (c, name="cublasDtpsv")
      import
      real(c_double), dimension(*) :: AP, x
      integer(c_int), value :: n, incx
      character(c_char), value :: uplo, trans, diag
    end subroutine cublasDtpsv

    subroutine cublasCtpsv (uplo, trans, diag, n, AP, x, incx) &
        bind (c, name="cublasCtpsv")
      import
      complex(c_float), dimension(*) :: AP, x
      integer(c_int), value :: n, incx
      character(c_char), value :: uplo, trans, diag
    end subroutine cublasCtpsv

    subroutine cublasZtpsv (uplo, trans, diag, n, AP, x, incx) &
        bind (c, name="cublasZtpsv")
      import
      integer(c_int), value :: n, incx
      complex(c_double), dimension(*) :: AP, x
      character(c_char), value :: uplo, trans, diag
    end subroutine cublasZtpsv

    subroutine cublasStbsv (uplo, trans, diag, n, k, A, lda, x, incx) &
        bind (c, name="cublasStbsv")
      import
      integer(c_int), value :: n, k, lda, incx
      real(c_float), dimension(*) :: x
      character(c_char), value :: uplo, trans, diag
      real(c_float), dimension(lda, *) :: A
    end subroutine cublasStbsv

    subroutine cublasDtbsv (uplo, trans, diag, n, k, A, lda, x, incx) &
        bind (c, name="cublasDtbsv")
      import
      integer(c_int), value :: n, k, lda, incx
      real(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans, diag
      real(c_double), dimension(*) :: x
    end subroutine cublasDtbsv

    subroutine cublasCtbsv (uplo, trans, diag, n, k, A, lda, x, incx) &
        bind (c, name="cublasCtbsv")
      import
      integer(c_int), value :: n, k, lda, incx
      complex(c_float), dimension(*) :: x
      character(c_char), value :: uplo, trans, diag
      complex(c_float), dimension(lda, *) :: A
    end subroutine cublasCtbsv

    subroutine cublasZtbsv (uplo, trans, diag, n, k, A, lda, x, incx) &
        bind (c, name="cublasZtbsv")
      import
      integer(c_int), value :: n, k, lda, incx
      complex(c_double), dimension(*) :: x
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans, diag
    end subroutine cublasZtbsv

    subroutine cublasSsymv (uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasSsymv")
      import
      integer(c_int), value :: n, lda, incx, incy
      real(c_float), dimension(*) :: x, y
      real(c_float), value :: alpha, beta
      character(c_char), value :: uplo
      real(c_float), dimension(lda, *) :: A
    end subroutine cublasSsymv

    subroutine cublasDsymv (uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasDsymv")
      import
      integer(c_int), value :: n, lda, incx, incy
      real(c_double), dimension(lda, *) :: A
      real(c_double), value :: alpha, beta
      character(c_char), value :: uplo
      real(c_double), dimension(*) :: x, y
    end subroutine cublasDsymv

    subroutine cublasChemv (uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasChemv")
      import
      integer(c_int), value :: n, lda, incx, incy
      complex(c_float), dimension(*) :: x, y
      character(c_char), value :: uplo
      complex(c_float), dimension(lda, *) :: A
      complex(c_float), value :: alpha, beta
    end subroutine cublasChemv

    subroutine cublasZhemv (uplo, n, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasZhemv")
      import
      integer(c_int), value :: n, lda, incx, incy
      complex(c_double), dimension(*) :: x, y
      complex(c_double), value :: alpha, beta
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo
    end subroutine cublasZhemv

    subroutine cublasSsbmv (uplo, n, k, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasSsbmv")
      import
      integer(c_int), value :: n, k, lda, incx, incy
      real(c_float), dimension(*) :: x, y
      real(c_float), value :: alpha, beta
      character(c_char), value :: uplo
      real(c_float), dimension(lda, *) :: A
    end subroutine cublasSsbmv

    subroutine cublasDsbmv (uplo, n, k, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasDsbmv")
      import
      integer(c_int), value :: n, k, lda, incx, incy
      real(c_double), dimension(lda, *) :: A
      real(c_double), value :: alpha, beta
      character(c_char), value :: uplo
      real(c_double), dimension(*) :: x, y
    end subroutine cublasDsbmv

    subroutine cublasChbmv (uplo, n, k, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasChbmv")
      import
      integer(c_int), value :: n, k, lda, incx, incy
      complex(c_float), dimension(*) :: x, y
      character(c_char), value :: uplo
      complex(c_float), dimension(lda, *) :: A
      complex(c_float), value :: alpha, beta
    end subroutine cublasChbmv

    subroutine cublasZhbmv (uplo, n, k, alpha, A, lda, x, incx, beta, y, incy) &
        bind (c, name="cublasZhbmv")
      import
      integer(c_int), value :: n, k, lda, incx, incy
      complex(c_double), dimension(*) :: x, y
      complex(c_double), value :: alpha, beta
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo
    end subroutine cublasZhbmv

    subroutine cublasSspmv (uplo, n, alpha, AP, x, incx, beta, y, incy) &
        bind (c, name="cublasSspmv")
      import
      integer(c_int), value :: n, incx, incy
      real(c_float), value :: alpha, beta
      character(c_char), value :: uplo
      real(c_float), dimension(*) :: AP, x, y
    end subroutine cublasSspmv

    subroutine cublasDspmv (uplo, n, alpha, AP, x, incx, beta, y, incy) &
        bind (c, name="cublasDspmv")
      import
      real(c_double), dimension(*) :: AP, x, y
      integer(c_int), value :: n, incx, incy
      real(c_double), value :: alpha, beta
      character(c_char), value :: uplo
    end subroutine cublasDspmv

    subroutine cublasChpmv (uplo, n, alpha, AP, x, incx, beta, y, incy) &
        bind (c, name="cublasChpmv")
      import
      complex(c_float), dimension(*) :: AP, x, y
      integer(c_int), value :: n, incx, incy
      character(c_char), value :: uplo
      complex(c_float), value :: alpha, beta
    end subroutine cublasChpmv

    subroutine cublasZhpmv (uplo, n, alpha, AP, x, incx, beta, y, incy) &
        bind (c, name="cublasZhpmv")
      import
      complex(c_double), value :: alpha, beta
      integer(c_int), value :: n, incx, incy
      complex(c_double), dimension(*) :: AP, x, y
      character(c_char), value :: uplo
    end subroutine cublasZhpmv

    subroutine cublasSger (m, n, alpha, x, incx, y, incy, A, lda) &
        bind (c, name="cublasSger")
      import
      integer(c_int), value :: m, n, incx, incy, lda
      real(c_float), value :: alpha
      real(c_float), dimension(*) :: x, y
      real(c_float), dimension(lda, *) :: A
    end subroutine cublasSger

    subroutine cublasDger (m, n, alpha, x, incx, y, incy, A, lda) &
        bind (c, name="cublasDger")
      import
      integer(c_int), value :: m, n, incx, incy, lda
      real(c_double), dimension(*) :: x, y
      real(c_double), value :: alpha
      real(c_double), dimension(lda, *) :: A
    end subroutine cublasDger

    subroutine cublasCgeru (m, n, alpha, x, incx, y, incy, A, lda) &
        bind (c, name="cublasCgeru")
      import
      integer(c_int), value :: m, n, incx, incy, lda
      complex(c_float), dimension(*) :: x, y
      complex(c_float), dimension(lda, *) :: A
      complex(c_float), value :: alpha
    end subroutine cublasCgeru

    subroutine cublasZgeru (m, n, alpha, x, incx, y, incy, A, lda) &
        bind (c, name="cublasZgeru")
      import
      integer(c_int), value :: m, n, incx, incy, lda
      complex(c_double), value :: alpha
      complex(c_double), dimension(lda, *) :: A
      complex(c_double), dimension(*) :: x, y
    end subroutine cublasZgeru

    subroutine cublasCgerc (m, n, alpha, x, incx, y, incy, A, lda) &
        bind (c, name="cublasCgerc")
      import
      integer(c_int), value :: m, n, incx, incy, lda
      complex(c_float), dimension(*) :: x, y
      complex(c_float), dimension(lda, *) :: A
      complex(c_float), value :: alpha
    end subroutine cublasCgerc

    subroutine cublasZgerc (m, n, alpha, x, incx, y, incy, A, lda) &
        bind (c, name="cublasZgerc")
      import
      integer(c_int), value :: m, n, incx, incy, lda
      complex(c_double), value :: alpha
      complex(c_double), dimension(lda, *) :: A
      complex(c_double), dimension(*) :: x, y
    end subroutine cublasZgerc

    subroutine cublasSsyr (uplo, n, alpha, x, incx, A, lda) &
        bind (c, name="cublasSsyr")
      import
      integer(c_int), value :: n, incx, lda
      real(c_float), dimension(lda, *) :: A
      real(c_float), value :: alpha
      character(c_char), value :: uplo
      real(c_float), dimension(*) :: x
    end subroutine cublasSsyr

    subroutine cublasDsyr (uplo, n, alpha, x, incx, A, lda) &
        bind (c, name="cublasDsyr")
      import
      integer(c_int), value :: n, incx, lda
      real(c_double), dimension(*) :: x
      real(c_double), value :: alpha
      character(c_char), value :: uplo
      real(c_double), dimension(lda, *) :: A
    end subroutine cublasDsyr

    subroutine cublasCher (uplo, n, alpha, x, incx, A, lda) &
        bind (c, name="cublasCher")
      import
      integer(c_int), value :: n, incx, lda
      complex(c_float), dimension(*) :: x
      character(c_char), value :: uplo
      complex(c_float), dimension(lda, *) :: A
      complex(c_float), value :: alpha
    end subroutine cublasCher

    subroutine cublasZher (uplo, n, alpha, x, incx, A, lda) &
        bind (c, name="cublasZher")
      import
      integer(c_int), value :: n, incx, lda
      complex(c_double), value :: alpha
      complex(c_double), dimension(*) :: x
      character(c_char), value :: uplo
      complex(c_double), dimension(lda, *) :: A
    end subroutine cublasZher

    subroutine cublasSspr (uplo, n, alpha, x, incx, AP) &
        bind (c, name="cublasSspr")
      import
      integer(c_int), value :: n, incx
      real(c_float), value :: alpha
      character(c_char), value :: uplo
      real(c_float), dimension(*) :: x, AP
    end subroutine cublasSspr

    subroutine cublasDspr (uplo, n, alpha, x, incx, AP) &
        bind (c, name="cublasDspr")
      import
      real(c_double), dimension(*) :: x, AP
      integer(c_int), value :: n, incx
      real(c_double), value :: alpha
      character(c_char), value :: uplo
    end subroutine cublasDspr

    subroutine cublasChpr (uplo, n, alpha, x, incx, AP) &
        bind (c, name="cublasChpr")
      import
      complex(c_float), dimension(*) :: x, AP
      integer(c_int), value :: n, incx
      character(c_char), value :: uplo
      complex(c_float), value :: alpha
    end subroutine cublasChpr

    subroutine cublasZhpr (uplo, n, alpha, x, incx, AP) &
        bind (c, name="cublasZhpr")
      import
      complex(c_double), value :: alpha
      integer(c_int), value :: n, incx
      complex(c_double), dimension(*) :: x, AP
      character(c_char), value :: uplo
    end subroutine cublasZhpr

    subroutine cublasSsyr2 (uplo, n, alpha, x, incx, y, incy, A, lda) &
        bind (c, name="cublasSsyr2")
      import
      integer(c_int), value :: n, incx, incy, lda
      real(c_float), dimension(lda, *) :: A
      real(c_float), value :: alpha
      character(c_char), value :: uplo
      real(c_float), dimension(*) :: x, y
    end subroutine cublasSsyr2

    subroutine cublasDsyr2 (uplo, n, alpha, x, incx, y, incy, A, lda) &
        bind (c, name="cublasDsyr2")
      import
      integer(c_int), value :: n, incx, incy, lda
      real(c_double), dimension(*) :: x, y
      real(c_double), value :: alpha
      character(c_char), value :: uplo
      real(c_double), dimension(lda, *) :: A
    end subroutine cublasDsyr2

    subroutine cublasCher2 (uplo, n, alpha, x, incx, y, incy, A, lda) &
        bind (c, name="cublasCher2")
      import
      integer(c_int), value :: n, incx, incy, lda
      complex(c_float), dimension(*) :: x, y
      character(c_char), value :: uplo
      complex(c_float), dimension(lda, *) :: A
      complex(c_float), value :: alpha
    end subroutine cublasCher2

    subroutine cublasZher2 (uplo, n, alpha, x, incx, y, incy, A, lda) &
        bind (c, name="cublasZher2")
      import
      integer(c_int), value :: n, incx, incy, lda
      complex(c_double), value :: alpha
      complex(c_double), dimension(*) :: x, y
      character(c_char), value :: uplo
      complex(c_double), dimension(lda, *) :: A
    end subroutine cublasZher2

    subroutine cublasSspr2 (uplo, n, alpha, x, incx, y, incy, AP) &
        bind (c, name="cublasSspr2")
      import
      integer(c_int), value :: n, incx, incy
      real(c_float), value :: alpha
      character(c_char), value :: uplo
      real(c_float), dimension(*) :: x, y, AP
    end subroutine cublasSspr2

    subroutine cublasDspr2 (uplo, n, alpha, x, incx, y, incy, AP) &
        bind (c, name="cublasDspr2")
      import
      real(c_double), dimension(*) :: x, y, AP
      integer(c_int), value :: n, incx, incy
      real(c_double), value :: alpha
      character(c_char), value :: uplo
    end subroutine cublasDspr2

    subroutine cublasChpr2 (uplo, n, alpha, x, incx, y, incy, AP) &
        bind (c, name="cublasChpr2")
      import
      complex(c_float), dimension(*) :: x, y, AP
      integer(c_int), value :: n, incx, incy
      character(c_char), value :: uplo
      complex(c_float), value :: alpha
    end subroutine cublasChpr2

    subroutine cublasZhpr2 (uplo, n, alpha, x, incx, y, incy, AP) &
        bind (c, name="cublasZhpr2")
      import
      complex(c_double), value :: alpha
      integer(c_int), value :: n, incx, incy
      complex(c_double), dimension(*) :: x, y, AP
      character(c_char), value :: uplo
    end subroutine cublasZhpr2

    subroutine cublasSgemm &
        (transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasSgemm")
      import
      integer(c_int), value :: m, n, k, lda, ldb, ldc
      real(c_float), dimension(lda, *) :: A
      character(c_char), value :: transa, transb
      real(c_float), dimension(ldc, *) :: C
      real(c_float), dimension(ldb, *) :: B
      real(c_float), value :: alpha, beta
    end subroutine cublasSgemm

    subroutine cublasDgemm &
        (transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasDgemm")
      import
      integer(c_int), value :: m, n, k, lda, ldb, ldc
      real(c_double), dimension(ldb, *) :: B
      real(c_double), dimension(ldc, *) :: C
      character(c_char), value :: transa, transb
      real(c_double), value :: alpha, beta
      real(c_double), dimension(lda, *) :: A
    end subroutine cublasDgemm

    subroutine cublasCgemm &
        (transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasCgemm")
      import
      integer(c_int), value :: m, n, k, lda, ldb, ldc
      complex(c_float), dimension(ldc, *) :: C
      complex(c_float), dimension(ldb, *) :: B
      character(c_char), value :: transa, transb
      complex(c_float), value :: alpha, beta
      complex(c_float), dimension(lda, *) :: A
    end subroutine cublasCgemm

    subroutine cublasZgemm &
        (transa, transb, m, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasZgemm")
      import
      integer(c_int), value :: m, n, k, lda, ldb, ldc
      complex(c_double), value :: alpha, beta
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: transa, transb
      complex(c_double), dimension(ldb, *) :: B
      complex(c_double), dimension(ldc, *) :: C
    end subroutine cublasZgemm

    subroutine cublasSsyrk (uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
        bind (c, name="cublasSsyrk")
      import
      integer(c_int), value :: n, k, lda, ldc
      real(c_float), dimension(ldc, *) :: C
      real(c_float), value :: alpha, beta
      character(c_char), value :: uplo, trans
      real(c_float), dimension(lda, *) :: A
    end subroutine cublasSsyrk

    subroutine cublasDsyrk (uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
        bind (c, name="cublasDsyrk")
      import
      integer(c_int), value :: n, k, lda, ldc
      real(c_double), dimension(lda, *) :: A
      real(c_double), dimension(ldc, *) :: C
      real(c_double), value :: alpha, beta
      character(c_char), value :: uplo, trans
    end subroutine cublasDsyrk

    subroutine cublasCsyrk (uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
        bind (c, name="cublasCsyrk")
      import
      integer(c_int), value :: n, k, lda, ldc
      complex(c_float), dimension(ldc, *) :: C
      character(c_char), value :: uplo, trans
      complex(c_float), dimension(lda, *) :: A
      complex(c_float), value :: alpha, beta
    end subroutine cublasCsyrk

    subroutine cublasZsyrk (uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
        bind (c, name="cublasZsyrk")
      import
      integer(c_int), value :: n, k, lda, ldc
      complex(c_double), value :: alpha, beta
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans
      complex(c_double), dimension(ldc, *) :: C
    end subroutine cublasZsyrk

    subroutine cublasCherk (uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
        bind (c, name="cublasCherk")
      import
      integer(c_int), value :: n, k, lda, ldc
      complex(c_float), dimension(ldc, *) :: C
      character(c_char), value :: uplo, trans
      complex(c_float), dimension(lda, *) :: A
      complex(c_float), value :: alpha, beta
    end subroutine cublasCherk

    subroutine cublasZherk (uplo, trans, n, k, alpha, A, lda, beta, C, ldc) &
        bind (c, name="cublasZherk")
      import
      integer(c_int), value :: n, k, lda, ldc
      complex(c_double), value :: alpha, beta
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans
      complex(c_double), dimension(ldc, *) :: C
    end subroutine cublasZherk

    subroutine cublasSsyr2k (uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasSsyr2k")
      import
      integer(c_int), value :: n, k, lda, ldb, ldc
      real(c_float), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans
      real(c_float), dimension(ldc, *) :: C
      real(c_float), dimension(ldb, *) :: B
      real(c_float), value :: alpha, beta
    end subroutine cublasSsyr2k

    subroutine cublasDsyr2k (uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasDsyr2k")
      import
      integer(c_int), value :: n, k, lda, ldb, ldc
      real(c_double), dimension(ldb, *) :: B
      real(c_double), dimension(ldc, *) :: C
      character(c_char), value :: uplo, trans
      real(c_double), value :: alpha, beta
      real(c_double), dimension(lda, *) :: A
    end subroutine cublasDsyr2k

    subroutine cublasCsyr2k (uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasCsyr2k")
      import
      integer(c_int), value :: n, k, lda, ldb, ldc
      complex(c_float), dimension(ldc, *) :: C
      complex(c_float), dimension(ldb, *) :: B
      character(c_char), value :: uplo, trans
      complex(c_float), value :: alpha, beta
      complex(c_float), dimension(lda, *) :: A
    end subroutine cublasCsyr2k

    subroutine cublasZsyr2k (uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasZsyr2k")
      import
      integer(c_int), value :: n, k, lda, ldb, ldc
      complex(c_double), value :: alpha, beta
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans
      complex(c_double), dimension(ldb, *) :: B
      complex(c_double), dimension(ldc, *) :: C
    end subroutine cublasZsyr2k

    subroutine cublasCher2k (uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasCher2k")
      import
      integer(c_int), value :: n, k, lda, ldb, ldc
      complex(c_float), dimension(ldc, *) :: C
      complex(c_float), dimension(ldb, *) :: B
      character(c_char), value :: uplo, trans
      complex(c_float), value :: alpha, beta
      complex(c_float), dimension(lda, *) :: A
    end subroutine cublasCher2k

    subroutine cublasZher2k (uplo, trans, n, k, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasZher2k")
      import
      integer(c_int), value :: n, k, lda, ldb, ldc
      complex(c_double), value :: alpha, beta
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: uplo, trans
      complex(c_double), dimension(ldb, *) :: B
      complex(c_double), dimension(ldc, *) :: C
    end subroutine cublasZher2k

    subroutine cublasSsymm (side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasSsymm")
      import
      integer(c_int), value :: m, n, lda, ldb, ldc
      real(c_float), dimension(lda, *) :: A
      character(c_char), value :: side, uplo
      real(c_float), dimension(ldc, *) :: C
      real(c_float), dimension(ldb, *) :: B
      real(c_float), value :: alpha, beta
    end subroutine cublasSsymm

    subroutine cublasDsymm (side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasDsymm")
      import
      integer(c_int), value :: m, n, lda, ldb, ldc
      real(c_double), dimension(ldb, *) :: B
      real(c_double), dimension(ldc, *) :: C
      character(c_char), value :: side, uplo
      real(c_double), value :: alpha, beta
      real(c_double), dimension(lda, *) :: A
    end subroutine cublasDsymm

    subroutine cublasCsymm (side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasCsymm")
      import
      integer(c_int), value :: m, n, lda, ldb, ldc
      complex(c_float), dimension(ldc, *) :: C
      complex(c_float), dimension(ldb, *) :: B
      character(c_char), value :: side, uplo
      complex(c_float), value :: alpha, beta
      complex(c_float), dimension(lda, *) :: A
    end subroutine cublasCsymm

    subroutine cublasZsymm (side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasZsymm")
      import
      integer(c_int), value :: m, n, lda, ldb, ldc
      complex(c_double), value :: alpha, beta
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: side, uplo
      complex(c_double), dimension(ldb, *) :: B
      complex(c_double), dimension(ldc, *) :: C
    end subroutine cublasZsymm

    subroutine cublasChemm (side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasChemm")
      import
      integer(c_int), value :: m, n, lda, ldb, ldc
      complex(c_float), dimension(ldc, *) :: C
      complex(c_float), dimension(ldb, *) :: B
      character(c_char), value :: side, uplo
      complex(c_float), value :: alpha, beta
      complex(c_float), dimension(lda, *) :: A
    end subroutine cublasChemm

    subroutine cublasZhemm (side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc) &
        bind (c, name="cublasZhemm")
      import
      integer(c_int), value :: m, n, lda, ldb, ldc
      complex(c_double), value :: alpha, beta
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: side, uplo
      complex(c_double), dimension(ldb, *) :: B
      complex(c_double), dimension(ldc, *) :: C
    end subroutine cublasZhemm

    subroutine cublasStrsm (side, uplo, transa, diag, m, n, alpha, A, lda, B, ldb) &
        bind (c, name="cublasStrsm")
      import
      integer(c_int), value :: m, n, lda, ldb
      real(c_float), dimension(ldb, *) :: B
      real(c_float), value :: alpha
      character(c_char), value :: side, uplo, transa, diag
      real(c_float), dimension(lda, *) :: A
    end subroutine cublasStrsm

    subroutine cublasDtrsm (side, uplo, transa, diag, m, n, alpha, A, lda, B, ldb) &
        bind (c, name="cublasDtrsm")
      import
      integer(c_int), value :: m, n, lda, ldb
      real(c_double), dimension(lda, *) :: A
      real(c_double), value :: alpha
      character(c_char), value :: side, uplo, transa, diag
      real(c_double), dimension(ldb, *) :: B
    end subroutine cublasDtrsm

    subroutine cublasCtrsm (side, uplo, transa, diag, m, n, alpha, A, lda, B, ldb) &
        bind (c, name="cublasCtrsm")
      import
      integer(c_int), value :: m, n, lda, ldb
      complex(c_float), dimension(ldb, *) :: B
      character(c_char), value :: side, uplo, transa, diag
      complex(c_float), dimension(lda, *) :: A
      complex(c_float), value :: alpha
    end subroutine cublasCtrsm

    subroutine cublasZtrsm (side, uplo, transa, diag, m, n, alpha, A, lda, B, ldb) &
        bind (c, name="cublasZtrsm")
      import
      integer(c_int), value :: m, n, lda, ldb
      complex(c_double), value :: alpha
      complex(c_double), dimension(ldb, *) :: B
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: side, uplo, transa, diag
    end subroutine cublasZtrsm

    subroutine cublasStrmm (side, uplo, transa, diag, m, n, alpha, A, lda, B, ldb) &
        bind (c, name="cublasStrmm")
      import
      integer(c_int), value :: m, n, lda, ldb
      real(c_float), dimension(ldb, *) :: B
      real(c_float), value :: alpha
      character(c_char), value :: side, uplo, transa, diag
      real(c_float), dimension(lda, *) :: A
    end subroutine cublasStrmm

    subroutine cublasDtrmm (side, uplo, transa, diag, m, n, alpha, A, lda, B, ldb) &
        bind (c, name="cublasDtrmm")
      import
      integer(c_int), value :: m, n, lda, ldb
      real(c_double), dimension(lda, *) :: A
      real(c_double), value :: alpha
      character(c_char), value :: side, uplo, transa, diag
      real(c_double), dimension(ldb, *) :: B
    end subroutine cublasDtrmm

    subroutine cublasCtrmm (side, uplo, transa, diag, m, n, alpha, A, lda, B, ldb) &
        bind (c, name="cublasCtrmm")
      import
      integer(c_int), value :: m, n, lda, ldb
      complex(c_float), dimension(ldb, *) :: B
      character(c_char), value :: side, uplo, transa, diag
      complex(c_float), dimension(lda, *) :: A
      complex(c_float), value :: alpha
    end subroutine cublasCtrmm

    subroutine cublasZtrmm (side, uplo, transa, diag, m, n, alpha, A, lda, B, ldb) &
        bind (c, name="cublasZtrmm")
      import
      integer(c_int), value :: m, n, lda, ldb
      complex(c_double), value :: alpha
      complex(c_double), dimension(ldb, *) :: B
      complex(c_double), dimension(lda, *) :: A
      character(c_char), value :: side, uplo, transa, diag
    end subroutine cublasZtrmm

  end interface
end module
