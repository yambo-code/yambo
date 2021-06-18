!!  Fortran CUDA Library interface -- Module for Core CUBLAS definitions
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

module cublas_core_m
  use iso_c_binding

  type, bind(c) :: cuBlasHandle
     type(c_ptr) :: handle
  end type cuBlasHandle

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

  enum, bind(c)
     enumerator :: CUBLAS_FILL_MODE_LOWER = 0
     enumerator :: CUBLAS_FILL_MODE_UPPER = 1
  end enum

  enum, bind(c)
     enumerator :: CUBLAS_DIAG_NON_UNIT = 0
     enumerator :: CUBLAS_DIAG_UNIT     = 1
  end enum

  enum, bind(c)
     enumerator :: CUBLAS_SIDE_LEFT  = 0
     enumerator :: CUBLAS_SIDE_RIGHT = 1
  end enum

  enum, bind(c)
     enumerator :: CUBLAS_OP_N = 0
     enumerator :: CUBLAS_OP_T = 1
     enumerator :: CUBLAS_OP_C = 2
  end enum

  enum, bind(c)
     enumerator :: CUBLAS_POINTER_MODE_HOST   = 0
     enumerator :: CUBLAS_POINTER_MODE_DEVICE = 1
  end enum

  interface
     integer(c_int) function cublasCreate (handle) &
         bind (c, name="cublasCreate_v2")
       import
       type(cublasHandle) :: handle
     end function cublasCreate

     integer(c_int) function cublasDestroy (handle) &
         bind (c, name="cublasDestroy_v2")
       import
       type(cublasHandle), value :: handle
     end function cublasDestroy

     integer(c_int) function cublasGetVersion (handle, version) &
         bind (c, name="cublasGetVersion_v2")
       import
       type(cublasHandle), value :: handle
       integer(c_int) :: version
     end function cublasGetVersion

     integer(c_int) function cublasSetStream (handle, streamId) &
         bind (c, name="cublasSetStream_v2")
       import
       type(c_ptr), value :: streamId
       type(cublasHandle), value :: handle
     end function cublasSetStream

     integer(c_int) function cublasGetStream (handle, streamId) &
         bind (c, name="cublasGetStream_v2")
       import
       type(cublasHandle), value :: handle
       type(c_ptr) :: streamId
     end function cublasGetStream

     integer(c_int) function cublasGetPointerMode (handle, mode) &
         bind (c, name="cublasGetPointerMode_v2")
       import
       type(cublasHandle), value :: handle
       integer(c_int) :: mode
     end function cublasGetPointerMode

     integer(c_int) function cublasSetPointerMode (handle, mode) &
         bind (c, name="cublasSetPointerMode_v2")
       import
       integer(c_int), value :: mode
       type(cublasHandle), value :: handle
     end function cublasSetPointerMode

     integer(c_int) function cublasGetAtomicsMode (handle, mode) &
         bind (c, name="cublasGetAtomicsMode_v2")
       import
       type(cublasHandle), value :: handle
       integer(c_int) :: mode
     end function cublasGetAtomicsMode

     integer(c_int) function cublasSetAtomicsMode (handle, mode) &
         bind (c, name="cublasSetAtomicsMode_v2")
       import
       integer(c_int), value :: mode
       type(cublasHandle), value :: handle
     end function cublasSetAtomicsMode

     integer(c_int) function cublasSetVector (n, elemSize, x, incx, y, incy) &
         bind (c, name="cublasSetVector_v2")
       import
       integer(c_int), value :: n, elemSize, incx, incy
       integer(c_signed_char), dimension(*) :: x, y
     end function cublasSetVector

     integer(c_int) function cublasGetVector (n, elemSize, x, incx, y, incy) &
         bind (c, name="cublasGetVector_v2")
       import
       integer(c_int), value :: n, elemSize, incx, incy
       integer(c_signed_char), dimension(*) :: x, y
     end function cublasGetVector

     integer(c_int) function cublasSetMatrix (rows, cols, elemSize, A, lda, B, ldb) &
         bind (c, name="cublasSetMatrix_v2")
       import
       integer(c_int), value :: rows, cols, elemSize, lda, ldb
       integer(c_signed_char), dimension(lda, *) :: A
       integer(c_signed_char), dimension(ldb, *) :: B
     end function cublasSetMatrix

     integer(c_int) function cublasGetMatrix (rows, cols, elemSize, A, lda, B, ldb) &
         bind (c, name="cublasGetMatrix_v2")
       import
       integer(c_int), value :: rows, cols, elemSize, lda, ldb
       integer(c_signed_char), dimension(lda, *) :: A
       integer(c_signed_char), dimension(ldb, *) :: B
     end function cublasGetMatrix

     integer(c_int) function cublasSetVectorAsync (n, elemSize, hostPtr, incx, devicePtr, incy, stream) &
         bind (c, name="cublasSetVectorAsync_v2")
       import
       integer(c_int), value :: n, elemSize, incx, incy
       type(c_ptr), value :: stream
       integer(c_signed_char), dimension(*) :: hostPtr, devicePtr
     end function cublasSetVectorAsync

     integer(c_int) function cublasGetVectorAsync (n, elemSize, devicePtr, incx, hostPtr, incy, stream) &
         bind (c, name="cublasGetVectorAsync_v2")
       import
       integer(c_int), value :: n, elemSize, incx, incy
       type(c_ptr), value :: stream
       integer(c_signed_char), dimension(*) :: devicePtr, hostPtr
     end function cublasGetVectorAsync

     integer(c_int) function cublasSetMatrixAsync (rows, cols, elemSize, A, lda, B, ldb, stream) &
         bind (c, name="cublasSetMatrixAsync_v2")
       import
       integer(c_int), value :: rows, cols, elemSize, lda, ldb
       integer(c_signed_char), dimension(lda, *) :: A
       integer(c_signed_char), dimension(ldb, *) :: B
       type(c_ptr), value :: stream
     end function cublasSetMatrixAsync

     integer(c_int) function cublasGetMatrixAsync (rows, cols, elemSize, A, lda, B, ldb, stream) &
         bind (c, name="cublasGetMatrixAsync_v2")
       import
       integer(c_int), value :: rows, cols, elemSize, lda, ldb
       integer(c_signed_char), dimension(lda, *) :: A
       integer(c_signed_char), dimension(ldb, *) :: B
       type(c_ptr), value :: stream
     end function cublasGetMatrixAsync

  end interface
end module
