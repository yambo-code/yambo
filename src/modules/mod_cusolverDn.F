!
! Copyright (c) 2016, NVIDIA CORPORATION. All rights reserved.
! 
! 
! Permission is hereby granted, free of charge, to any person obtaining a
! copy of this software and associated documentation files (the "Software"),
! to deal in the Software without restriction, including without limitation
! the rights to use, copy, modify, merge, publish, distribute, sublicense,
! and/or sell copies of the Software, and to permit persons to whom the
! Software is furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
! FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
! DEALINGS IN THE SOFTWARE.
!

module cusolverDn_m
  use iso_c_binding

#ifdef _CUDA
  ! enums

  enum, bind(C) ! cusolverStatus_t
     enumerator :: CUSOLVER_STATUS_SUCCESS=0
     enumerator :: CUSOLVER_STATUS_NOT_INITIALIZED=1
     enumerator :: CUSOLVER_STATUS_ALLOC_FAILED=2
     enumerator :: CUSOLVER_STATUS_INVALID_VALUE=3
     enumerator :: CUSOLVER_STATUS_ARCH_MISMATCH=4
     enumerator :: CUSOLVER_STATUS_MAPPING_ERROR=5
     enumerator :: CUSOLVER_STATUS_EXECUTION_FAILED=6
     enumerator :: CUSOLVER_STATUS_INTERNAL_ERROR=7
     enumerator :: CUSOLVER_STATUS_MATRIX_TYPE_NOT_SUPPORTED=8
     enumerator :: CUSOLVER_STATUS_NOT_SUPPORTED = 9
     enumerator :: CUSOLVER_STATUS_ZERO_PIVOT=10
     enumerator :: CUSOLVER_STATUS_INVALID_LICENSE=11
  end enum

  enum, bind(c) ! cusolverEigType_t
     enumerator :: CUSOLVER_EIG_TYPE_1=1
     enumerator :: CUSOLVER_EIG_TYPE_2=2
     enumerator :: CUSOLVER_EIG_TYPE_3=3
  end enum

  enum, bind(c) ! cusolverEigMode_t
     enumerator :: CUSOLVER_EIG_MODE_NOVECTOR=0
     enumerator :: CUSOLVER_EIG_MODE_VECTOR=1
  end enum

  ! types

  type cusolverDnHandle
     type(c_ptr) :: handle
  end type cusolverDnHandle

  ! ---------
  ! functions
  ! ---------

  interface
     integer(c_int) function cusolverDnCreate(handle) &
          bind(C,name='cusolverDnCreate')
       import cusolverDnHandle
       type(cusolverDnHandle) :: handle
     end function cusolverDnCreate
  end interface

  interface
     integer(c_int) function cusolverDnDestroy(handle) &
          bind(C,name='cusolverDnDestroy')
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle
     end function cusolverDnDestroy
  end interface

  interface
     integer(c_int) function cusolverDnSetStream(handle, stream) &
          bind(C,name='cusolverDnSetStream')
       use cudafor
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle
       integer(cuda_stream_kind), value :: stream
     end function cusolverDnSetStream
  end interface

  interface
     integer(c_int) function cusolverDnZpotrf_bufferSize( &
          handle, uplo, n, A, lda, Lwork) &
          bind(C, name='cusolverDnZpotrf_bufferSize') 
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle 
       integer(c_int), value :: uplo 
       integer(c_int), value :: n 
       complex(8), device :: A(*) 
       integer(c_int), value :: lda 
       integer(c_int) :: Lwork
     end function cusolverDnZpotrf_bufferSize
  end interface

  interface
     integer(c_int) function cusolverDnDpotrf_bufferSize( &
          handle, uplo, n, A, lda, Lwork) &
          bind(C, name='cusolverDnDpotrf_bufferSize') 
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle 
       integer(c_int), value :: uplo 
       integer(c_int), value :: n 
       real(8), device :: A(*) 
       integer(c_int), value :: lda 
       integer(c_int) :: Lwork
     end function cusolverDnDpotrf_bufferSize
  end interface

  interface
     integer(c_int) function cusolverDnZpotrf( &
          handle, uplo, n, A, lda, Workspace, Lwork, devInfo) &
          bind(C,name='cusolverDnZpotrf') 
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle 
       integer(c_int), value :: uplo 
       integer(c_int), value :: n 
       complex(8), device :: A(*) 
       integer(c_int), value :: lda 
       !pgi$ ignore_tkr (r) Workspace
       complex(8), device :: Workspace(*)
       integer(c_int), value :: Lwork
       integer(c_int), device :: devInfo
     end function cusolverDnZpotrf
  end interface

  interface
     integer(c_int) function cusolverDnDpotrf( &
          handle, uplo, n, A, lda, Workspace, Lwork, devInfo) &
          bind(C,name='cusolverDnDpotrf') 
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle 
       integer(c_int), value :: uplo 
       integer(c_int), value :: n 
       real(8), device :: A(*) 
       integer(c_int), value :: lda 
       !pgi$ ignore_tkr (r) Workspace
       real(8), device :: Workspace(*)
       integer(c_int), value :: Lwork
       integer(c_int), device :: devInfo
     end function cusolverDnDpotrf
  end interface

  interface
     integer(c_int) function cusolverDnZhegvd_bufferSize( &
          handle, itype, jobz, uplo, n, A, lda, B, ldb, W, Lwork) &
          bind(C, name='cusolverDnZhegvd_bufferSize')
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle
       integer(c_int), value :: itype
       integer(c_int), value :: jobz
       integer(c_int), value :: uplo
       integer(c_int), value :: n
       complex(8), device :: A(*)
       integer(c_int), value :: lda
       complex(8), device :: B(*)
       integer(c_int), value :: ldb
       real(8), device :: W(*)
       integer(c_int) :: Lwork
     end function cusolverDnZhegvd_bufferSize
  end interface

  interface
     integer(c_int) function cusolverDnDsygvd_bufferSize( &
          handle, itype, jobz, uplo, n, A, lda, B, ldb, W, Lwork) &
          bind(C, name='cusolverDnDsygvd_bufferSize')
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle
       integer(c_int), value :: itype
       integer(c_int), value :: jobz
       integer(c_int), value :: uplo
       integer(c_int), value :: n
       real(8), device :: A(*)
       integer(c_int), value :: lda
       real(8), device :: B(*)
       integer(c_int), value :: ldb
       real(8), device :: W(*)
       integer(c_int) :: Lwork
     end function cusolverDnDsygvd_bufferSize
  end interface

  interface
     integer(c_int) function cusolverDnZhegvd( &
          handle, itype, jobz, uplo, n, A, lda, B, ldb, W, Workspace, Lwork, devInfo) &
          bind(C,name='cusolverDnZhegvd')
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle
       integer(c_int), value :: itype
       integer(c_int), value :: jobz
       integer(c_int), value :: uplo
       integer(c_int), value :: n
       complex(8), device :: A(*)
       integer(c_int), value :: lda
       complex(8), device :: B(*)
       integer(c_int), value :: ldb
       real(8), device :: W(*)
       !pgi$ ignore_tkr (r) Workspace
       complex(8), device :: Workspace(*)
       integer(c_int), value :: Lwork
       integer(c_int), device :: devInfo
     end function cusolverDnZhegvd
  end interface

  interface
     integer(c_int) function cusolverDnDsygvd( &
          handle, itype, jobz, uplo, n, A, lda, B, ldb, W, Workspace, Lwork, devInfo) &
          bind(C,name='cusolverDnDsygvd')
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle
       integer(c_int), value :: itype
       integer(c_int), value :: jobz
       integer(c_int), value :: uplo
       integer(c_int), value :: n
       real(8), device :: A(*)
       integer(c_int), value :: lda
       real(8), device :: B(*)
       integer(c_int), value :: ldb
       real(8), device :: W(*)
       !pgi$ ignore_tkr (r) Workspace
       real(8), device :: Workspace(*)
       integer(c_int), value :: Lwork
       integer(c_int), device :: devInfo
     end function cusolverDnDsygvd
  end interface

  interface
     integer(c_int) function cusolverDnZheevd_bufferSize( &
          handle, jobz, uplo, n, A, lda, W, Lwork) &
          bind(C, name='cusolverDnZheevd_bufferSize')
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle
       integer(c_int), value :: jobz
       integer(c_int), value :: uplo
       integer(c_int), value :: n
       complex(8), device :: A(*)
       integer(c_int), value :: lda
       real(8), device :: W(*)
       integer(c_int) :: Lwork
     end function cusolverDnZheevd_bufferSize
  end interface

  interface
     integer(c_int) function cusolverDnZheevd( &
          handle, jobz, uplo, n, A, lda, W, Workspace, Lwork, devInfo) &
          bind(C,name='cusolverDnZheevd')
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle
       integer(c_int), value :: jobz
       integer(c_int), value :: uplo
       integer(c_int), value :: n
       complex(8), device :: A(*)
       integer(c_int), value :: lda
       real(8), device :: W(*)
       !pgi$ ignore_tkr (r) Workspace
       complex(8), device :: Workspace(*)
       integer(c_int), value :: Lwork
       integer(c_int), device :: devInfo
     end function cusolverDnZheevd
  end interface

  interface
     integer(c_int) function cusolverDnCgetrf_bufferSize( &
          handle, m, n, A, lda, Lwork) &
          bind(C, name='cusolverDnCgetrf_bufferSize') 
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle 
       integer(c_int), value :: m
       integer(c_int), value :: n
       complex(4), device :: A(lda,*) 
       integer(c_int), value :: lda 
       integer(c_int) :: Lwork
     end function cusolverDnCgetrf_bufferSize
  end interface

  interface
     integer(c_int) function cusolverDnCgetrf( &
          handle, m, n, A, lda, Workspace, devIpiv, devInfo) &
          bind(C,name='cusolverDnCgetrf')
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle
       integer(c_int), value :: m
       integer(c_int), value :: n
       complex(4), device :: A(lda,*)
       integer(c_int), value :: lda
       !pgi$ ignore_tkr (r) Workspace
       complex(4), device :: Workspace(*)
       integer(c_int), device :: devIpiv(*)
       integer(c_int), device :: devInfo(*)
     end function cusolverDnCgetrf
  end interface

  interface
     integer(c_int) function cusolverDnCgetrs( &
          handle, trans, n, nrhs, A, lda, devIpiv, B, ldb, devInfo) &
          bind(C,name='cusolverDnCgetrs')
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle
       integer(c_int), value :: trans
       integer(c_int), value :: n
       integer(c_int), value :: nrhs
       complex(4), device :: A(lda,*)
       integer(c_int), value :: lda
       integer(c_int), device :: devIpiv(*)
       complex(4), device :: B(ldb,*)
       integer(c_int), value :: ldb
       integer(c_int), device :: devInfo(*)
     end function cusolverDnCgetrs
  end interface

  interface
     integer(c_int) function cusolverDnZgetrf_bufferSize( &
          handle, m, n, A, lda, Lwork) &
          bind(C, name='cusolverDnZgetrf_bufferSize') 
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle 
       integer(c_int), value :: m
       integer(c_int), value :: n
       complex(8), device :: A(lda,*) 
       integer(c_int), value :: lda 
       integer(c_int) :: Lwork
     end function cusolverDnZgetrf_bufferSize
  end interface

  interface
     integer(c_int) function cusolverDnZgetrf( &
          handle, m, n, A, lda, Workspace, devIpiv, devInfo) &
          bind(C,name='cusolverDnZgetrf')
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle
       integer(c_int), value :: m
       integer(c_int), value :: n
       complex(8), device :: A(lda,*)
       integer(c_int), value :: lda
       !pgi$ ignore_tkr (r) Workspace
       complex(8), device :: Workspace(*)
       integer(c_int), device :: devIpiv(*)
       integer(c_int), device :: devInfo(*)
     end function cusolverDnZgetrf
  end interface

  interface
     integer(c_int) function cusolverDnZgetrs( &
          handle, trans, n, nrhs, A, lda, devIpiv, B, ldb, devInfo) &
          bind(C,name='cusolverDnZgetrs')
       use iso_c_binding
       import cusolverDnHandle
       type(cusolverDnHandle), value :: handle
       integer(c_int), value :: trans
       integer(c_int), value :: n
       integer(c_int), value :: nrhs
       complex(8), device :: A(lda,*)
       integer(c_int), value :: lda
       integer(c_int), device :: devIpiv(*)
       complex(8), device :: B(ldb,*)
       integer(c_int), value :: ldb
       integer(c_int), device :: devInfo(*)
     end function cusolverDnZgetrs
  end interface

#endif

end module cusolverDn_m
  

