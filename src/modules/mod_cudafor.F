!*************************************************************************
!* Copyright (c) 2016 Research Computing Services (RCS), University of
!* Cambridge. All rights reserved.
!*
!* See LICENSE.txt for license information
!*************************************************************************
!
! LICENSE.txt
!
! From PyTorch:
!
! Copyright (c) 2016-     Facebook, Inc            (Adam Paszke)
! Copyright (c) 2014-     Facebook, Inc            (Soumith Chintala)
! Copyright (c) 2011-2014 Idiap Research Institute (Ronan Collobert)
! Copyright (c) 2012-2014 Deepmind Technologies    (Koray Kavukcuoglu)
! Copyright (c) 2011-2012 NEC Laboratories America (Koray Kavukcuoglu)
! Copyright (c) 2011-2013 NYU                      (Clement Farabet)
! Copyright (c) 2006-2010 NEC Laboratories America (Ronan Collobert, Leon
! CBottou, Iain Melvin, Jason Weston)
! Copyright (c) 2006      Idiap Research Institute (Samy Bengio)
! Copyright (c) 2001-2004 Idiap Research Institute (Ronan Collobert, Samy
! CBengio, Johnny Mariethoz)
!
! From Caffe2:
!
! Copyright (c) 2016-present, Facebook Inc. All rights reserved.
!
! All contributions by Facebook:
! Copyright (c) 2016 Facebook Inc.
! 
! All contributions by Google:
! Copyright (c) 2015 Google Inc.
! All rights reserved.
!  
! All contributions by Yangqing Jia:
! Copyright (c) 2015 Yangqing Jia
! All rights reserved.
! 
! All contributions from Caffe:
! Copyright(c) 2013, 2014, 2015, the respective contributors
! All rights reserved.
!  
! All other contributions:
! Copyright(c) 2015, 2016 the respective contributors
! All rights reserved.
!  
! Caffe2 uses a copyright model similar to Caffe: each contributor holds
! copyright over their contributions to Caffe2. The project versioning records
! all such contribution and copyright details. If a contributor wants to further
! mark their specific copyright on a particular contribution, they should
! indicate their copyright solely in the commit message of the change when it is
! committed.
!
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are
! met:
! 
! 1. Redistributions of source code must retain the above copyright
!    notice, this list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright
!    notice, this list of conditions and the following disclaimer in the
!    documentation and/or other materials provided with the distribution.
!
! 3. Neither the names of Facebook, Deepmind Technologies, NYU, NEC
!    Laboratories America and IDIAP Research Institute nor the names of 
!    its contributors may be used to endorse or promote products derived from 
!    this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
! THE POSSIBILITY OF SUCH DAMAGE.
!
!*************************************************************************

!Start cudafor module
module cudafor_m
use iso_c_binding
implicit none
private

#if defined _GPU && ! defined _CUDAF

public :: c_devptr
public :: cudaMemcpyKind,           &
          cudaMemcpyHostToHost,     &
          cudaMemcpyHostToDevice,   &
          cudaMemcpyDeviceToHost,   &
          cudaMemcpyDeviceToDevice, &
          cudaMemcpyDefault
public :: cuda_stream_kind
public :: cudaGetDeviceCount
public :: cudaSetDevice
public :: cudaMalloc
public :: cudaMemcpy
public :: cudaFree
public :: cudaStreamCreate
public :: cudaStreamSynchronize
public :: cudaStreamDestroy
! Added by Andrea Ferretti, Jun 2021
public :: cudaDeviceSynchronize
public :: cudaGetLastError

!Start types

!Start c_devptr
type, bind(c) :: c_devptr
type(c_ptr) :: member
end type c_devptr
!End c_devptr

!Start cudaMemcpyKind
type, bind(c) :: cudaMemcpyKind
integer(c_int) :: member
end type cudaMemcpyKind

type(cudaMemcpyKind), parameter :: cudaMemcpyHostToHost     = cudaMemcpyKind(0), &
                                   cudaMemcpyHostToDevice   = cudaMemcpyKind(1), &
                                   cudaMemcpyDeviceToHost   = cudaMemcpyKind(2), &
                                   cudaMemcpyDeviceToDevice = cudaMemcpyKind(3), &
                                   cudaMemcpyDefault        = cudaMemcpyKind(4)
!End cudaMemcpyKind

!Start cuda_stream_kind
integer(c_intptr_t), parameter :: cuda_stream_kind = c_intptr_t
!End cuda_stream_kind

!End types

!Start interfaces

!Start cudaGetDeviceCount
interface cudaGetDeviceCount
integer(c_int) function cudaGetDeviceCount(count) bind(c, name = "cudaGetDeviceCount")
import :: c_int
implicit none
integer(c_int) :: count
end function cudaGetDeviceCount
end interface cudaGetDeviceCount
!End cudaGetDeviceCount

!Start cudaSetDevice
interface cudaSetDevice
integer(c_int) function cudaSetDevice(device) bind(c, name = "cudaSetDevice")
import :: c_int
implicit none
integer(c_int), value :: device
end function cudaSetDevice
end interface cudaSetDevice
!End cudaSetDevice

!Start cudaMalloc
interface cudaMalloc
integer(c_int) function cudaMalloc(devPtr, size) bind(c, name = "cudaMalloc")
import :: c_int, c_size_t
import :: c_devptr
implicit none
type(c_devptr) :: devPtr
integer(c_size_t), value :: size
end function cudaMalloc
end interface cudaMalloc
!End cudaMalloc

!Start cudaMemcpy
interface cudaMemcpy

!Start cudaMemcpyH2D
integer(c_int) function cudaMemcpyH2D(dst, src, count, kind) bind(c, name = "cudaMemcpy")
import :: c_ptr, c_int, c_size_t
import :: c_devptr, cudaMemcpyKind
implicit none
type(c_devptr), value :: dst
type(c_ptr), value :: src
integer(c_size_t), value :: count
type(cudaMemcpyKind), value :: kind
end function cudaMemcpyH2D
!End cudaMemcpyH2D

!Start cudaMemcpyD2H
integer(c_int) function cudaMemcpyD2H(dst, src, count, kind) bind(c, name = "cudaMemcpy")
import :: c_ptr, c_int, c_size_t
import :: c_devptr, cudaMemcpyKind
implicit none
type(c_ptr), value :: dst
type(c_devptr), value :: src
integer(c_size_t), value :: count
type(cudaMemcpyKind), value :: kind
end function cudaMemcpyD2H
!End cudaMemcpyD2H

end interface cudaMemcpy
!End cudaMemcpy

!Start cudaFree
interface cudaFree
integer(c_int) function cudaFree(devPtr) bind(c, name = "cudaFree")
import :: c_int
import :: c_devptr
implicit none
type(c_devptr), value :: devPtr
end function cudaFree
end interface cudaFree
!End cudaFree

!Start cudaStreamCreate
interface cudaStreamCreate
integer(c_int) function cudaStreamCreate(pStream) bind(c, name = "cudaStreamCreate")
import :: c_int
import :: cuda_stream_kind
implicit none
integer(cuda_stream_kind) :: pStream
end function cudaStreamCreate
end interface cudaStreamCreate
!End cudaStreamCreate

!Start cudaStreamSynchronize
interface cudaStreamSynchronize
integer(c_int) function cudaStreamSynchronize(stream) bind(c, name = "cudaStreamSynchronize")
import :: c_int
import :: cuda_stream_kind
implicit none
integer(cuda_stream_kind), value :: stream
end function cudaStreamSynchronize
end interface cudaStreamSynchronize
!End cudaStreamSynchronize

!Start cudaStreamDestroy
interface cudaStreamDestroy
integer(c_int) function cudaStreamDestroy(stream) bind(c, name = "cudaStreamDestroy")
import :: c_int
import :: cuda_stream_kind
implicit none
integer(cuda_stream_kind), value :: stream
end function cudaStreamDestroy
end interface cudaStreamDestroy
!End cudaStreamDestroy

!
! THe following have been added by Andrea Ferretti on Jun 2021
!
!Start cudaDeviceSynchronize
interface cudaDeviceSynchronize
integer(c_int) function cudaDeviceSynchronize() bind(c, name = "cudaDeviceSynchronize")
import :: c_int
implicit none
end function cudaDeviceSynchronize
end interface cudaDeviceSynchronize
!End cudaDeviceSynchronize

!Start cudaGetLastError
interface cudaGetLastError
integer(c_int) function cudaGetLastError() bind(c, name = "cudaGetLastError")
import :: c_int
implicit none
end function cudaGetLastError
end interface cudaGetLastError
!End cudaGetLastError


!End interfaces

#endif

end module cudafor_m
!End cudaFor module

