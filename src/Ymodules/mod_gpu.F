!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MF, AF
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
!================
module gpu_m
  !================
  !
  use devxlib_environment
  use devxlib_cublas
  use devxlib_memcpy
  !use cusolverdn
  use cusolverdn_y
# if defined _CUDAF
  use cufft
# elif defined _OPENACC
  use cufft_m
# elif defined _OPENMP_GPU && defined _HIP
  use hip_m
  use hipfft_m
# endif
  use parallel_m,   ONLY:myid,host_name,PAR_COM_HOST
  use iso_c_binding
  !
  implicit none
  public
  !
  integer :: GPU_test_dim = 1000000

#ifdef _GPU
  logical, parameter:: have_gpu=.true.
#else
  logical, parameter:: have_gpu=.false.
#endif
#ifdef _CUDAF
  logical, parameter:: have_cuf=.true.
#else
  logical, parameter:: have_cuf=.false.
#endif
#ifdef _OPENACC
  logical, parameter:: have_openacc=.true.
#else
  logical, parameter:: have_openacc=.false.
#endif
#ifdef _OPENMP_GPU
  logical, parameter:: have_ompgpu=.true.
#else
  logical, parameter:: have_ompgpu=.false.
#endif
  character(16)  :: gpu_str=""
  !
  logical:: have_gpu_devices=.false.
  character(256) :: gpu_visible_devices=" "
  integer:: gpu_subscription

  logical:: gpu_linalg_init=.false.
#ifdef _GPU
  type(cusolverDnHandle)   :: cusolv_h
  public :: cusolv_h
#endif
  
  !
  ! streams
  !
#ifdef _CUDAF
  integer, parameter :: stream_kind=Cuda_Stream_Kind
#else
  integer, parameter :: stream_kind=c_intptr_t
#endif
  integer(stream_kind) :: stream_default
  integer(stream_kind) :: stream_h2d
  integer(stream_kind) :: stream_d2h
  integer(stream_kind) :: stream_d

  !
  ! async
  !
#ifdef _CUDAF
  integer, parameter :: async_kind=Cuda_Stream_Kind
#else
  integer, parameter :: async_kind=c_int
#endif
  integer(async_kind) :: async_default
  integer(async_kind) :: async_h2d
  integer(async_kind) :: async_d2h
  integer(async_kind) :: async_d

  !
  ! public
  !
  public :: have_gpu
  public :: have_cuf
  public :: have_openacc
  public :: have_ompgpu
  public :: have_gpu_devices
  public :: gpu_visible_devices
  public :: gpu_str
  public :: gpu_setup
  public :: gpu_linalg_init
  public :: gpu_linalg_setup
  public :: gpu_getstat
  public :: gpu_devsync
  !
  public :: gpu_subscription
  !
  public :: stream_kind
  public :: stream_default
  public :: stream_h2d
  public :: stream_d2h
  public :: stream_d
  !
  public :: async_kind
  public :: async_default
  public :: async_h2d
  public :: async_d2h
  public :: async_d
  !
#if defined _GPU && defined _CUDA
  public :: cudaGetDevice
#endif

contains

  subroutine gpu_setup()
    implicit none
    integer :: ndev,myid_host_loc
    integer :: ierr
    !
    gpu_subscription=1
    gpu_str=""
    if (have_cuf)     gpu_str="CUDA-Fortran"
    if (have_openacc) gpu_str="OpenACC"
    if (have_ompgpu)  gpu_str="OpenMP-GPU"
    !
    ! GPU assignment
    ! This needs to be done first
    !
#if defined _GPU && defined _MPI
    !
    myid_host_loc=PAR_COM_HOST%CPU_id
    !
    ierr = cudaGetDeviceCount( ndev )
    if (ierr/=0) call error("[GPU] cannot get DeviceCount")
    ierr = cudaSetDevice(mod(myid_host_loc, ndev))
    if (ierr/=0) call error("[GPU] cannot set device")
    ierr = cudaDeviceSynchronize()
    if (ierr/=0) call error("[GPU] cannot device-sync")
    !
    gpu_subscription=PAR_COM_HOST%n_CPU / ndev
    !
    !<debug>
    !write(*,*) "MPI ", myid, " on node ", trim(host_name), " is using GPU: ", mod(myid_host_loc, ndev)
    !</debug>
    !
#endif
    !
    have_gpu_devices=.false. 
    if (have_gpu) then
      !
      call get_environment_variable("CUDA_VISIBLE_DEVICES",gpu_visible_devices)
      if (len_trim(gpu_visible_devices) >0) have_gpu_devices=.true.
      !
      if (.not.have_gpu_devices) then
         call get_environment_variable("ROCR_VISIBLE_DEVICES",gpu_visible_devices)
         if (len_trim(gpu_visible_devices) >0) have_gpu_devices=.true.
      endif
      if (.not.have_gpu_devices) then
         call get_environment_variable("HIP_VISIBLE_DEVICES",gpu_visible_devices)
         if (len_trim(gpu_visible_devices) >0) have_gpu_devices=.true.
      endif
      !
    endif
    !
#if defined _CUDA
#  if defined _CUDAF
    stream_default=cudaforGetDefaultStream()
    ierr = CudaStreamCreateWithFlags(stream_h2d,cudaStreamNonBlocking)
    ierr = CudaStreamCreateWithFlags(stream_d2h,cudaStreamNonBlocking)
    ierr = CudaStreamCreateWithFlags(stream_d,  cudaStreamNonBlocking)
    async_default =stream_default
    async_h2d     =stream_h2d
    async_d2h     =stream_d2h
    async_d       =stream_d
    !
#  elif defined _OPENACC
    !
    ! 2022-02-02 DS and AF TO BE FIXED
    !stream_default=acc_async_noval
    stream_default=acc_get_cuda_stream(acc_async_sync)
    async_default=acc_get_cuda_stream(acc_async_sync)
    !call dev_stream_create(stream_default,default_stream=.true.)
    stream_h2d=1001
    stream_d2h=1002
    stream_d=10
    !
    async_h2d=1001
    async_d2h=1002
    async_d=10
    !
#  endif
#endif
!
#if defined _HIP && _OPENMP_GPU
#endif
    !
  end subroutine gpu_setup
  !
  subroutine gpu_linalg_setup()
    implicit none
#if defined _CUDAF || defined  _OPNEACC 
    integer :: istat 
    istat = cublasInit()
    if(istat/=0) call error('cublasInit failed')
    istat = cusolverDnCreate(cusolv_h)
    if(istat/=CUSOLVER_STATUS_SUCCESS) call error('cusolverDnCreate failed')
#endif
    gpu_linalg_init=.true.
  end subroutine

  function gpu_getstat() result(ierr)
    implicit none
    integer :: ierr
    ierr = 0
#if ( defined _CUDAF || defined  _OPNEACC ) && defined _CUDA
    ierr = CudaDeviceSynchronize()
    ierr = CudaGetLastError()
#endif
#if defined _OPENMP_GPU && defined _HIP
    ierr = HipDeviceSynchronize()
    ierr = HipGetLastError()
#endif
    return
  end function

  function gpu_devsync() result(ierr)
    implicit none
    integer :: ierr
    ierr = 0 
#if ( defined _CUDAF || defined  _OPNEACC ) && defined _CUDA
    ierr = CudaDeviceSynchronize()
#endif
#if defined _OPENMP_GPU && defined _HIP
    ierr = HipDeviceSynchronize()
#endif
    return
  end function
  
end module gpu_m

