!
! License-Identifier: GPL
!
! Copyright (C) 2022 the Yambo Team
!
! Authors (see AUTHORS file for details): MF, AF
!
!================
module gpu_m
  !================
  !
  use devxlib_environment
  use devxlib_cublas
  use devxlib_memcpy
  use devxlib_linalg
  use devxlib_async,     ONLY:devxlib_async2stream
#if defined _GPU && defined _CUDA
  use cusolverdn
#endif
#if defined _CUDAF
  use cufft
#elif defined _OPENACC
  use cufft_m
#elif defined _OPENMP_GPU && defined _HIP
  use hip_m
  use hipfft_m
  use omp_lib
#elif defined _OPENMP_GPU && defined _MKLGPU
  use omp_lib
  use mkl_dfti_omp_offload
#endif
  use parallel_m,   ONLY:myid,host_name,PAR_COM_HOST
  use iso_c_binding
  !
  implicit none
  public
  !
  integer :: GPU_test_dim = 1000000
  !
#ifdef _GPU
  logical, parameter:: have_gpu=.true.
#else
  logical, parameter:: have_gpu=.false.
#endif
#ifdef _CUDA
  logical, parameter:: have_cuda=.true.
#else
  logical, parameter:: have_cuda=.false.
#endif
#ifdef _HIP
  logical, parameter:: have_hip=.true.
#else
  logical, parameter:: have_hip=.false.
#endif
#ifdef _MKLGPU
  logical, parameter:: have_mklgpu=.true.
#else
  logical, parameter:: have_mklgpu=.false.
#endif
  !
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
  !
  character(16)  :: gpu_str=""
  !
  logical:: have_gpu_devices=.false.
  character(256) :: gpu_visible_devices=" "
  integer:: gpu_subscription

  logical:: gpu_linalg_init=.false.
#if defined _GPU && defined _CUDA
  type(cusolverDnHandle)   :: cusolv_h
  public :: cusolv_h
#endif

  !
  ! Streams
  !
  ! The following should be ok with both CUDA and HIP streams
  !
#if defined _CUDAF
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
  public :: gpu_GetDevice
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
    integer :: ndev,myid_host_loc,devtype
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
#if defined _CUDAF && defined _CUDA
    ierr = cudaGetDeviceCount( ndev )
    if (ierr/=0) call error("[GPU] cannot get DeviceCount")
    ierr = cudaSetDevice(mod(myid_host_loc, ndev))
    if (ierr/=0) call error("[GPU] cannot set device")
    ierr = cudaDeviceSynchronize()
    if (ierr/=0) call error("[GPU] cannot device-sync")
#endif
#if defined _OPENACC
    devtype = acc_get_device_type()
    ndev = acc_get_num_devices(devtype)
    call acc_set_device_num(mod(myid_host_loc, ndev),devtype)
# if defined _CUDA
    ierr = cudaDeviceSynchronize()
    if (ierr/=0) call error("[GPU] cannot device-sync")
# endif
#endif
#if defined _HIP
    ierr = hipGetDeviceCount( ndev )
    if (ierr/=0) call error("[GPU] cannot get DeviceCount")
    ierr = hipSetDevice(mod(myid_host_loc, ndev))
    if (ierr/=0) call error("[GPU] cannot set device")
    ierr = hipDeviceSynchronize()
    if (ierr/=0) call error("[GPU] cannot device-sync")
#endif
#if defined _MKLGPU && defined _OPENMP_GPU
    ndev = omp_get_num_devices()
    if (ndev==0) call error("[GPU] cannot get DeviceCount")
    call omp_set_default_device(mod(myid_host_loc, ndev))
#endif
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
      if (.not.have_gpu_devices) then
#if defined _OPENMP_GPU
         ndev = omp_get_num_devices()
         if (ndev >0) have_gpu_devices=.true.
#endif
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
#   if defined _PGI || defined _NV
    stream_default=acc_get_cuda_stream(acc_async_sync)
    async_default=acc_get_cuda_stream(acc_async_sync)
#   else
    ! The following 2 lines, in place of the 2 above,
    ! are needed to compile with gfortran
    ! However they give an error with nvfortran
    stream_default=devxlib_async2stream(acc_async_sync)
    async_default=devxlib_async2stream(acc_async_sync)
#endif
    !call dev_stream_create(stream_default,default_stream=.true.)
    !
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
    !
    stream_default=hipStreamDefault
    ierr = hipStreamCreateWithFlags(stream_h2d,hipStreamNonBlocking)
    ierr = hipStreamCreateWithFlags(stream_d2h,hipStreamNonBlocking)
    ierr = hipStreamCreateWithFlags(stream_d,  hipStreamNonBlocking)
    async_default =stream_default
    async_h2d     =stream_h2d
    async_d2h     =stream_d2h
    async_d       =stream_d
    !
#endif
    !
    ! basic LinAlg functionalities of devxlib
    !
    call dev_linalg_setup()
    !
  end subroutine gpu_setup
  !
  subroutine gpu_linalg_setup()
    implicit none
#if defined _CUDAF || defined  _OPENACC
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
#if ( defined _CUDAF || defined  _OPENACC ) && defined _CUDA
    ierr = CudaDeviceSynchronize()
    ierr = CudaGetLastError()
#endif
#if defined _OPENMP_GPU && defined _HIP
    ierr = hipDeviceSynchronize()
    ierr = hipGetLastError()
#endif
    return
  end function

  function gpu_devsync() result(ierr)
    implicit none
    integer :: ierr
    ierr = 0
#if ( defined _CUDAF || defined  _OPENACC ) && defined _CUDA
    ierr = CudaDeviceSynchronize()
#endif
#if defined _OPENMP_GPU && defined _HIP
    ierr = hipDeviceSynchronize()
#endif
    return
  end function

  function gpu_GetDevice(i_dev) result(ierr)
    implicit none
    integer :: ierr, i_dev, devtype
    ierr = 0
#if defined _CUDAF && defined _CUDA
    ierr = cudaGetDevice(i_dev)
#endif
#if defined _OPENACC
    devtype = acc_get_device_type()
    i_dev = acc_get_device_num(devtype)
#endif
#if defined _OPENMP_GPU && defined _HIP
    ierr = hipGetDevice(i_dev)
#endif
#if defined _OPENMP_GPU && defined _MKLGPU
    i_dev = omp_get_default_device()
#endif
    return
  end function

end module gpu_m

