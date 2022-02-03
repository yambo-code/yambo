!
!        Copyright (C) 2000-2021 the YAMBO team
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
module cuda_m
  !================
  !
  use dev_openacc_m
  ! 2022-02-02 DS and AF TO BE FIXED
  !use dev_cudafor_m
  use dev_cublas_m
  use device_memcpy_m
  use cusolverdn_y
# if defined _CUDAF
  use cudafor
  !use cublas
  use cufft
  !use cusolverdn_y
# elif defined _OPENACC
  use cudafor_m
  use cufft_m
# endif
  use parallel_m,   ONLY:myid,host_name,PAR_COM_HOST
  use iso_c_binding
  !
  implicit none
  public
  !
  integer :: GPU_test_dim = 1000000

#ifdef _GPU
  logical, parameter:: have_cuda=.true.
#else
  logical, parameter:: have_cuda=.false.
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
  !
  logical:: have_cuda_devices=.false.
  character(256) :: cuda_visible_devices=" "
  integer:: cuda_gpu_subscription

  logical:: cuda_linalg_init=.false.
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
  ! public
  !
  public :: have_cuda
  public :: have_cuf
  public :: have_openacc
  public :: have_cuda_devices
  public :: cuda_visible_devices
  public :: cuda_setup
  public :: cuda_linalg_init
  public :: cuda_linalg_setup
  public :: cuda_getstat
  public :: cuda_devsync
  !
  public :: cuda_gpu_subscription
  !
  public :: stream_kind
  public :: stream_default
  public :: stream_h2d
  public :: stream_d2h
  public :: stream_d
  !
#if defined _GPU
  public :: cudaGetDevice
#endif

contains

  subroutine cuda_setup()
    implicit none
    integer :: ndev,myid_host_loc
    integer :: ierr
    !
    cuda_gpu_subscription=1
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
    cuda_gpu_subscription=PAR_COM_HOST%n_CPU / ndev
    !
    !<debug>
    !write(*,*) "MPI ", myid, " on node ", trim(host_name), " is using GPU: ", mod(myid_host_loc, ndev)
    !</debug>
    !
#endif
    !
    have_cuda_devices=.false. 
    if (have_cuda) then
      !
      call get_environment_variable("CUDA_VISIBLE_DEVICES",cuda_visible_devices)
      if (len_trim(cuda_visible_devices) >0) have_cuda_devices=.true.
      !
    endif
    !
#if defined _CUDAF
    stream_default=cudaforGetDefaultStream()
    ierr = CudaStreamCreateWithFlags(stream_h2d,cudaStreamNonBlocking)
    ierr = CudaStreamCreateWithFlags(stream_d2h,cudaStreamNonBlocking)
    ierr = CudaStreamCreateWithFlags(stream_d,  cudaStreamNonBlocking)
#elif defined _OPENACC
    ! 2022-02-02 DS and AF TO BE FIXED
    !stream_default=acc_async_noval
    !stream_default=acc_get_cuda_stream(acc_async_sync)
    call dev_stream_create(stream_default,default_stream=.true.)
    stream_h2d=1001
    stream_d2h=1002
    stream_d=10
    !
#endif
    !
  end subroutine cuda_setup
  !
  subroutine cuda_linalg_setup()
    implicit none
#ifdef _GPU
    integer :: istat 
    istat = cublasInit()
    if(istat/=0) call error('cublasInit failed')
    istat = cusolverDnCreate(cusolv_h)
    if(istat/=CUSOLVER_STATUS_SUCCESS) call error('cusolverDnCreate failed')
#endif
    cuda_linalg_init=.true.
  end subroutine

  function cuda_getstat() result(ierr)
    implicit none
    integer :: ierr
    ierr = 0
#ifdef _GPU
    ierr = CudaDeviceSynchronize()
    ierr = CudaGetLastError()
    !<debug>
    !write(*,*) CudaGetLastMessage(ierr)
    !</debug>
#endif
    return
  end function

  function cuda_devsync() result(ierr)
    implicit none
    integer :: ierr
    ierr = 0 
#ifdef _GPU
    ierr = CudaDeviceSynchronize()
#endif
    return
  end function
  
end module cuda_m


!=========================

! ----
! nvtx
! ----

module nvtx
  use iso_c_binding
#ifdef _CUDAF
  use cudafor
#endif
#if defined _OPENACC || defined _OPENMP5
  use cudafor_m
#endif
  implicit none

#ifdef _NVTX
  integer,private :: col(7) = [ Z'0000ff00', Z'000000ff', Z'00ffff00',Z'00ff00ff',Z'0000ffff', &
                                Z'00ff0000', Z'00ffffff']
  character(len=256),private :: tempName
!  logical, save :: use_nvtx=.false.
  type, bind(C):: nvtxEventAttributes
     integer(C_INT16_T):: version=1
     integer(C_INT16_T):: size=48 !
     integer(C_INT):: category=0
     integer(C_INT):: colorType=1 ! NVTX_COLOR_ARGB = 1
     integer(C_INT):: color
     integer(C_INT):: payloadType=0 ! NVTX_PAYLOAD_UNKNOWN = 0
     integer(C_INT):: reserved0
     integer(C_INT64_T):: payload   ! union uint,int,double
     integer(C_INT):: messageType=1  ! NVTX_MESSAGE_TYPE_ASCII     = 1 
     type(C_PTR):: message  ! ascii char
  end type nvtxEventAttributes

  interface nvtxRangePush
     ! push range with custom label and standard color
     subroutine nvtxRangePushA(name) bind(C, name='nvtxRangePushA')
       use iso_c_binding
       character(kind=C_CHAR,len=*) :: name
     end subroutine nvtxRangePushA

     ! push range with custom label and custom color
     subroutine nvtxRangePushEx(event) bind(C, name='nvtxRangePushEx')
       use iso_c_binding
       import:: nvtxEventAttributes
       type(nvtxEventAttributes):: event
     end subroutine nvtxRangePushEx
  end interface nvtxRangePush

  interface nvtxRangePop
     subroutine nvtxRangePop() bind(C, name='nvtxRangePop')
     end subroutine nvtxRangePop
  end interface nvtxRangePop
#endif

contains

  subroutine nvtxStartRange(name,id)
    character(kind=c_char,len=*) :: name
    integer, optional:: id
#ifdef _NVTX
    type(nvtxEventAttributes):: event
#ifdef _GPU
    integer :: istat
    istat = cudaDeviceSynchronize()
#endif

    tempName=trim(name)//c_null_char

    if ( .not. present(id)) then
       call nvtxRangePush(tempName)
    else
       event%color=col(mod(id,7)+1)
       event%message=c_loc(tempName)
       call nvtxRangePushEx(event)
    end if
#endif
  end subroutine nvtxStartRange

  subroutine nvtxStartRangeAsync(name,id)
    character(kind=c_char,len=*) :: name
    integer, optional:: id
#ifdef _NVTX
    type(nvtxEventAttributes):: event

    tempName=trim(name)//c_null_char

    if ( .not. present(id)) then
       call nvtxRangePush(tempName)
    else
       event%color=col(mod(id,7)+1)
       event%message=c_loc(tempName)
       call nvtxRangePushEx(event)
    end if
#endif
  end subroutine nvtxStartRangeAsync


  subroutine nvtxEndRange
#ifdef _NVTX
#ifdef _GPU
    integer :: istat
    istat = cudaDeviceSynchronize()
#endif
    call nvtxRangePop
#endif
  end subroutine nvtxEndRange

  subroutine nvtxEndRangeAsync
#ifdef _NVTX
    call nvtxRangePop
#endif
  end subroutine nvtxEndRangeAsync

end module nvtx

