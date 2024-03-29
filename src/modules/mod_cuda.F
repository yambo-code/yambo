!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): MF AF
!
!================
module cuda_m
  !================
#ifdef _CUDA
  use cudafor
  use cublas
  use cusolverdn_y
#endif
  use parallel_m,   ONLY:myid,host_name,PAR_COM_HOST
  implicit none
  public

#ifdef _CUDA
  logical, parameter:: have_cuda=.true.
#else
  logical, parameter:: have_cuda=.false.
#endif
  !
  logical:: have_cuda_devices=.false.
  character(256) :: cuda_visible_devices=" "
  integer:: cuda_gpu_subscription

  logical:: cuda_linalg_init=.false.
#ifdef _CUDA
  type(cusolverDnHandle)   :: cusolv_h
  public :: cusolv_h
#endif
  
  !
  ! streams
  !
#ifdef _CUDA
  integer, parameter :: stream_kind=Cuda_Stream_Kind
#else
  integer, parameter :: stream_kind=kind(1)
#endif
  integer(stream_kind) :: stream_default
  integer(stream_kind) :: stream_h2d
  integer(stream_kind) :: stream_d2h

  !
  ! public
  !
  public :: have_cuda
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
#if defined _CUDA && defined _MPI
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
#if defined _CUDA
    !
    have_cuda_devices=.false. 
    call get_environment_variable("CUDA_VISIBLE_DEVICES",cuda_visible_devices)
    if (len_trim(cuda_visible_devices) >0) have_cuda_devices=.true.
    !
    stream_default=cudaforGetDefaultStream()
    ierr = CudaStreamCreateWithFlags(stream_h2d,cudaStreamNonBlocking)
    ierr = CudaStreamCreateWithFlags(stream_d2h,cudaStreamNonBlocking)
    !
#else
    have_cuda_devices=.false.
#endif
    !
  end subroutine cuda_setup
  !
  subroutine cuda_linalg_setup()
    implicit none
#ifdef _CUDA
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
#ifdef _CUDA
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
#ifdef _CUDA
    ierr = CudaDeviceSynchronize()
#endif
    return
  end function

end module cuda_m


!=========================

#if defined _CUDA

!================
module mpiDeviceUtil
#if defined(_MPI)
 use mpi
#endif
  !================
  implicit none
#if defined(_MPI)
! 2021/01/14 DS, commented.
! It maybe needed for old libraries
! To be uncommented but protected with precompiler flags on gfortran compiler
! include 'mpif.h'
#else
  integer, parameter ::MPI_MAX_PROCESSOR_NAME=20
  integer            :: mpi_comm_world=0,MPI_CHARACTER
#endif
    character (len=MPI_MAX_PROCESSOR_NAME) :: hostname
    integer :: dev_id, node_id

  interface
     subroutine quicksort(base, nmemb, elemsize, compar) &
          bind(C,name='qsort')
       use iso_c_binding
       implicit none
       !pgi$ ignore_tkr base,nmemb,elemsize,compar
       type(C_PTR), value :: base
       integer(C_SIZE_T), value :: nmemb, elemsize
       type(C_FUNPTR), value :: compar
     end subroutine quicksort

     integer function strcmp(a,b) bind(C,name='strcmp')
       use iso_c_binding
       implicit none
       !pgi$ ignore_tkr a,b
       type(C_PTR), value :: a, b
     end function strcmp
  end interface

contains

  subroutine assignDevice(dev)
    use cudafor
    implicit none
    integer :: dev
    character (len=MPI_MAX_PROCESSOR_NAME), allocatable :: hosts(:)
    !character (len=MPI_MAX_PROCESSOR_NAME) :: hostname
    integer :: namelength, color, i, j
    integer :: nProcs, myrank, newComm, newRank, ierr

#if !defined _MPI
    return
#else
    call MPI_COMM_SIZE(MPI_COMM_WORLD, nProcs, ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierr)

    ! allocate array of hostnames
    allocate(hosts(0:nProcs-1))
  
    ! Every process collects the hostname of all the nodes
    call MPI_GET_PROCESSOR_NAME(hostname, namelength, ierr)
    hosts(myrank)=hostname(1:namelength)

    do i=0,nProcs-1
       call MPI_BCAST(hosts(i),MPI_MAX_PROCESSOR_NAME,MPI_CHARACTER,i, &
            MPI_COMM_WORLD,ierr)
    end do
  
    ! sort the list of names
    call quicksort(hosts,nProcs,MPI_MAX_PROCESSOR_NAME,strcmp)

    ! assign the same color to the same node
    color=0
    do i=0,nProcs-1
       if (i > 0) then
          if ( lne(hosts(i-1),hosts(i)) ) color=color+1
       end if
       if ( leq(hostname,hosts(i)) ) exit
    end do
  
    call MPI_COMM_SPLIT(MPI_COMM_WORLD,color,0,newComm,ierr)
    call MPI_COMM_RANK(newComm, newRank, ierr)

    dev = newRank
    dev_id = dev
    node_id = color
    ierr = cudaSetDevice(dev_id)
    
    ! DEBUG <
    !do i=0,nProcs-1
    !  if(myrank == i) then
    !      write(6,"(A8,I4,A8,A12,A12,I2)") "Rank: ",myrank,"Host: ",hostname(1:namelength),"Using GPU: ",dev_id
    !  endif
    !  do j=0,1000
    !    call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    !  end do
    !end do
    !DEBUG >
    
    deallocate(hosts)
#endif
  end subroutine assignDevice

  ! lexical .eq.
  function leq(s1, s2) result(res)
    implicit none
    character (len=*) :: s1, s2
    logical :: res    
    res = .false.
    if (lle(s1,s2) .and. lge(s1,s2)) res = .true.
  end function leq

  ! lexical .ne.
  function lne(s1, s2) result(res)
    implicit none
    character (len=*) :: s1, s2
    logical :: res    
    res = .not. leq(s1, s2)
  end function lne
end module mpiDeviceUtil

! ----
! nvtx
! ----

module nvtx
  use iso_c_binding
#ifdef _CUDA
  use cudafor
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
#ifdef _CUDA
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
#ifdef _CUDA
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

#endif

