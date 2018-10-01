!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MF
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

#if defined _CUDA

module mpiDeviceUtil
  implicit none
#if defined(__MPI)
  INCLUDE 'mpif.h'
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
    
#if 0
    do i=0,nProcs-1
      if(myrank == i) then
          write(6,"(A8,I4,A8,A12,A12,I2)") "Rank: ",myrank,"Host: ",hostname(1:namelength),"Using GPU: ",dev_id
      endif
      do j=0,1000
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
      end do
    end do
#endif
    deallocate(hosts)
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
#endif

! ----
! nvtx
! ----

module nvtx
  use iso_c_binding
#ifdef _CUDA
  use cudafor
#endif
  implicit none
#ifdef USE_NVTX
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
#ifdef USE_NVTX
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
#ifdef USE_NVTX
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
#ifdef USE_NVTX
#ifdef _CUDA
    integer :: istat
    istat = cudaDeviceSynchronize()
#endif
    call nvtxRangePop
#endif
  end subroutine nvtxEndRange

  subroutine nvtxEndRangeAsync
#ifdef USE_NVTX
    call nvtxRangePop
#endif
  end subroutine nvtxEndRangeAsync

end module nvtx

