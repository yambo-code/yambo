!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
module par_proc_m
  !
  use pars,       ONLY:SP,DP
  implicit none
  !
#if defined MPI
  include 'mpif.h'
#endif
  !
  integer            :: myid
  integer            :: ncpu
  integer, parameter :: p_sum=1
  integer, parameter :: p_prod=2
  logical            :: master_node
  !
  type pp_indexes 
    logical, pointer :: i1p(:)
    logical, pointer :: i2p(:,:)
    integer, pointer :: stps(:)
    integer, pointer :: i1wts(:)
  end type pp_indexes
  !
  integer, private :: local_type
  !
  interface pp_redux_wait
    module procedure i1share,i18share,i2share,i3share,r0share,&
&                    r1share,r2share,r3share,c1share,c2share,&
&                    c3share,c4share,pwait
  end interface pp_redux_wait
  !
contains
  ! 
  subroutine pp_indexes_reset(ip)
    type(pp_indexes)::ip
    nullify(ip%i1p,ip%i2p,ip%stps,ip%i1wts)
  end subroutine
  !
  subroutine p_finalize
    implicit none
#if defined MPI
    integer :: ierr
    if (ncpu>1) then
      call mpi_barrier(mpi_comm_world,ierr)
      call mpi_finalize(ierr)
    endif
#endif
    stop
  end subroutine
  !
  subroutine pwait
#if defined MPI
    integer :: ierr
    if (ncpu==1) return
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
  subroutine i1share(array,imode)
    integer:: array(:)
    integer, optional :: imode
#if defined MPI
    integer ::ierr,omode
    integer ::dimensions(1),dimension ! Work Space
    integer,allocatable::larray(:)   ! Work Space
    if (ncpu==1) return
    call mpi_barrier(mpi_comm_world,ierr)
    if (present(imode)) then
      if (imode==1) omode=mpi_sum
      if (imode==2) omode=mpi_prod
    else
      omode=mpi_sum
    endif
    dimensions=shape(array)
    dimension=product(dimensions)
    allocate(larray(dimension))
    larray=0.
    call mpi_allreduce(array(1),larray,dimension,&
&        mpi_integer,omode,mpi_comm_world,ierr)
    array=reshape(larray,dimensions)
    deallocate(larray)
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
  subroutine i18share(array,imode)
    integer(8)        :: array(:)
    integer, optional :: imode
#if defined MPI
    integer :: ierr,omode
    integer::dimensions(1),dimension !Work Space
    integer(8),allocatable::larray(:) !Work Space
    if (ncpu==1) return
    call mpi_barrier(mpi_comm_world,ierr)
    if (present(imode)) then
      if (imode==1) omode=mpi_sum
      if (imode==2) omode=mpi_prod
    else
      omode=mpi_sum
    endif
    dimensions=shape(array)
    dimension=product(dimensions)
    allocate(larray(dimension))
    larray=0.
    call mpi_allreduce(array(1),larray,dimension,&
&        mpi_integer8,omode,mpi_comm_world,ierr)
    array=reshape(larray,dimensions)
    deallocate(larray)
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
  subroutine i2share(array,mode)
    integer :: array(:,:)
    integer, optional :: mode
#if defined MPI
    integer :: ierr
    integer::dimensions(2),dimension  ! Work Space
    integer,allocatable::larray(:)  ! Work Space
    if (ncpu==1) return
    call mpi_barrier(mpi_comm_world,ierr)
    dimensions=shape(array)
    dimension=product(dimensions)
    allocate(larray(dimension))
    larray=0
    call mpi_allreduce(array(1,1),larray,dimension,&
&        mpi_integer,mpi_sum,mpi_comm_world,ierr)
    array=reshape(larray,dimensions)
    deallocate(larray)
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
  subroutine i3share(array,mode)
    integer:: array(:,:,:)
    integer, optional :: mode
#if defined MPI
    integer :: ierr
    integer::dimensions(3),dimension  ! Work Space
    integer,allocatable::larray(:) ! Work Space
    if (ncpu==1) return
    call mpi_barrier(mpi_comm_world,ierr)
    dimensions=shape(array)
    dimension=product(dimensions)
    allocate(larray(dimension))
    larray=0.
    call mpi_allreduce(array(1,1,1),larray,dimension,&
&        mpi_integer,mpi_sum,mpi_comm_world,ierr)
    array=reshape(larray,dimensions)
    deallocate(larray)
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
  subroutine r0share(rval,imode)
    real(SP)          :: rval
    integer, optional :: imode
#if defined MPI
    integer :: ierr,omode  ! Work Space
    real(SP):: local_rval  ! Work Space
    if (ncpu==1) return
    !
    local_type=MPI_REAL
    if (SP==DP) local_type=MPI_DOUBLE_PRECISION
    !
    call mpi_barrier(mpi_comm_world,ierr)
    if (present(imode)) then
      if (imode==1) omode=mpi_sum
      if (imode==2) omode=mpi_prod
    else
      omode=mpi_sum
    endif
    local_rval=0.
    call mpi_allreduce(rval,local_rval,1,&
&        local_type,omode,mpi_comm_world,ierr)
    rval=local_rval
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
  subroutine r1share(array,mode)
    real(SP) :: array(:)
    integer, optional :: mode
#if defined MPI
    integer :: ierr
    integer::dimensions(1),dimension ! Work Space
    real(SP),allocatable::larray(:)  ! Work Space
    if (ncpu==1) return
    !
    local_type=MPI_REAL
    if (SP==DP) local_type=MPI_DOUBLE_PRECISION
    !
    call mpi_barrier(mpi_comm_world,ierr)
    dimensions=shape(array)
    dimension=product(dimensions)
    allocate(larray(dimension))
    larray=0.
    call mpi_allreduce(array(1),larray,dimension,&
&        local_type,mpi_sum,mpi_comm_world,ierr)
    array=reshape(larray,dimensions)
    deallocate(larray)
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
  subroutine r2share(array,mode)
    real(SP) :: array(:,:)
    integer, optional :: mode
#if defined MPI
    integer :: ierr
    integer::dimensions(2),dimension  ! Work Space
    real(SP),allocatable::larray(:)  ! Work Space
    if (ncpu==1) return
    !
    local_type=MPI_REAL
    if (SP==DP) local_type=MPI_DOUBLE_PRECISION
    !
    call mpi_barrier(mpi_comm_world,ierr)
    dimensions=shape(array)
    dimension=product(dimensions)
    allocate(larray(dimension))
    larray=0.
    call mpi_allreduce(array(1,1),larray,dimension,&
&        local_type,mpi_sum,mpi_comm_world,ierr)
    array=reshape(larray,dimensions)
    deallocate(larray)
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
  subroutine r3share(array,mode)
    real(SP):: array(:,:,:)
    integer, optional :: mode
#if defined MPI
    integer :: ierr
    integer::dimensions(3),dimension  ! Work Space
    real(SP),allocatable::larray(:)  ! Work Space
    if (ncpu==1) return
    !
    local_type=MPI_REAL
    if (SP==DP) local_type=MPI_DOUBLE_PRECISION
    !
    call mpi_barrier(mpi_comm_world,ierr)
    dimensions=shape(array)
    dimension=product(dimensions)
    allocate(larray(dimension))
    larray=0.
    call mpi_allreduce(array(1,1,1),larray,dimension,&
&        local_type,mpi_sum,mpi_comm_world,ierr)
    array=reshape(larray,dimensions)
    deallocate(larray)
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
  subroutine c1share(array,mode)
    complex(SP):: array(:)
    integer, optional :: mode
#if defined MPI
    integer :: ierr
    complex(SP),allocatable::larray(:)  ! Work Space
    if (ncpu==1) return
    !
    local_type=MPI_COMPLEX
    if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
    !
    call mpi_barrier(mpi_comm_world,ierr)
    allocate(larray(size(array)))
    larray=0.
    call mpi_allreduce(array(1),larray,size(array),&
&        local_type,mpi_sum,mpi_comm_world,ierr)
    array=larray
    deallocate(larray)
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
  subroutine c2share(array,mode)
    complex(SP):: array(:,:)
    integer, optional :: mode
#if defined MPI
    integer :: ierr
    integer::dimensions(2),dimension  ! Work Space
    complex(SP),allocatable::larray(:)  ! Work Space
    if (ncpu==1) return
    !
    local_type=MPI_COMPLEX
    if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
    !
    call mpi_barrier(mpi_comm_world,ierr)
    dimensions=shape(array)
    dimension=product(dimensions)
    allocate(larray(dimension))
    larray=(0.,0.)
    call mpi_allreduce(array(1,1),larray,dimension,&
&        local_type,mpi_sum,mpi_comm_world,ierr)
    array=reshape(larray,dimensions)
    deallocate(larray)
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
  subroutine c3share(array,mode)
    complex(SP):: array(:,:,:)
    integer, optional :: mode
#if defined MPI
    integer :: ierr
    integer::dimensions(3),dimension  ! Work Space
    complex(SP),allocatable::larray(:)  ! Work Space
    if (ncpu==1) return
    !
    local_type=MPI_COMPLEX
    if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
    !
    call mpi_barrier(mpi_comm_world,ierr)
    dimensions=shape(array)
    dimension=product(dimensions)
    allocate(larray(dimension))
    larray=0.
    call mpi_allreduce(array(1,1,1),larray,dimension,&
&        local_type,mpi_sum,mpi_comm_world,ierr)
    array=reshape(larray,dimensions)
    deallocate(larray)
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
  subroutine c4share(array,mode)
    complex(SP):: array(:,:,:,:)
    integer, optional :: mode
#if defined MPI
    integer :: ierr
    integer::dimensions(4),dimension  ! Work Space
    complex(SP),allocatable::larray(:)  ! Work Space
    if (ncpu==1) return
    !
    local_type=MPI_COMPLEX
    if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
    !
    call mpi_barrier(mpi_comm_world,ierr)
    dimensions=shape(array)
    dimension=product(dimensions)
    allocate(larray(dimension))
    larray=0.
    call mpi_allreduce(array(1,1,1,1),larray,dimension,&
&        local_type,mpi_sum,mpi_comm_world,ierr)
    array=reshape(larray,dimensions)
    deallocate(larray)
    call mpi_barrier(mpi_comm_world,ierr)
#endif
  end subroutine
  !
end module par_proc_m
