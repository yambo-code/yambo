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
!=================
   use pars
   integer            :: myid,ncpu
   integer, parameter :: p_sum=1,p_prod=2
   logical            :: master_node
   type pp_indexes 
     logical, pointer :: i1p(:),i2p(:,:)
     integer, pointer :: stps(:),i1wts(:)
   end type pp_indexes
   interface pp_redux_wait
     module procedure i1share,i18share,i2share,i3share,r0share,&
&                     r1share,r2share,r3share,c1share,c2share,&
&                     c3share,c4share,pwait
   end interface pp_redux_wait

 contains

 subroutine pp_indexes_reset(ip)
!===============================
 implicit none
 type(pp_indexes)::ip
 nullify(ip%i1p,ip%i2p,ip%stps,ip%i1wts)
 end subroutine

 subroutine p_finalize
!=====================
 implicit none
#if defined MPI
 include 'mpif.h'
 integer :: ierr
 if (ncpu>1) then
   call mpi_barrier(mpi_comm_world,ierr)
   call mpi_finalize(ierr)
 endif
#endif
 stop
 end subroutine

 subroutine pwait
!================
 implicit none
#if defined MPI
 include 'mpif.h'
 integer :: ierr
 if (ncpu==1) return
 call mpi_barrier(mpi_comm_world,ierr)
#endif
 end subroutine

 subroutine i1share(array,imode)
!===============================
 implicit none
 integer:: array(:)
 integer, optional :: imode
#if defined MPI
 include 'mpif.h'
 integer :: ierr,omode
!ws
 integer::dimensions(1),dimension
 integer,allocatable::larray(:)
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
&     mpi_integer,omode,mpi_comm_world,ierr)
 array=reshape(larray,dimensions)
 deallocate(larray)
 call mpi_barrier(mpi_comm_world,ierr)
#endif
 end subroutine

 subroutine i18share(array,imode)
!================================
 implicit none
 integer(8):: array(:)
 integer, optional :: imode
#if defined MPI
 include 'mpif.h'
 integer :: ierr,omode
!ws
 integer::dimensions(1),dimension
 integer(8),allocatable::larray(:)
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
&     mpi_integer8,omode,mpi_comm_world,ierr)
 array=reshape(larray,dimensions)
 deallocate(larray)
 call mpi_barrier(mpi_comm_world,ierr)
#endif
 end subroutine

 subroutine i2share(array,mode)
!==============================
 implicit none
 integer:: array(:,:)
 integer, optional :: mode
#if defined MPI
 include 'mpif.h'
 integer :: ierr
!ws
 integer::dimensions(2),dimension
 integer,allocatable::larray(:)
 if (ncpu==1) return
 call mpi_barrier(mpi_comm_world,ierr)
 dimensions=shape(array)
 dimension=product(dimensions)
 allocate(larray(dimension))
 larray=0
 call mpi_allreduce(array(1,1),larray,dimension,&
&     mpi_integer,mpi_sum,mpi_comm_world,ierr)
 array=reshape(larray,dimensions)
 deallocate(larray)
 call mpi_barrier(mpi_comm_world,ierr)
#endif
 end subroutine

 subroutine i3share(array,mode)
!==============================
 implicit none
 integer:: array(:,:,:)
 integer, optional :: mode
#if defined MPI
 include 'mpif.h'
 integer :: ierr
!ws
 integer::dimensions(3),dimension
 integer,allocatable::larray(:)
 if (ncpu==1) return
 call mpi_barrier(mpi_comm_world,ierr)
 dimensions=shape(array)
 dimension=product(dimensions)
 allocate(larray(dimension))
 larray=0.
 call mpi_allreduce(array(1,1,1),larray,dimension,&
&     mpi_integer,mpi_sum,mpi_comm_world,ierr)
 array=reshape(larray,dimensions)
 deallocate(larray)
 call mpi_barrier(mpi_comm_world,ierr)
#endif
 end subroutine

 subroutine r0share(rval,imode)
!==============================
 implicit none
 real(SP)          :: rval
 integer, optional :: imode
#if defined MPI
 include 'mpif.h'
 integer :: ierr,omode
 real(SP):: local_rval
!ws
 if (ncpu==1) return
 call mpi_barrier(mpi_comm_world,ierr)
 if (present(imode)) then
  if (imode==1) omode=mpi_sum
  if (imode==2) omode=mpi_prod
 else
  omode=mpi_sum
 endif
 local_rval=0.
 call mpi_allreduce(rval,local_rval,1,&
&     mpi_real,omode,mpi_comm_world,ierr)
 rval=local_rval
 call mpi_barrier(mpi_comm_world,ierr)
#endif
 end subroutine

 subroutine r1share(array,mode)
!==============================
 implicit none
 real(SP):: array(:)
 integer, optional :: mode
#if defined MPI
 include 'mpif.h'
 integer :: ierr
!ws
 integer::dimensions(1),dimension
 real(SP),allocatable::larray(:)
 if (ncpu==1) return
 call mpi_barrier(mpi_comm_world,ierr)
 dimensions=shape(array)
 dimension=product(dimensions)
 allocate(larray(dimension))
 larray=0.
 call mpi_allreduce(array(1),larray,dimension,&
&     mpi_real,mpi_sum,mpi_comm_world,ierr)
 array=reshape(larray,dimensions)
 deallocate(larray)
 call mpi_barrier(mpi_comm_world,ierr)
#endif
 end subroutine

 subroutine r2share(array,mode)
!==============================
 implicit none
 real(SP):: array(:,:)
 integer, optional :: mode
#if defined MPI
 include 'mpif.h'
 integer :: ierr
!ws
 integer::dimensions(2),dimension
 real(SP),allocatable::larray(:)
 if (ncpu==1) return
 call mpi_barrier(mpi_comm_world,ierr)
 dimensions=shape(array)
 dimension=product(dimensions)
 allocate(larray(dimension))
 larray=0.
 call mpi_allreduce(array(1,1),larray,dimension,&
&     mpi_real,mpi_sum,mpi_comm_world,ierr)
 array=reshape(larray,dimensions)
 deallocate(larray)
 call mpi_barrier(mpi_comm_world,ierr)
#endif
 end subroutine

 subroutine r3share(array,mode)
!==============================
 implicit none
 real(SP):: array(:,:,:)
 integer, optional :: mode
#if defined MPI
 include 'mpif.h'
 integer :: ierr
!ws
 integer::dimensions(3),dimension
 real(SP),allocatable::larray(:)
 if (ncpu==1) return
 call mpi_barrier(mpi_comm_world,ierr)
 dimensions=shape(array)
 dimension=product(dimensions)
 allocate(larray(dimension))
 larray=0.
 call mpi_allreduce(array(1,1,1),larray,dimension,&
&     mpi_real,mpi_sum,mpi_comm_world,ierr)
 array=reshape(larray,dimensions)
 deallocate(larray)
 call mpi_barrier(mpi_comm_world,ierr)
#endif
 end subroutine

 subroutine c1share(array,mode)
!==============================
 implicit none
 complex(SP):: array(:)
 integer, optional :: mode
#if defined MPI
 include 'mpif.h'
 integer :: ierr
!ws
 integer::dimensions(1),dimension
 complex(SP),allocatable::larray(:)
 if (ncpu==1) return
 call mpi_barrier(mpi_comm_world,ierr)
 dimensions=shape(array)
 dimension=product(dimensions)
 allocate(larray(dimension))
 larray=0.
 call mpi_allreduce(array(1),larray,dimension,&
&     mpi_complex,mpi_sum,mpi_comm_world,ierr)
 array=reshape(larray,dimensions)
 deallocate(larray)
 call mpi_barrier(mpi_comm_world,ierr)
#endif

 end subroutine

 subroutine c2share(array,mode)
!==============================
 implicit none
 complex(SP):: array(:,:)
 integer, optional :: mode
#if defined MPI
 include 'mpif.h'
 integer :: ierr
!ws
 integer::dimensions(2),dimension
 complex(SP),allocatable::larray(:)
 if (ncpu==1) return
 call mpi_barrier(mpi_comm_world,ierr)
 dimensions=shape(array)
 dimension=product(dimensions)
 allocate(larray(dimension))
 larray=(0.,0.)
 call mpi_allreduce(array(1,1),larray,dimension,&
&     mpi_complex,mpi_sum,mpi_comm_world,ierr)
 array=reshape(larray,dimensions)
 deallocate(larray)
 call mpi_barrier(mpi_comm_world,ierr)
#endif
 end subroutine

 subroutine c3share(array,mode)
!==============================
 implicit none
 complex(SP):: array(:,:,:)
 integer, optional :: mode
#if defined MPI
 include 'mpif.h'
 integer :: ierr
!ws
 integer::dimensions(3),dimension
 complex(SP),allocatable::larray(:)
 if (ncpu==1) return
 call mpi_barrier(mpi_comm_world,ierr)
 dimensions=shape(array)
 dimension=product(dimensions)
 allocate(larray(dimension))
 larray=0.
 call mpi_allreduce(array(1,1,1),larray,dimension,&
&     mpi_complex,mpi_sum,mpi_comm_world,ierr)
 array=reshape(larray,dimensions)
 deallocate(larray)
 call mpi_barrier(mpi_comm_world,ierr)
#endif
 end subroutine

 subroutine c4share(array,mode)
!==============================
 implicit none
 complex(SP):: array(:,:,:,:)
 integer, optional :: mode
#if defined MPI
 include 'mpif.h'
 integer :: ierr
!ws
 integer::dimensions(4),dimension
 complex(SP),allocatable::larray(:)
 if (ncpu==1) return
 call mpi_barrier(mpi_comm_world,ierr)
 dimensions=shape(array)
 dimension=product(dimensions)
 allocate(larray(dimension))
 larray=0.
 call mpi_allreduce(array(1,1,1,1),larray,dimension,&
&     mpi_complex,mpi_sum,mpi_comm_world,ierr)
 array=reshape(larray,dimensions)
 deallocate(larray)
 call mpi_barrier(mpi_comm_world,ierr)
#endif
 end subroutine

 end module par_proc_m
