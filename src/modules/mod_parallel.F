!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
module parallel_m
 !
 use pars,       ONLY:SP,DP,cZERO,rZERO
 implicit none
 !
#if defined _MPI
 include 'mpif.h'
#endif
 !
 integer            :: myid
 integer            :: ncpu
 integer            :: n_nodes
 integer, parameter :: p_sum=1
 integer, parameter :: p_prod=2
 logical            :: master_cpu
 !
 integer, parameter :: max_n_of_cpus=1000
 !
 logical            :: local_master_cpu(max_n_of_cpus)
 !
 type PP_indexes 
   logical, pointer :: element_1D(:)     => null()
   logical, pointer :: element_2D(:,:)   => null()
   integer, pointer :: n_of_elements(:)  => null()
   integer, pointer :: weight_1D(:)      => null()
   integer, pointer :: first_of_1D(:)    => null()
 end type PP_indexes
 !
 integer, private :: local_type
 !
 interface PP_redux_wait
   module procedure i1share,i18share,i2share,i3share,r0share,&
&                   r1share,r2share,r3share,c0share,c1share,c2share,&
&                   c3share,c4share,pwait
 end interface PP_redux_wait
 !
 interface pp_bcast
   module procedure c1bcast,c2bcast
 end interface pp_bcast
 !
 contains
   !
   subroutine check_for_a_redundant_IO(path_,cpu_seen_by_me,cpu_seen_by_any)
     !
     ! In this routine I check if all cpu
     ! can access the same path_. If not only distinct
     ! cpu's are allowed to write
     !
     use pars,    ONLY:lchlen
     use stderr,  ONLY:cstr,intc
     implicit none
     character(*)    ::path_
     !
     ! Work Space
     !
     character(lchlen) ::file_name(ncpu)
     integer           ::ic,ierr
     integer(8)        ::cpu_seen_by_me(ncpu),cpu_seen_by_any(ncpu)
     logical           ::file_exists
     do ic=1,ncpu
       file_name(ic)=path_//'/access_cpu_'//trim(intc(ic))
     enddo
     do ic=1,ncpu
       if (myid/=ic-1) cycle
       open(unit=20,file=trim(file_name(ic)))
       close(20)
     enddo
#if defined _MPI
     call mpi_barrier(mpi_comm_world,ierr)
#endif
     cpu_seen_by_me=0
     do ic=1,ncpu
       inquire(file=trim(file_name(ic)),exist=file_exists)
       if (file_exists) cpu_seen_by_me(ic)=1
     enddo
#if defined _MPI
     call mpi_barrier(mpi_comm_world,ierr)
#endif
     do ic=1,ncpu
       if (myid/=ic-1) cycle
       call iremove( cstr(trim(file_name(ic))) )
     enddo
#if defined _MPI
     call mpi_barrier(mpi_comm_world,ierr)
     call mpi_allreduce(cpu_seen_by_me,cpu_seen_by_any,ncpu,&
&         mpi_integer8,mpi_sum,mpi_comm_world,ierr)
#endif
     !
   end subroutine
   !
   subroutine PP_indexes_reset(ip)
     type(PP_indexes)::ip
     if(associated(ip%element_1D))    deallocate(ip%element_1D)
     if(associated(ip%element_2D))    deallocate(ip%element_2D)
     if(associated(ip%weight_1D))     deallocate(ip%weight_1D)
     if(associated(ip%n_of_elements)) deallocate(ip%n_of_elements)
     if(associated(ip%first_of_1D))   deallocate(ip%first_of_1D)
     nullify(ip%element_1D,ip%element_2D,ip%n_of_elements,ip%weight_1D,ip%first_of_1D)
   end subroutine
   !
   subroutine MPI_close
     implicit none
#if defined _MPI
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
#if defined _MPI
     integer :: ierr
     if (ncpu==1) return
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine i1share(array,imode)
     integer(4):: array(:)
     integer, optional :: imode
#if defined _MPI
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
     larray=0
     call mpi_allreduce(array(1),larray,dimension,&
&         mpi_integer,omode,mpi_comm_world,ierr)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine i18share(array,imode)
     integer(8)        :: array(:)
     integer, optional :: imode
#if defined _MPI
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
     larray=0
     call mpi_allreduce(array(1),larray,dimension,&
&         mpi_integer8,omode,mpi_comm_world,ierr)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine i2share(array,mode)
     integer :: array(:,:)
     integer, optional :: mode
#if defined _MPI
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
&         mpi_integer,mpi_sum,mpi_comm_world,ierr)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine i3share(array,mode)
     integer:: array(:,:,:)
     integer, optional :: mode
#if defined _MPI
     integer :: ierr
     integer::dimensions(3),dimension  ! Work Space
     integer,allocatable::larray(:) ! Work Space
     if (ncpu==1) return
     call mpi_barrier(mpi_comm_world,ierr)
     dimensions=shape(array)
     dimension=product(dimensions)
     allocate(larray(dimension))
     larray=0
     call mpi_allreduce(array(1,1,1),larray,dimension,&
&         mpi_integer,mpi_sum,mpi_comm_world,ierr)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine r0share(rval,imode)
     real(SP)          :: rval
     integer, optional :: imode
#if defined _MPI
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
&         local_type,omode,mpi_comm_world,ierr)
     rval=local_rval
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine r1share(array,mode)
     real(SP) :: array(:)
     integer, optional :: mode
#if defined _MPI
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
     larray=rZERO
     call mpi_allreduce(array(1),larray,dimension,&
&         local_type,mpi_sum,mpi_comm_world,ierr)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine r2share(array,mode)
     real(SP) :: array(:,:)
     integer, optional :: mode
#if defined _MPI
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
     larray=rZERO
     call mpi_allreduce(array(1,1),larray,dimension,&
&         local_type,mpi_sum,mpi_comm_world,ierr)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine r3share(array,mode)
     real(SP):: array(:,:,:)
     integer, optional :: mode
#if defined _MPI
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
     larray=rZERO
     call mpi_allreduce(array(1,1,1),larray,dimension,&
&         local_type,mpi_sum,mpi_comm_world,ierr)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine c0share(cval,imode)
     complex(SP)       :: cval
     integer, optional :: imode
#if defined _MPI
     integer :: ierr,omode  ! Work Space
     complex(SP):: local_cval  ! Work Space
     if (ncpu==1) return
     !
     local_type=MPI_COMPLEX
     if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
     !
     call mpi_barrier(mpi_comm_world,ierr)
     if (present(imode)) then
       if (imode==1) omode=mpi_sum
       if (imode==2) omode=mpi_prod
     else
       omode=mpi_sum
     endif
     local_cval=rZERO
     call mpi_allreduce(cval,local_cval,1,&
&         local_type,omode,mpi_comm_world,ierr)
     cval=local_cval
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine c1share(array,mode)
     complex(SP):: array(:)
     integer, optional :: mode
#if defined _MPI
     integer :: ierr
     complex(SP),allocatable::larray(:)  ! Work Space
     if (ncpu==1) return
     !
     local_type=MPI_COMPLEX
     if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
     !
     call mpi_barrier(mpi_comm_world,ierr)
     allocate(larray(size(array)))
     larray=cZERO
     call mpi_allreduce(array(1),larray,size(array),&
&         local_type,mpi_sum,mpi_comm_world,ierr)
     array=larray
     deallocate(larray)
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine c2share(array,mode)
     complex(SP):: array(:,:)
     integer, optional :: mode
#if defined _MPI
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
     larray=cZERO
     call mpi_allreduce(array(1,1),larray,dimension,&
&         local_type,mpi_sum,mpi_comm_world,ierr)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine c3share(array,mode)
     complex(SP):: array(:,:,:)
     integer, optional :: mode
#if defined _MPI
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
     larray=rZERO
     call mpi_allreduce(array(1,1,1),larray,dimension,&
&         local_type,mpi_sum,mpi_comm_world,ierr)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine c4share(array,mode)
     complex(SP):: array(:,:,:,:)
     integer, optional :: mode
#if defined _MPI
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
     larray=rZERO
     call mpi_allreduce(array(1,1,1,1),larray,dimension,&
&         local_type,mpi_sum,mpi_comm_world,ierr)
     array=reshape(larray,dimensions)
     deallocate(larray)
     call mpi_barrier(mpi_comm_world,ierr)
#endif
   end subroutine
   !
   subroutine c1bcast(array,node)
     complex(SP):: array(:)
     integer, intent(in) :: node
#if defined _MPI
     integer :: ierr
     if (ncpu==1) return
     !
     local_type=MPI_COMPLEX
     if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
     !
     call mpi_bcast(array(1),size(array),local_type, node, mpi_comm_world, ierr)
#endif
   end subroutine
   !
   subroutine c2bcast(array,node)
     complex(SP):: array(:,:)
     integer, intent(in) :: node
#if defined _MPI
     integer :: ierr
     if (ncpu==1) return
     !
     local_type=MPI_COMPLEX
     if (SP==DP) local_type=MPI_DOUBLE_COMPLEX
     !
     call mpi_bcast(array(1,1),size(array),local_type, node, mpi_comm_world, ierr)
#endif
   end subroutine
   !
end module parallel_m
