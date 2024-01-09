!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine check_for_a_redundant_IO(path_,cpu_seen_by_me,cpu_seen_by_any)
 !
 ! In this routine I check if all cpu
 ! can access the same path_. If not only distinct
 ! cpu's are allowed to write
 !
#if defined _MPI
 use pars,      ONLY:lchlen
 use stderr,    ONLY:cstr,intc
 use parallel_m,ONLY:myid,ncpu
 use mpi
 implicit none
! 2021/01/14 DS, commented.
! It maybe needed for old libraries
! To be uncommented but protected with precompiler flags on gfortran compiler
! include 'mpif.h'
 character(*)    ::path_
 !
 ! Work Space
 !
 character(lchlen) ::file_name(ncpu)
 integer           ::ic,i_err
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
 call mpi_barrier(mpi_comm_world,i_err)
 cpu_seen_by_me=0
 do ic=1,ncpu
   inquire(file=trim(file_name(ic)),exist=file_exists)
   if (file_exists) cpu_seen_by_me(ic)=1
 enddo
 call mpi_barrier(mpi_comm_world,i_err)
 do ic=1,ncpu
   if (myid/=ic-1) cycle
   call iremove( cstr(trim(file_name(ic))) )
 enddo
 call mpi_barrier(mpi_comm_world,i_err)
 call mpi_allreduce(cpu_seen_by_me,cpu_seen_by_any,ncpu,&
&     mpi_integer8,mpi_sum,mpi_comm_world,i_err)
#endif
 !
end subroutine
