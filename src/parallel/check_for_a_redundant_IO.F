!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
 implicit none
 include 'mpif.h'
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
