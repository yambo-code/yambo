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
subroutine PARALLEL_wait(COMM)
 use parallel_m,ONLY:ncpu
 implicit none
 integer, optional :: COMM
#if defined _MPI
 integer :: i_err
 include 'mpif.h'
 if (ncpu==1) return
 !
 if (present(COMM)) then
   call mpi_barrier(COMM,i_err)
 else
   call mpi_barrier(mpi_comm_world,i_err)
 endif
#endif
end subroutine
