!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine PARALLEL_build_up_child_chains(i_child,COMM_child,i_father)
 !
 ! Create hierarchy-based communicators
 !
#if defined _MPI
 !
 use parallel_m,    ONLY:MPI_comm,myid,mpi_comm_world
 !
 implicit none
 !
 type(MPI_comm) :: COMM_child
 integer        :: i_err,i_child,i_father,n_father,n_child
 !
 n_father=2**i_father
 n_child =2**(i_father-i_child)
 !
 COMM_child%my_CHAIN=(myid/n_father)*(n_father/n_child)+mod(myid,n_father/n_child)
 !
 call MPI_COMM_SPLIT(mpi_comm_world,COMM_child%my_CHAIN,myid,COMM_child%COMM,i_err)
 call MPI_COMM_RANK(COMM_child%COMM,COMM_child%CPU_id,i_err)
 !
 COMM_child%n_CPU=n_child
 COMM_child%chain_order=i_child
 !
#endif
 !
end subroutine
