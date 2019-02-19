!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine PARALLEL_build_up_child_INTER_chains(i_child,COMM_child,i_father)
 !
 ! Create hierarchy-based INTER (=non countiguous) communicators
 !
#if defined _MPI
 !
 use parallel_m,    ONLY:MPI_comm,myid,PAR_COM_WORLD,ncpu_chain,nchains_group,&
&                        n_groups,ncpu,i_INTER_CHAIN,CREATE_the_COMM
 !
 implicit none
 !
 type(MPI_comm) :: COMM_child
 integer        :: i_child,i_father,n_father,n_child
 !
 n_father = ncpu/product(ncpu_chain(nchains_group(n_groups*2-1):i_father))
 n_child  = ncpu_chain(i_child)
 !
 COMM_child%my_CHAIN=i_INTER_CHAIN(n_father,n_child)
 call CREATE_the_COMM(PAR_COM_WORLD%COMM,COMM_child,myid)
 !
 COMM_child%chain_order=i_child-nchains_group(n_groups*2-1)+1
 !
#endif
 !
end subroutine
