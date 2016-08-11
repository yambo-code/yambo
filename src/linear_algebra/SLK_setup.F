!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, AF
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
SUBROUTINE SLK_setup( ORTHO, POOL, INDEX_comm, A2A_comm )
 !
 use SLK_m,       ONLY:n_pools,ORTHO_group,POOL_group
 use parallel_m,  ONLY:MPI_comm,ncpu,myid
 !
 IMPLICIT NONE
 !
 type(MPI_comm),    intent(in)    :: INDEX_comm 
 type(MPI_comm),    intent(in)    :: A2A_comm 
 type(ORTHO_group), intent(inout) :: ORTHO 
 type(POOL_group) , intent(inout) :: POOL 
 !
 ! # of POOLS
 !
 n_pools = INDEX_comm%n_CPU
 !
 ! BLACS grid
 !
 ORTHO%grid=nint(sqrt(real(ncpu/n_pools)))
 !
 ! Init pools
 !
 POOL%CPU_id      = A2A_comm%CPU_id
 POOL%n_CPU       = A2A_comm%n_CPU
 POOL%ID          = myid/POOL%n_CPU
 POOL%INTER_comm  = INDEX_comm%COMM
 POOL%INTRA_comm  = A2A_comm%COMM
 !
 ! Init the scalapack grid
 !
 call SLK_ORTHO_init( ORTHO, POOL, product(ORTHO%grid), POOL%INTRA_comm )
 !
END SUBROUTINE SLK_setup
