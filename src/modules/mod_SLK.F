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
module SLK_m
 !
 use pars,       ONLY: SP
 use parallel_m, ONLY: MPI_comm
#include<memory.h>
 !
 integer              :: n_pools = 1 ! number of pools
 !
 ! SLK test
 !
 integer              :: SLK_test_H_dim=1
 !
 ! POOL type
 !===========
 type POOL_group
   integer :: CPU_id      = 0  ! index of the processor within a pool 
   integer :: ID          = 0  ! index of my pool
   integer :: n_CPU       = 1  ! number of processors within a pool
   integer :: INTER_comm  = 0  ! inter pool communicator
   integer :: INTRA_comm  = 0  ! intra pool communicator
 end type POOL_group
 ! 
 ! BLACS Ortho group
 !===================
 type ORTHO_group 
   INTEGER :: grid(2) = 1        ! size of the processor grid used in ortho
   INTEGER :: coordinate(2) = 0  ! coordinates of the processors
   INTEGER :: task   = 0         ! task id for the ortho group
   INTEGER :: n_CPU = 1          ! size of the ortho group:
   INTEGER :: leg   = 1          ! the distance in the father communicator
   !                             ! of two neighbour processors in ortho_comm
   INTEGER :: COMM  = 0          ! communicator for the ortho group
   INTEGER :: ROW_comm  = 0      ! communicator for the ortho row group
   INTEGER :: COL_comm  = 0      ! communicator for the ortho col group
   INTEGER :: COMM_id   = 0      ! id of the ortho_comm
   !
   INTEGER :: BLACS_id   =  0    ! BLACS processor index starting from 0
   INTEGER :: BLACS_np   =  1    ! BLACS number of processor
   INTEGER :: world_cntx = -1    ! BLACS context of all processor 
   INTEGER :: ortho_cntx = -1    ! BLACS context for ortho_comm
 end type ORTHO_group
 !
 ! Specific SLK worlds ...
 !========================
 !
 type(POOL_group ),SAVE :: SLK_POOL
 type(ORTHO_group),SAVE :: SLK_ORTHO
 !
 type(MPI_comm),SAVE    :: SLK_COM_INDEX(2)
 type(MPI_comm),SAVE    :: SLK_COM_A2A(2)
 !
 contains
   !
   subroutine SLK_POOL_clean_up(POOL)
     type(POOL_group) :: POOL
     POOL%CPU_id      = 0
     POOL%ID          = 0
     POOL%n_CPU       = 1
     POOL%INTRA_comm  = 0
   end subroutine
   !
   subroutine SLK_POOL_init(POOL,COMM)
     use parallel_m, ONLY:myid
     type(POOL_group) :: POOL
     type(MPI_comm)   :: COMM
     POOL%CPU_id      = COMM%CPU_id
     POOL%n_CPU       = COMM%n_CPU
     POOL%ID          = myid/POOL%n_CPU
     POOL%INTRA_comm  = COMM%COMM
   end subroutine
   !
   subroutine SLK_ORTHO_GROUP_clean_up(ORTHO)
     type(ORTHO_group) :: ORTHO
     ORTHO%grid(2)       = 1        
     ORTHO%coordinate(2) = 0  
     ORTHO%task          = 0         
     ORTHO%n_CPU         = 1          
     ORTHO%leg           = 1          
     ORTHO%COMM          = 0          
     ORTHO%ROW_comm      = 0      
     ORTHO%COL_comm      = 0      
     ORTHO%COMM_id       = 0      
     ORTHO%BLACS_id      = 0    
     ORTHO%BLACS_np      = 1    
     ORTHO%world_cntx    = -1    
     ORTHO%ortho_cntx    = -1    
   end subroutine
   !
end module SLK_m
