!
!
!        Copyright (C) 2000-2015 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, AF
!
! Copyright (C) 2001-2007 Quantum-ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
SUBROUTINE SLK_POOLS_init( POOL, parent_comm )
 !---------------------------------------------------------------------------
 !
 use com,   ONLY: error
 USE SLK_m, ONLY: POOL_group,n_pools
 !
 IMPLICIT NONE
 !
 INTEGER,          INTENT(IN)    :: parent_comm
 TYPE(POOL_GROUP), INTENT(INOUT) :: POOL
 !
 INTEGER :: ierr = 0
 INTEGER :: parent_nproc = 1
 INTEGER :: parent_mype  = 0
 !
 CALL MPI_COMM_SIZE(parent_comm,parent_nproc,ierr)
 CALL MPI_COMM_RANK(parent_comm,parent_mype,ierr)
 !
 ! ... number of cpus per pool of k-points (they are created inside each image)
 !
 POOL%n_CPU = parent_nproc / n_pools
 !
 IF ( MOD( parent_nproc, n_pools ) /= 0 ) &
      CALL error( 'SLK_POOLS_init  invalid number of pools, parent_nproc /= POOL%n_CPU * n_pools' )
 !
 ! ... POOL%ID  =  pool index for this processor    ( 0 : n_pools - 1 )
 ! ... POOL%CPU_id     =  processor index within the pool  ( 0 : POOL%n_CPU - 1 )
 !
 POOL%ID = parent_mype / POOL%n_CPU
 POOL%CPU_id    = MOD( parent_mype, POOL%n_CPU )
 !
 CALL MPI_barrier( parent_comm, ierr )
 !
 ! ... the POOL%INTRA_comm communicator is created
 !
 CALL MPI_COMM_SPLIT( parent_comm, POOL%ID, parent_mype, POOL%INTRA_comm, ierr )
 !
 IF ( ierr /= 0 ) CALL error( 'SLK_POOLS_init intra pool communicator initialization' )!, ABS(ierr))
 !
 CALL MPI_barrier( parent_comm, ierr )
 !
 ! ... the POOL%INTRA_comm communicator is created
 !
 CALL MPI_COMM_SPLIT( parent_comm, POOL%CPU_id, parent_mype, POOL%INTRA_comm, ierr )
 !
 IF ( ierr /= 0 ) CALL error( 'SLK_POOLS_init inter pool communicator initialization')!, ABS(ierr) )
 !
 RETURN
 !
END SUBROUTINE SLK_POOLS_init
