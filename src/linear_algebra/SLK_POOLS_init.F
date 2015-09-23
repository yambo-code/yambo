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
SUBROUTINE SLK_POOLS_init( parent_comm , POOL )
 !---------------------------------------------------------------------------
 !
 use para_module, ONLY : npool,my_pool_id,me_pool,nproc_pool,intra_pool_comm,inter_pool_comm
 !
 USE SLK_m, ONLY: POOL_group,n_pool
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
 nproc_pool = parent_nproc / npool
 !
 IF ( MOD( parent_nproc, npool ) /= 0 ) &
      CALL errore( 'SLK_POOLS_init', 'invalid number of pools, parent_nproc /= nproc_pool * npool', 1 )
 !
 ! ... my_pool_id  =  pool index for this processor    ( 0 : npool - 1 )
 ! ... me_pool     =  processor index within the pool  ( 0 : nproc_pool - 1 )
 !
 my_pool_id = parent_mype / nproc_pool
 me_pool    = MOD( parent_mype, nproc_pool )
 !
 CALL MPI_barrier( parent_comm, ierr )
 !
 ! ... the intra_pool_comm communicator is created
 !
 CALL MPI_COMM_SPLIT( parent_comm, my_pool_id, parent_mype, intra_pool_comm, ierr )
 !
 IF ( ierr /= 0 ) &
    CALL errore( 'SLK_POOLS_init', 'intra pool communicator initialization', ABS(ierr) )
 !
 CALL MPI_barrier( parent_comm, ierr )
 !
 ! ... the inter_pool_comm communicator is created
 !
 CALL MPI_COMM_SPLIT( parent_comm, me_pool, parent_mype, inter_pool_comm, ierr )
 !
 IF ( ierr /= 0 ) &
    CALL errore( 'SLK_POOLS_init', 'inter pool communicator initialization', ABS(ierr) )
 !
 RETURN
 !
END SUBROUTINE SLK_POOLS_init
