SUBROUTINE SLK_POOLS_init( parent_comm )
 !---------------------------------------------------------------------------
 !
 use para_module, ONLY : npool,my_pool_id,me_pool,nproc_pool,intra_pool_comm,inter_pool_comm
 IMPLICIT NONE
 !
 INTEGER, INTENT(IN) :: parent_comm
 !
 INTEGER :: ierr = 0
 INTEGER :: parent_nproc = 1
 INTEGER :: parent_mype  = 0
 !
#if defined (__MPI)
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
#endif
 !
 RETURN
 !
END SUBROUTINE SLK_POOLS_init
