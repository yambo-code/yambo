!
!        Copyright (C) 2000-2019 the YAMBO team
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
SUBROUTINE SLK_ORTHO_group( ORTHO, POOL, nproc_try_in, comm_all )
 !
 use com,        ONLY:error
 use SLK_m,      ONLY:ORTHO_group,POOL_group,n_pools
 use parallel_m, ONLY:MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD
 !
 IMPLICIT NONE
 !
 TYPE(ORTHO_group), INTENT(INOUT) :: ORTHO
 TYPE(POOL_group) , INTENT(INOUT) :: POOL
 INTEGER,              INTENT(IN) :: nproc_try_in, comm_all
 !
 LOGICAL, SAVE :: first = .true.
 INTEGER :: ierr, color, key, me_all, nproc_all, nproc_try
 !
 INTEGER, ALLOCATABLE :: blacsmap(:,:), buff(:,:)
 INTEGER, ALLOCATABLE :: ortho_cntx_pe(:,:)
 INTEGER :: nprow, npcol, myrow, mycol, i, j
 INTEGER, EXTERNAL :: BLACS_PNUM
 !
 INTEGER :: nparent=1
 INTEGER :: total_nproc=1
 INTEGER :: total_mype=0
 INTEGER :: nproc_parent=1
 INTEGER :: my_parent_id=0
 !
 CALL MPI_COMM_RANK( comm_all, me_all, ierr )
 CALL MPI_COMM_SIZE( comm_all, nproc_all, ierr )
 !
 nproc_try = MIN( nproc_try_in, nproc_all )
 nproc_try = MAX( nproc_try, 1 )
 !
 IF( .NOT. first ) THEN
   !
   IF(  ORTHO%COMM_id > 0  ) THEN
      CALL BLACS_GRIDEXIT( ORTHO%ortho_cntx )
   ENDIF
   ORTHO%ortho_cntx = -1
   !
 END IF
 !
 !  find the square closer (but lower) to nproc_try
 !
 CALL grid2d_dims( 'S', nproc_try, ORTHO%grid(1), ORTHO%grid(2) )
 !
 !  now, and only now, it is possible to define the number of tasks
 !  in the ortho group for parallel linear algebra
 !
 ORTHO%n_CPU = ORTHO%grid(1) * ORTHO%grid(2)
 !
 IF( nproc_all >= 4*ORTHO%n_CPU ) THEN
   !
   !  here we choose a processor every 4, in order not to stress memory BW
   !  on multi core procs, for which further performance enhancements are
   !  possible using OpenMP BLAS inside regter/cegter/rdiaghg/cdiaghg
   !  (to be implemented)
   !
   color = 0
   IF( me_all < 4*ORTHO%n_CPU .AND. MOD( me_all, 4 ) == 0 ) color = 1
   !
   ORTHO%leg = 4
   !
 ELSE IF( nproc_all >= 2*ORTHO%n_CPU ) THEN
   !
   !  here we choose a processor every 2, in order not to stress memory BW
   !
   color = 0
   IF( me_all < 2*ORTHO%n_CPU .AND. MOD( me_all, 2 ) == 0 ) color = 1
   !
   ORTHO%leg = 2
   !
 ELSE
   !
   !  here we choose the first processors
   !
   color = 0
   IF( me_all < ORTHO%n_CPU ) color = 1
   !
   ORTHO%leg = 1
   !
 END IF
 !
 key   = me_all
 !
 !  initialize the communicator for the new group by splitting the input communicator
 !
 CALL MPI_COMM_SPLIT( comm_all, color, key, ORTHO%COMM, ierr )
 !
 IF( ierr /= 0 ) CALL error("[SLK_ORTHO_group] initializing ortho group communicator ")
 !
 !  Computes coordinates of the processors, in row maior order
 !
 CALL MPI_COMM_RANK( ORTHO%COMM, ORTHO%task, ierr)
 !
 IF( me_all == 0 .AND. ORTHO%task /= 0 ) CALL error("[SLK_ORTHO_group] wrong root task in ortho group ")
 !
 if( color == 1 ) then
   !
   ORTHO%COMM_id = 1
   !
   CALL GRID2D_COORDS( 'R', ORTHO%task, ORTHO%grid(1), ORTHO%grid(2), ORTHO%coordinate(1), ORTHO%coordinate(2) )
   CALL GRID2D_RANK( 'R', ORTHO%grid(1), ORTHO%grid(2), ORTHO%coordinate(1), ORTHO%coordinate(2), ierr )
   !
   IF( ierr /= ORTHO%task ) CALL error("[SLK_ORTHO_group] wrong task coordinates in ortho group")
   !
   IF( ORTHO%task*ORTHO%leg /= me_all ) CALL error("[SLK_ORTHO_group] wrong rank assignment in ortho group")
   !
   CALL MPI_COMM_SPLIT( ORTHO%COMM, ORTHO%coordinate(2), ORTHO%coordinate(1), ORTHO%COL_comm, ierr )
   CALL MPI_COMM_SPLIT( ORTHO%COMM, ORTHO%coordinate(1), ORTHO%coordinate(2), ORTHO%ROW_comm, ierr )
   !
 else
   !
   ORTHO%COMM_id = 0
   ORTHO%coordinate(1) = ORTHO%task
   ORTHO%coordinate(2) = ORTHO%task
   !
 endif
 !
 !  This part is used to eliminate the image dependency from ortho groups
 !  SCALAPACK is now independent of whatever level of parallelization
 !  is present on top of pool parallelization
 !
 CALL MPI_COMM_SIZE(mpi_comm_world,total_nproc,ierr)
 CALL MPI_COMM_RANK(mpi_comm_world,total_mype,ierr)
 !
 nparent = total_nproc/n_pools/POOL%n_CPU
 nproc_parent = total_nproc/nparent
 my_parent_id = total_mype/nproc_parent
 !
 ALLOCATE( ortho_cntx_pe( n_pools, nparent ) )
 ALLOCATE( blacsmap( ORTHO%grid(1), ORTHO%grid(2) ) )
 !
 DO j = 1, nparent
   !
   DO i = 1, n_pools
     !
     CALL BLACS_GET( -1, 0, ortho_cntx_pe( i, j ) ) ! take a default value 
     !
     blacsmap = 0
     nprow = ORTHO%grid(1)
     npcol = ORTHO%grid(2)
     !
     IF( ( j == ( my_parent_id + 1 ) ) .and. &
         ( i == ( POOL%id  + 1 ) ) .and. ( ORTHO%COMM_id > 0 ) ) THEN
       !
       blacsmap( ORTHO%coordinate(1) + 1, ORTHO%coordinate(2) + 1 ) = BLACS_PNUM( ORTHO%world_cntx, 0, ORTHO%BLACS_id )
       !
     END IF
     !
     ! All MPI tasks defined in world comm take part in the definition of the BLACS grid
     !
     allocate( buff(size(blacsmap,1),size(blacsmap,2) ) )
     buff=blacsmap
     call MPI_ALLREDUCE( buff, blacsmap, size(blacsmap), MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
     deallocate( buff )
     !
     CALL BLACS_GRIDMAP( ortho_cntx_pe(i,j), blacsmap, nprow, nprow, npcol )
     !
     CALL BLACS_GRIDINFO( ortho_cntx_pe(i,j), nprow, npcol, myrow, mycol )
     !
     IF( ( j == ( my_parent_id + 1 ) ) .and. &
         ( i == ( POOL%id  + 1 ) ) .and. ( ORTHO%COMM_id > 0 ) ) THEN
       !
       IF(  ORTHO%grid(1) /= nprow ) CALL error('[SLK_ORTHO_group] problem with SCALAPACK, wrong no. of task rows')
       IF(  ORTHO%grid(2) /= npcol ) CALL error('[SLK_ORTHO_group] problem with SCALAPACK, wrong no. of task columns')
       IF(  ORTHO%coordinate(1) /= myrow ) CALL error('[SLK_ORTHO_group] problem with SCALAPACK, wrong task row ID')
       IF(  ORTHO%coordinate(2) /= mycol ) CALL error('[SLK_ORTHO_group] problem with SCALAPACK, wrong task columns ID')
       !
       ORTHO%ortho_cntx = ortho_cntx_pe(i,j)
       !
     END IF
     !
   END DO
   !
 END DO 
 !
 DEALLOCATE( blacsmap )
 DEALLOCATE( ortho_cntx_pe )
 !
 first = .false.
 !
 RETURN
 !
END SUBROUTINE SLK_ORTHO_group
