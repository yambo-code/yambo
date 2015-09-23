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
SUBROUTINE SLK_ORTHO_group( ORTHO, nproc_try_in, comm_all )
 !
 use para_module, ONLY:ortho_comm,ortho_comm_id,ortho_cntx,np_ortho,nproc_ortho,leg_ortho,me_ortho,ortho_col_comm,ortho_row_comm,&
&                      me_ortho1,mpi_comm_world,npool,my_pool_id,world_cntx,me_blacs,nproc_pool,MPI_INTEGER,MPI_SUM
 !
 IMPLICIT NONE
 !
 INTEGER, INTENT(IN) :: nproc_try_in, comm_all
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
   !  free resources associated to the communicator
   !
   !CALL mp_comm_free( ortho_comm )
   CALL MPI_comm_free( ortho_comm, ierr )
   !
   IF(  ortho_comm_id > 0  ) THEN
      CALL BLACS_GRIDEXIT( ortho_cntx )
   ENDIF
   ortho_cntx = -1
   !
 END IF
 !
 !  find the square closer (but lower) to nproc_try
 !
 CALL grid2d_dims( 'S', nproc_try, np_ortho(1), np_ortho(2) )
 !
 !  now, and only now, it is possible to define the number of tasks
 !  in the ortho group for parallel linear algebra
 !
 nproc_ortho = np_ortho(1) * np_ortho(2)
 !
 IF( nproc_all >= 4*nproc_ortho ) THEN
   !
   !  here we choose a processor every 4, in order not to stress memory BW
   !  on multi core procs, for which further performance enhancements are
   !  possible using OpenMP BLAS inside regter/cegter/rdiaghg/cdiaghg
   !  (to be implemented)
   !
   color = 0
   IF( me_all < 4*nproc_ortho .AND. MOD( me_all, 4 ) == 0 ) color = 1
   !
   leg_ortho = 4
   !
 ELSE IF( nproc_all >= 2*nproc_ortho ) THEN
   !
   !  here we choose a processor every 2, in order not to stress memory BW
   !
   color = 0
   IF( me_all < 2*nproc_ortho .AND. MOD( me_all, 2 ) == 0 ) color = 1
   !
   leg_ortho = 2
   !
 ELSE
   !
   !  here we choose the first processors
   !
   color = 0
   IF( me_all < nproc_ortho ) color = 1
   !
   leg_ortho = 1
   !
 END IF
 !
 key   = me_all
 !
 !  initialize the communicator for the new group by splitting the input communicator
 !
 CALL MPI_COMM_SPLIT( comm_all, color, key, ortho_comm, ierr )
 !
 IF( ierr /= 0 ) &
      CALL errore( " SLK_ORTHO_group ", " initializing ortho group communicator ", ierr )
 !
 !  Computes coordinates of the processors, in row maior order
 !
 CALL MPI_COMM_RANK( ortho_comm, me_ortho1, ierr)
 !
 IF( me_all == 0 .AND. me_ortho1 /= 0 ) &
      CALL errore( " SLK_ORTHO_group ", " wrong root task in ortho group ", ierr )
 !
 if( color == 1 ) then
   !
   ortho_comm_id = 1
   !
   CALL GRID2D_COORDS( 'R', me_ortho1, np_ortho(1), np_ortho(2), me_ortho(1), me_ortho(2) )
   CALL GRID2D_RANK( 'R', np_ortho(1), np_ortho(2), me_ortho(1), me_ortho(2), ierr )
   !
   IF( ierr /= me_ortho1 ) &
        CALL errore( " SLK_ORTHO_group ", " wrong task coordinates in ortho group ", ierr )
   !
   IF( me_ortho1*leg_ortho /= me_all ) &
        CALL errore( " SLK_ORTHO_group ", " wrong rank assignment in ortho group ", ierr )
   !
   CALL MPI_COMM_SPLIT( ortho_comm, me_ortho(2), me_ortho(1), ortho_col_comm, ierr )
   CALL MPI_COMM_SPLIT( ortho_comm, me_ortho(1), me_ortho(2), ortho_row_comm, ierr )
   !
 else
   !
   ortho_comm_id = 0
   me_ortho(1) = me_ortho1
   me_ortho(2) = me_ortho1
   !
 endif
 !
 !  This part is used to eliminate the image dependency from ortho groups
 !  SCALAPACK is now independent of whatever level of parallelization
 !  is present on top of pool parallelization
 !
 !total_nproc = mp_size(mpi_comm_world)
 !total_mype = mp_rank(mpi_comm_world)
 CALL MPI_COMM_SIZE(mpi_comm_world,total_nproc,ierr)
 CALL MPI_COMM_RANK(mpi_comm_world,total_mype,ierr)
 !
 nparent = total_nproc/npool/nproc_pool
 nproc_parent = total_nproc/nparent
 my_parent_id = total_mype/nproc_parent
 !
 ALLOCATE( ortho_cntx_pe( npool, nparent ) )
 ALLOCATE( blacsmap( np_ortho(1), np_ortho(2) ) )
 !
 DO j = 1, nparent
   !
   DO i = 1, npool
     !
     CALL BLACS_GET( -1, 0, ortho_cntx_pe( i, j ) ) ! take a default value 
     !
     blacsmap = 0
     nprow = np_ortho(1)
     npcol = np_ortho(2)
     !
     IF( ( j == ( my_parent_id + 1 ) ) .and. &
         ( i == ( my_pool_id  + 1 ) ) .and. ( ortho_comm_id > 0 ) ) THEN
       !
       blacsmap( me_ortho(1) + 1, me_ortho(2) + 1 ) = BLACS_PNUM( world_cntx, 0, me_blacs )
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
         ( i == ( my_pool_id  + 1 ) ) .and. ( ortho_comm_id > 0 ) ) THEN
       !
       IF(  np_ortho(1) /= nprow ) &
          CALL errore( ' SLK_ORTHO_group ', ' problem with SCALAPACK, wrong no. of task rows ', 1 )
       IF(  np_ortho(2) /= npcol ) &
          CALL errore( ' SLK_ORTHO_group ', ' problem with SCALAPACK, wrong no. of task columns ', 1 )
       IF(  me_ortho(1) /= myrow ) &
          CALL errore( ' SLK_ORTHO_group ', ' problem with SCALAPACK, wrong task row ID ', 1 )
       IF(  me_ortho(2) /= mycol ) &
          CALL errore( ' SLK_ORTHO_group ', ' problem with SCALAPACK, wrong task columns ID ', 1 )
       !
       ortho_cntx = ortho_cntx_pe(i,j)
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
