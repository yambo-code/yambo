!
! Copyright (C) 2002-2004 PWSCF-FPMD-CP90 group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
MODULE mp_global
  !----------------------------------------------------------------------------
  !
#if defined (__SHMEM)
  USE shmem_include
#endif
  !
  USE kinds,    ONLY:SP,DP
  !
  IMPLICIT NONE 
#if defined _MPI
    include 'mpif.h'
#endif
  !
  SAVE
  !
  INTEGER :: mpime = 0  ! absolute processor index starting from 0
  INTEGER :: root  = 0  ! index of the absolute root processor
  INTEGER :: nproc = 1  ! absolute number of processor
  INTEGER :: nproc_file = 1  ! absolute number of processor written in the 
                             ! xml punch file
  INTEGER :: world_comm = 0  ! communicator of all processor
  INTEGER :: kunit = 1  ! granularity of k-point distribution
  !
  ! ... indeces ( all starting from 0 !!! )
  !
  INTEGER :: me_pool     = 0  ! index of the processor within a pool 
  INTEGER :: me_image    = 0  ! index of the processor within an image
  INTEGER :: root_pool   = 0  ! index of the root processor within a pool
  INTEGER :: root_image  = 0  ! index of the root processor within an image
  INTEGER :: my_pool_id  = 0  ! index of my pool
  INTEGER :: my_image_id = 0  ! index of my image
  INTEGER :: me_ortho(2) = 0  ! coordinates of the processors
  !
  INTEGER :: npool       = 1  ! number of "k-points"-pools
  INTEGER :: nimage      = 1  ! number of "path-images"-pools
  INTEGER :: nogrp       = 1  ! number of proc. in an orbital "task group" 
  INTEGER :: npgrp       = 1  ! number of proc. in a plane-wave "task group" 
  INTEGER :: nproc_pool  = 1  ! number of processor within a pool
  INTEGER :: nproc_pool_file  = 1  ! number of processor within a pool of
                              !   written in the xml punch file
  INTEGER :: nproc_image = 1  ! number of processor within an image
  INTEGER :: np_ortho(2) = 1  ! size of the processor grid used in ortho
  INTEGER :: leg_ortho   = 1  ! the distance in the father communicator
                              ! of two neighbour processors in ortho_comm
  INTEGER, ALLOCATABLE :: nolist(:) ! list of processors in my orbital task group 
  INTEGER, ALLOCATABLE :: nplist(:) ! list of processors in my plane wave task group 
  !
  ! ... communicators
  !
  INTEGER :: inter_pool_comm  = 0  ! inter pool communicator
  INTEGER :: intra_pool_comm  = 0  ! intra pool communicator
  INTEGER :: inter_image_comm = 0  ! inter image communicator
  INTEGER :: intra_image_comm = 0  ! intra image communicator  
  INTEGER :: pgrp_comm        = 0  ! plane-wave group communicator (task grouping)
  INTEGER :: ogrp_comm        = 0  ! orbital group communicarot (task grouping)
  INTEGER :: ortho_comm       = 0  ! communicator used for fast and memory saving ortho
  INTEGER :: ortho_comm_id    = 0  ! id of the ortho_comm
  !
  INTEGER :: local_type, local_type_complex
  !
  CONTAINS
     !
     !-----------------------------------------------------------------------
     SUBROUTINE mp_global_start( root_i, mpime_i, group_i, nproc_i )
       !-----------------------------------------------------------------------
       !
       IMPLICIT NONE
       !
       INTEGER, INTENT(IN) :: root_i, mpime_i, group_i, nproc_i
       !
       root             = root_i
       mpime            = mpime_i
       world_comm       = group_i
       nproc            = nproc_i
       nproc_pool       = nproc_i
       nproc_image      = nproc_i
       my_pool_id       = 0
       my_image_id      = 0
       me_pool          = mpime
       me_image         = mpime
       root_pool        = root
       root_image       = root
       inter_pool_comm  = group_i
       intra_pool_comm  = group_i
       inter_image_comm = group_i
       intra_image_comm = group_i
       ortho_comm       = group_i
       ALLOCATE( nolist( nproc_i ) )
       ALLOCATE( nplist( nproc_i ) )
       nolist = 0
       nplist = 0
       !
#if defined _MPI
       local_type = MPI_REAL
       if(SP==DP) local_type=MPI_DOUBLE_PRECISION
#endif
       !               
       RETURN
       !
     END SUBROUTINE mp_global_start
     !
     !-----------------------------------------------------------------------     
     SUBROUTINE mp_global_group_start( mep, myp, nprocp, num_of_pools )
       !-----------------------------------------------------------------------
       !
       IMPLICIT NONE
       !     
       INTEGER, INTENT(IN) :: mep, myp, nprocp, num_of_pools
       !
       me_pool    = mep
       my_pool_id = myp
       nproc_pool = nprocp
       npool      = num_of_pools
       !
       RETURN
       !
     END SUBROUTINE mp_global_group_start
     !
!
SUBROUTINE init_ortho_group()
   !
   USE mp, ONLY : mp_comm_free, mp_size, mp_rank
   !
   IMPLICIT NONE
    
   INTEGER :: nproc_try, comm_all
    
   LOGICAL, SAVE :: first = .true.
   INTEGER :: ierr, color, key, me_all, newid, nproc_all
   INTEGER :: i, nproc_ortho
    
#if defined _MPI

   comm_all  = mpi_comm_world

   local_type         = MPI_REAL
   local_type_complex = MPI_COMPLEX
   if(SP==DP) then
     local_type        =MPI_DOUBLE_PRECISION
     local_type_complex=MPI_DOUBLE_COMPLEX
   endif

   me_all    = mp_rank( comm_all )
   nproc_all = mp_size( comm_all )

! Try to use all CPU available

   nproc_try = nproc_all

   IF( nproc_try > nproc_all ) THEN
      CALL errore( " init_ortho_group ", " argument 1 out of range ", nproc_try )
   END IF

   IF( .NOT. first ) THEN
      !  
      !  free resources associated to the communicator
      !
      CALL mp_comm_free( ortho_comm )
      !
   END IF

   !  find the square closer (but lower) to nproc_try
   !
   CALL grid2d_dims( 'S', nproc_try, np_ortho(1), np_ortho(2) )
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
   !  initialize the communicator for the new group
   !
   CALL MPI_COMM_SPLIT( comm_all, color, key, ortho_comm, ierr )
   IF( ierr /= 0 ) &
      CALL errore( " init_ortho_group ", " error splitting communicator ", ierr )
   !
   !  Computes coordinates of the processors, in row maior order
   !
   newid       = mp_rank( ortho_comm )
   nproc_ortho = mp_size( ortho_comm )
   IF( color == 1 .AND. nproc_ortho /= np_ortho(1) * np_ortho(2) ) &
      CALL errore( " init_ortho_group ", " wrong number of proc in ortho_comm ", ierr )
   !
   IF( me_all == 0 .AND. newid /= 0 ) &
      CALL errore( " init_ortho_group ", " wrong root in ortho_comm ", ierr )
   !
   if( color == 1 ) then
      ortho_comm_id = 1
      CALL GRID2D_COORDS( 'R', newid, np_ortho(1), np_ortho(2), me_ortho(1), me_ortho(2) )
      CALL GRID2D_RANK( 'R', np_ortho(1), np_ortho(2), me_ortho(1), me_ortho(2), ierr )
      IF( ierr /= newid ) &
         CALL errore( " init_ortho_group ", " wrong coordinates in ortho_comm ", ierr )
      IF( newid*leg_ortho /= me_all ) &
         CALL errore( " init_ortho_group ", " wrong rank assignment in ortho_comm ", ierr )
   else
      ortho_comm_id = 0
      me_ortho(1) = newid
      me_ortho(2) = newid
   endif

#else

   ortho_comm_id = 1

#endif

   first = .false.
    
   RETURN
END SUBROUTINE init_ortho_group
     !
     !
END MODULE mp_global
