!
!        Copyright (C) 2000-2018 the YAMBO team
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
SUBROUTINE SLK_ORTHO_init( ORTHO, POOL, n_CPU_in )
 !
 ! ... Ortho group initialization
 !
 use SLK_m,  ONLY:ORTHO_group,POOL_group
 !
 IMPLICIT NONE
 !
 INTEGER,           INTENT(IN)    :: n_CPU_in      ! read from command-line, 0 if unset
 TYPE(ORTHO_group), INTENT(INOUT) :: ORTHO 
 TYPE(POOL_group) , INTENT(INOUT) :: POOL 
 !
 INTEGER :: nproc_ortho_try
 INTEGER :: parent_nproc  ! nproc of the parent group
 INTEGER :: ierr = 0
 !
 CALL MPI_COMM_SIZE(POOL%INTRA_comm,parent_nproc,ierr)
 !
 ! define a 1D grid containing all MPI task of MPI_COMM_WORLD communicator
 !
 CALL BLACS_PINFO( ORTHO%BLACS_id, ORTHO%BLACS_np )
 CALL BLACS_GET( -1, 0, ORTHO%world_cntx )
 CALL BLACS_GRIDINIT( ORTHO%world_cntx, 'Row', 1, ORTHO%BLACS_np )
 !
 IF( n_CPU_in > 0 ) THEN
   ! use the input value ensuring that it falls in the proper range
   nproc_ortho_try = MIN( n_CPU_in , parent_nproc )
 ELSE 
   ! no input value.
   ! Insert here custom architecture specific default definitions
   nproc_ortho_try = MAX( parent_nproc/2, 1 )
 END IF
 !
 ! the ortho group for parallel linear algebra is a sub-group of the pool,
 ! then there are as many ortho groups as pools.
 !
 CALL SLK_ORTHO_group( ORTHO, POOL, nproc_ortho_try, POOL%INTRA_comm )
 !  
 RETURN
 !
END SUBROUTINE SLK_ORTHO_init
