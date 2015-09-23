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
SUBROUTINE SLK_ORTHO_init( nproc_ortho_in, parent_comm )
 !---------------------------------------------------------------------------
 !
 ! ... Ortho group initialization
 !
 use para_module, ONLY:me_blacs,np_blacs,world_cntx
 !
 IMPLICIT NONE
 !
 INTEGER, INTENT(IN) :: nproc_ortho_in! read from command-line, 0 if unset
 INTEGER, INTENT(IN) :: parent_comm   ! communicator of the parent group
 !
 INTEGER :: nproc_ortho_try
 INTEGER :: parent_nproc  ! nproc of the parent group
 INTEGER :: ierr = 0
 !
 !parent_nproc = mp_size( parent_comm )
 CALL MPI_COMM_SIZE(parent_comm,parent_nproc,ierr)
 !
#if defined __SCALAPACK

 ! define a 1D grid containing all MPI task of MPI_COMM_WORLD communicator
 !
 CALL BLACS_PINFO( me_blacs, np_blacs )
 CALL BLACS_GET( -1, 0, world_cntx )
 CALL BLACS_GRIDINIT( world_cntx, 'Row', 1, np_blacs )
 !
#endif
 !
 IF( nproc_ortho_in > 0 ) THEN
   ! command-line argument -ndiag N or -nproc N set to N
   ! use the command line value ensuring that it falls in the proper range
   nproc_ortho_try = MIN( nproc_ortho_in , parent_nproc )
 ELSE 
   ! no command-line argument -ndiag N or -nproc N is present
   ! insert here custom architecture specific default definitions
#if defined __SCALAPACK
   nproc_ortho_try = MAX( parent_nproc/2, 1 )
#else
   nproc_ortho_try = 1
#endif
 END IF
 !
 ! the ortho group for parallel linear algebra is a sub-group of the pool,
 ! then there are as many ortho groups as pools.
 !
 CALL SLK_ORTHO_group( nproc_ortho_try, parent_comm )
 !  
 RETURN
 !
END SUBROUTINE SLK_ORTHO_init
