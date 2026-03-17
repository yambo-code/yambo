!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AF
!
subroutine SLK_setup( idriver, ncpu_in_use, ORTHO, POOL, COMM, INDEX_comm, slk_error)
 !
 use SLK_m,          ONLY:n_pools,ORTHO_group,POOL_group,SLK_POOL_init
 use parallel_m,     ONLY:yMPI_comm
 use linear_algebra, ONLY:INV,DIAGO,DIAGX,LIN_SYS,MAT_MUL
 use com,            ONLY:msg
 !
 IMPLICIT NONE
 !
 integer,           intent(in)    :: idriver
 integer,           intent(in)    :: ncpu_in_use
 type(yMPI_comm),    intent(in)    :: COMM , INDEX_comm
 type(ORTHO_group), intent(inout) :: ORTHO 
 type(POOL_group) , intent(inout) :: POOL 
 integer,           intent(inout) :: slk_error 
 !
 ! Work Space
 !
 character(20) :: ch
 !
 slk_error=0
 !
 ! # of POOLS
 !
 n_pools = INDEX_comm%n_CPU
 !
 ! Init pools
 !
 call SLK_POOL_init(POOL,COMM)
 !
 ! Init the scalapack grid
 !
 call SLK_ORTHO_init( ORTHO, POOL, min(POOL%n_CPU,ncpu_in_use) )
 ! 
 if (ORTHO%grid(1)<=1) then 
   slk_error=-1
   return
 endif
 !
 if (idriver==INV)     ch="Inversion"
 if (idriver==DIAGO)   ch="Digonalization"
 if (idriver==DIAGX)   ch="Few Eigenvalues"
 if (idriver==LIN_SYS) ch="LinearSystem"
 if (idriver==MAT_MUL) ch="MatrixMult."
 !
 call msg("lr" ,'[SLK-'//trim(ch)//']  Pools        ',n_pools)
 call msg("lrn",'[SLK-'//trim(ch)//']  BLACS grid   ',ORTHO%grid)
 !
end subroutine SLK_setup
