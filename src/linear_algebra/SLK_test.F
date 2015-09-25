subroutine SLK_test(E,k,q,PARENT_comm) 
 ! 
 ! simple program to invert a NxN matrix using scalapack 
 !
 use pars,          ONLY:DP
 use com,           ONLY:error
 use parallel_m,    ONLY:ncpu,MPI_comm
 use SLK_m,         ONLY:POOL_group,ORTHO_group,n_pools,SLK_test_H_dim
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use interfaces,    ONLY:PARALLEL_global_indexes
 !
 implicit none
 !
 type(levels)   ::E
 type(bz_samp)  ::k,q
 type(MPI_comm) :: PARENT_comm
 !
 ! Work Space
 !
 integer                 :: ndim
 real(DP),   allocatable :: Amat(:,:), Ainv(:,:)
 type (POOL_group)       :: POOL
 type (ORTHO_group)      :: ORTHO
 !
 if (ncpu/=8) return
 !
 call section("*","ScaLapacK test")
 !
 call PARALLEL_global_indexes(E,k,q,"ScaLapacK")
 stop
 !
#if !defined _DOUBLE
 call error("This test works only if the code is compiled in double precision")
#endif
 !
 ORTHO%grid=2
 n_pools   =2
 ndim = SLK_test_H_dim
 !
 ! setup parallel structure
 !
 call parallel_structure(PARENT_comm%COMM,POOL,ORTHO)
 !
 ! workspace
 !
 allocate( Amat(ndim,ndim) )
 allocate( Ainv(ndim,ndim) )
 !
 ! init
 !
 call section("+",'Matrix Building')
 call build_matrix( POOL, ndim, Amat )
 !
 ! serial inversion
 !
 call section("=",'Serial Inversion')
 call serial_inverse( POOL, ndim, Amat, Ainv )
 !
 call section("=",'Check A*Ainv=Id')
 call inverse_check( POOL, ndim, Amat, Ainv )
 !
 ! parallel inversion
 !
 call section("=",'Parallel Inversion')
 call para_inverse( POOL, ORTHO, ndim, Amat, Ainv )
 !
 call section("=",'Check A*Ainv=Id')
 call inverse_check( POOL, ndim, Amat, Ainv )
 !
 ! cleanup
 !
 deallocate( Amat, Ainv )
 !
end subroutine SLK_test 
!
!=============================================
subroutine parallel_structure(COMM,POOL,ORTHO)
 !=============================================
 !
 use parallel_m, ONLY:mpi_comm_world
 use SLK_m,      ONLY:POOL_group,ORTHO_group
 !
 implicit none
 !
 integer           :: COMM
 type(POOL_group)  :: POOL
 type(ORTHO_group) :: ORTHO
 !
 ! init pools
 call SLK_POOLS_init( POOL, COMM )
 !
 ! init the scalapack grid
 call SLK_ORTHO_init( ORTHO, POOL, product(ORTHO%grid), POOL%INTRA_comm )
 !
 return
 !
end subroutine parallel_structure
!
!===========================================
 subroutine build_matrix(POOL, ndim, Amat )
 !===========================================
 !
 ! build A = I + scal * randmat
 ! a small value of scal ensures A is invertible
 !
 use pars
 use util_module
 use parallel_m, ONLY:MPI_DOUBLE_PRECISION,mpi_comm_world
 use SLK_m,      ONLY:POOL_group,ndim
 use com,        ONLY:msg
 use ifport
 !
 implicit none
 !
 integer           :: ndim
 type(POOL_group)  :: POOL
 real(DP)          :: Amat(ndim,ndim)
 !
 integer   :: i, j, ierr
 real(DP) :: scal=0.2d0 
 real(DP), allocatable :: w(:), zmat(:,:)
 !
 call msg("r",'  Matrix Sized:',ndim)
 !
 if ( POOL%CPU_id == POOL%ROOT_id ) then
   !
   Amat = 0.0d0
   do i = 1, ndim
     Amat(i,i) = 1.0d0
   enddo
   !
   call srand( 192*POOL%id )
   !
   do j = 1, ndim
     do i = j, ndim
       ! 192 is just a random number
       Amat(i,j) = Amat(i,j) + scal * rand(0)
       Amat(j,i) = Amat(i,j)
     enddo
   enddo
   !
 endif
 !
 call MPI_bcast(Amat,ndim*ndim,MPI_DOUBLE_PRECISION,POOL%ROOT_id,POOL%INTRA_comm,ierr)
 !
 ! compute and report eigenvalues
 !
 allocate( w(ndim), zmat(ndim,ndim) )
 call mat_hdiag( zmat, w, Amat, ndim )
 !
 call msg("s",'  Amat Inv Eigenvalue@POOL:',POOL%id)
 call msg("s",' ',1.0d0/w(1:ndim))
 !
 deallocate( w, zmat )
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine build_matrix
!
!===================================================
 subroutine serial_inverse( POOL, ndim, Amat, Ainv )
 !===================================================
 !
 use util_module
 use com,          ONLY:error,msg
 use pars,         ONLY:DP
 use parallel_m,   ONLY:mpi_comm_world
 use SLK_m,        ONLY:POOL_group,ndim
 implicit none
 !
 type(POOL_group) :: POOL
 real(DP)         :: Amat(ndim,ndim)
 real(DP)         :: Ainv(ndim,ndim)
 integer          :: ierr,ndim
 !
 real(DP), allocatable :: w(:), zmat(:,:)
 !
 call mat_inv( ndim, Amat, Ainv, IERR=ierr )
 if (ierr/=0 ) call error('serial_inverse inverting Amat')
 !
 ! compute and report eigenvalues of Ainv
 !
 allocate( w(ndim), zmat(ndim,ndim) )
 call mat_hdiag( zmat, w, Ainv, ndim )
 !
 call msg("s",'  Ainv Inv Eigenvalue@POOL:',POOL%id)
 call msg("s",' ',w(ndim:1:-1))
 !
 deallocate( w, zmat )
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine serial_inverse
!
!========================================================
 subroutine para_inverse( POOL, ORTHO, ndim, Amat, Ainv )
 !========================================================
 !
 ! perform the inversion by using scalapack
 !
 !
 use util_module
 use com,        ONLY:error,msg
 use pars,       ONLY:DP
 use parallel_m, ONLY:mpi_comm_world,MPI_DOUBLE_PRECISION, MPI_SUM
 use SLK_m,      ONLY:POOL_group,ORTHO_group,ndim
 !
 implicit none
 !
 integer, parameter :: dlen_ = 9
 !
 integer           :: ndim
 type(POOL_group)  :: POOL
 type(ORTHO_group) :: ORTHO
 real(DP) :: Amat(ndim,ndim)
 real(DP) :: Ainv(ndim,ndim)
 !
 integer   :: descA(dlen_), descAinv(dlen_)
 !
 integer   :: nprow, npcol, myrow, mycol
 integer   :: ndim_blc, lld
 integer   :: info, ierr
 integer   :: lwork, liwork
 integer   :: ils, ile, jls, jle
 !
 real(DP), allocatable :: Amat_loc(:,:), Ainv_loc(:,:)
 real(DP), allocatable :: buff(:,:)
 integer,  allocatable :: ipiv(:)
 integer,  allocatable :: iwork(:)
 real(DP), allocatable :: work(:)
 real(DP), allocatable :: w(:), zmat(:,:)
 !
 ! init global blacs grid
 !
 call BLACS_GRIDINFO( ORTHO%ortho_cntx, ORTHO%grid(1), ORTHO%grid(2), ORTHO%coordinate(1), ORTHO%coordinate(2) )
 !
 nprow=ORTHO%grid(1)
 npcol=ORTHO%grid(2)
 myrow=ORTHO%coordinate(1)
 mycol=ORTHO%coordinate(2)
 !
 ! spectator tasks
 if ( ORTHO%coordinate(1) == -1 ) return   ! or do something else
 !
 ! distribute the matrix on the process grid
 ! Initialize the array descriptors for the matrices A and B
 !
 ndim_blc = int(ndim/nprow)
 if (ndim_blc*nprow < ndim ) ndim_blc=ndim_blc+1
 !
 lld = ndim_blc
 !
 call DESCINIT( descA, ndim, ndim, ndim_blc, ndim_blc, 0, 0, ORTHO%ortho_cntx, lld, info )
 !
 allocate( Amat_loc(ndim_blc,ndim_blc) )
 allocate( Ainv_loc(ndim_blc,ndim_blc) )
 allocate( ipiv(ndim+ndim_blc) )
 !
 ! LWORK  = LOCr(N+MOD(IA-1,MB_A))*NB_A
 ! LIWORK = LOCc( N_A + MOD(JA-1, NB_A) ) + NB_A
 lwork  = ndim_blc*ndim_blc
 liwork = ndim_blc+ndim_blc
 !
 allocate( work(lwork) )
 allocate( iwork(liwork) )
 !
 ! distribute the matrix A
 !
 ils=myrow*ndim_blc+1
 ile=min(myrow*ndim_blc+ndim_blc,ndim)
 jls=mycol*ndim_blc+1
 jle=min(mycol*ndim_blc+ndim_blc,ndim)
 !
 Amat_loc=0.0d0
 Amat_loc=Amat(ils:ile,jls:jle)
 !
 ! perform the inversion
 !
 CALL PDGETRF( ndim, ndim, Amat_loc, 1, 1, descA, ipiv, info )
 if ( info /= 0 ) call error('para_inverse performing PDGETRF')
 !
 CALL PDGETRI( ndim, Amat_loc, 1, 1, descA, &
               ipiv, work, lwork, iwork, liwork, info )
 if ( info /= 0 ) call error('para_inverse performing PDGETRI')
 !
 ! gather the inverse matrix
 !
 Ainv=0.0d0
 Ainv(ils:ile,jls:jle)=Amat_loc(:,:)
 !
 allocate( buff(ndim,ndim) )
 !
 buff = Ainv
 call MPI_ALLREDUCE( buff, Ainv, ndim*ndim, MPI_DOUBLE_PRECISION, MPI_SUM, POOL%INTRA_comm, info)
 if ( info /= 0 ) call error('para_inverse performing MPIALLGATHER')
 !
 deallocate(buff)
 ! 
 ! local cleanup 
 ! 
 deallocate( Amat_loc, Ainv_loc )
 deallocate( ipiv, work, iwork )
 !
 ! compute and report eigenvalues of Ainv
 !
 allocate( w(ndim), zmat(ndim,ndim) )
 call mat_hdiag( zmat, w, Ainv, ndim )
 !
 call msg("s",'  Ainv Inv Eigenvalue@POOL:',POOL%id)
 call msg("s",' ',w(ndim:1:-1))
 !
 deallocate( w, zmat )
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine para_inverse
!
!==================================================
 subroutine inverse_check( POOL, ndim, Amat, Ainv )
 !==================================================
 use pars,       ONLY:DP
 use util_module
 use com,        ONLY:error,msg
 use SLK_m,      ONLY:POOL_group,ndim
 use parallel_m, ONLY:mpi_comm_world
 implicit none
 !
 integer            :: ndim
 type (POOL_group)  :: POOL
 real(DP) :: Amat(ndim,ndim)
 real(DP) :: Ainv(ndim,ndim)
 !
 integer   :: i, j, ierr
 logical   :: lerror
 real(DP) :: toll=1.0d-10
 real(DP), allocatable :: zmat(:,:)
 !
 allocate( zmat(ndim,ndim) )
 !
 zmat=0._DP
 !
 call mat_mul( zmat, Amat, 'N', Ainv, 'N', ndim,ndim,ndim)
 !
 lerror = .false.
 outer_loop:&
 do j = 1, ndim
 do i = j+1, ndim
   if ( abs( zmat(i,j) ) > toll ) then 
     lerror = .true.
     exit outer_loop
   endif
 enddo
 enddo outer_loop
 if ( lerror ) call error('inverse_check   A * Ainv /= Id')
 !
 do i = 1, ndim
   if ( abs( zmat(i,i)-1.0d0 ) > toll ) then 
     lerror = .true.
     exit
   endif
 enddo
 if ( lerror ) call error('inverse_check   A * Ainv /= Id')
 !
 if ( .not. lerror ) call msg("sr",'  Inverse_check:   passed')
 if (       lerror ) call msg("sr",'  Inverse_check:   failed')
 !
 deallocate( zmat )
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine inverse_check
