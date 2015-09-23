subroutine SLK_test() 
 ! 
 ! simple program to invert a NxN matrix using scalapack 
 !
 use pars,       ONLY:DP
 use parallel_m, ONLY:ncpu,master_cpu
 use SLK_m,      ONLY:POOL_group,ORTHO_group,n_pools
 !
 implicit none
 !
 integer        :: ndim
 real(DP),   allocatable :: Amat(:,:), Ainv(:,:)
 !
 integer        :: ind, ierr
 !
 type (POOL_group)  :: POOL
 type (ORTHO_group) :: ORTHO
 !
 if (ncpu/=8) return
 !
 call section("*","SCALAPACK test")
 !
 ORTHO%grid=2
 n_pools   =2
 ndim=10
 !
 ! setup parallel structure
 !
 call parallel_structure(POOL,ORTHO)
 !
 ! report
 !
 if ( master_cpu ) then
    write(0,"(/,2x,'INPUT report')")
    write(0,"(2x,'      ndim : ',i5)") ndim
    write(0,"(2x,'      ncpu : ',i5)") ncpu
    write(0,"(2x,' para grid : ',3x,i2,' x',i2)") ORTHO%grid
    write(0,"(2x,'     npool : ',i5)") n_pools
    write(0,"()")
 endif
 return
 !
 ! workspace
 !
 allocate( Amat(ndim,ndim) )
 allocate( Ainv(ndim,ndim) )
 !
 ! init
 !
 if (master_cpu) write(0,"(/,1x,'Matrix Building')") 
 call build_matrix( ndim, Amat )
 !
 ! serial inversion
 !
 if (master_cpu) write(0,"(/,1x,'Serial Inversion')") 
 call serial_inverse( ndim, Amat, Ainv )
 !
 if (master_cpu) write(0,"(/,2x,'Check A*Ainv=Id')") 
 call inverse_check( ndim, Amat, Ainv )
 !
 ! parallel inversion
 !
 if (master_cpu) write(0,"(/,1x,'Parallel Inversion')") 
 call para_inverse( ndim, Amat, Ainv )
 !
 if (master_cpu) write(0,"(/,2x,'Check A*Ainv=Id')") 
 call inverse_check( ndim, Amat, Ainv )
 !
 ! cleanup
 !
 deallocate( Amat, Ainv )
 !
 call MPI_FINALIZE(ierr)
  !
end subroutine SLK_test 
!
!===========================================
subroutine parallel_structure(POOL,ORTHO)
 !===========================================
 !
 use parallel_m, ONLY:mpi_comm_world
 use SLK_m,      ONLY:POOL_group,ORTHO_group
 !
 implicit none
 !
 type(POOL_group)  :: POOL
 type(ORTHO_group) :: ORTHO
 !
 ! init pools
 call SLK_POOLS_init( POOL, mpi_comm_world )
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
 use SLK_m,      ONLY: POOL_group
 use ifport
 !
 implicit none
 !
 type(POOL_group)  :: POOL
 integer   :: ndim
 real(DP) :: Amat(ndim,ndim)
 !
 integer   :: i, j, ierr
 real(DP) :: scal=0.2d0 
 real(DP), allocatable :: w(:), zmat(:,:)
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
 if ( POOL%CPU_id == POOL%ROOT_id ) then 
    write(0,"(5x,i2,' Amat Inv Eigenvalues')") POOL%id
    write(0,"(5x,i2,15f10.6)") POOL%ID, 1.0d0/w(1:ndim)
 endif
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
 use com,          ONLY:error
 use pars,         ONLY:DP
 use parallel_m,   ONLY:mpi_comm_world
 use SLK_m,        ONLY:POOL_group
 implicit none
 !
 integer   :: ndim
 type(POOL_group) :: POOL
 real(DP)         :: Amat(ndim,ndim)
 real(DP)         :: Ainv(ndim,ndim)
 integer          :: ierr
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
 if ( POOL%CPU_id == POOL%ROOT_id ) then 
   write(0,"(5x,i2,' Ainv Eigenvalues')") POOL%ID
   write(0,"(5x,i2,15f10.6)") POOL%ID, w(ndim:1:-1)
 endif
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
 use com,        ONLY:error
 use pars,       ONLY:DP
 use parallel_m, ONLY:mpi_comm_world,MPI_DOUBLE_PRECISION, MPI_SUM
 use SLK_m,      ONLY:POOL_group,ORTHO_group
 !
 implicit none
 !
 integer, parameter :: dlen_ = 9
 !
 type(POOL_group)  :: POOL
 type(ORTHO_group) :: ORTHO
 integer  :: ndim
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
 if ( POOL%CPU_id == POOL%ROOT_id ) then
   write(0,"(5x,i2,' Ainv Eigenvalues')") POOL%ID
   write(0,"(5x,i2,15f10.6)") POOL%ID,w(ndim:1:-1)
 endif
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
 use com,        ONLY:error
 use SLK_m,      ONLY:POOL_group
 use parallel_m, ONLY:mpi_comm_world
 implicit none
 !
 integer   :: ndim
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
 if ( .not. lerror ) then 
    if (POOL%CPU_id==POOL%ROOT_id) write(0,"(5x,i2,' Inverse_check:   passed')") POOL%ID
 else
    if (POOL%CPU_id==POOL%ROOT_id) write(0,"(5x,i2,' Inverse_check:   failed')") POOL%ID
 endif
 !
 deallocate( zmat )
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine inverse_check
