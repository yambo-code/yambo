subroutine SLK_test(E,k,q) 
 ! 
 ! simple program to invert a NxN matrix using scalapack 
 !
 use pars,          ONLY:DP,SP
 use com,           ONLY:msg
 use SLK_m,         ONLY:POOL_group,ORTHO_group,n_pools,SLK_test_H_dim,SLK_COM_A2A,SLK_COM_INDEX
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use interfaces,    ONLY:PARALLEL_global_indexes
 !
 implicit none
 !
 type(levels)   ::E
 type(bz_samp)  ::k,q
 !
 ! Work Space
 !
 integer                 :: ndim,ierr
 real(DP),   allocatable :: Amat(:,:), Ainv(:,:),Adiago(:,:)
 type (POOL_group)       :: POOL
 type (ORTHO_group)      :: ORTHO
 !
 call section("*","ScaLapacK test")
 !
 call PARALLEL_global_indexes(E,k,q,"ScaLapacK")
 !
 ndim    = SLK_test_H_dim
 !
 ! workspace
 !
 allocate( Amat(ndim,ndim) )
 allocate( Ainv(ndim,ndim) )
 allocate( Adiago(ndim,ndim) )
 !
 ! init
 !
 call section("+",'Matrix Building')
 call build_matrix( ndim, Amat )
 !
 ! serial inversion
 !
 call section("=",'Serial Inversion')
 call serial_inverse( ndim, Amat, Ainv )
 !
 ! SLK setup
 !
 call SLK_setup(ORTHO, POOL,SLK_COM_INDEX(1),SLK_COM_A2A(1))
 !
 call msg("r",'  Pools        :',n_pools)
 call msg("r",'  BLACS grid   :',ORTHO%grid)
 call msg("r",'  Matrix Size  :',ndim)
 !
 call section("=",'Check A*Ainv=Id')
 call inverse_check( ndim, Amat, Ainv )
 !
 ! parallel inversion
 !
 call section("=",'Parallel Inversion')
 call PARALLEL_inversion( POOL, ORTHO, ndim, Amat, Ainv )
 !
 call section("=",'Check A*Ainv=Id')
 call inverse_check( ndim, Amat, Ainv )
 !
 ! cleanup
 !
 deallocate( Amat, Ainv )
 !
end subroutine SLK_test 
!
!===========================================
 subroutine build_matrix(ndim, Amat )
 !===========================================
 !
 ! build A = I + scal * randmat
 ! a small value of scal ensures A is invertible
 !
 use pars
 use util_module
 use parallel_m, ONLY:MPI_DOUBLE_PRECISION,mpi_comm_world
 use SLK_m,      ONLY:SLK_COM_A2A
 use com,        ONLY:msg
 !
 implicit none
 !
 integer           :: ndim,iseed(8)
 real(DP)          :: Amat(ndim,ndim)
 character(12)     :: ch(3)
 real(DP),external :: dlaran
 !
 integer   :: i, j, ierr
 real(DP) :: scal=0.2_SP 
 real(DP), allocatable :: w(:), zmat(:,:)
 !
 if ( SLK_COM_A2A(1)%CPU_id == 0 ) then
   !
   Amat = 0.0_SP
   do i = 1, ndim
     Amat(i,i) = 1.0_SP
   enddo
   !
   do j = 1, ndim
     do i = j, ndim
       ! 192 is just a random number
       Amat(i,j) = Amat(i,j) + scal * dlaran(iseed(4:))
       Amat(j,i) = Amat(i,j)
     enddo
   enddo
   !
 endif
 !
 call MPI_bcast(Amat,ndim*ndim,MPI_DOUBLE_PRECISION,0,SLK_COM_A2A(1)%COMM,ierr)
 !
 ! compute and report eigenvalues
 !
 allocate( w(ndim), zmat(ndim,ndim) )
 call mat_hdiag( zmat, w, Amat, ndim )
 !
 deallocate( w, zmat )
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine build_matrix
!
!===================================================
 subroutine serial_inverse( ndim, Amat, Ainv )
 !===================================================
 !
 use util_module
 use com,          ONLY:error,msg
 use pars,         ONLY:DP,SP
 use parallel_m,   ONLY:mpi_comm_world
 implicit none
 !
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
 deallocate( w, zmat )
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine serial_inverse
!
!==================================================
 subroutine inverse_check( ndim, Amat, Ainv )
 !==================================================
 use pars,       ONLY:DP,SP
 use util_module
 use com,        ONLY:error,msg
 use parallel_m, ONLY:mpi_comm_world
 implicit none
 !
 integer            :: ndim
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
   if ( abs( zmat(i,i)-1.0_SP ) > toll ) then 
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
