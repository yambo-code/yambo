subroutine SLK_test(E,k,q) 
 ! 
 ! simple program to invert a NxN matrix using scalapack 
 !
 use pars,          ONLY:SP
 use com,           ONLY:msg
 use SLK_m,         ONLY:POOL_group,ORTHO_group,SLK_test_H_dim,SLK_COM_A2A,SLK_COM_INDEX
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use interfaces,    ONLY:PARALLEL_global_indexes
 use linear_algebra,ONLY:LINEAR_ALGEBRA_driver,INV,USE_LK,USE_SLK
 !
 implicit none
 !
 type(levels)   ::E
 type(bz_samp)  ::k,q
 !
 ! Work Space
 !
 integer                    :: ndim,ierr
 complex(SP),   allocatable :: Amat(:,:), Ainv(:,:),Adiago(:,:)
 type (POOL_group)          :: POOL
 type (ORTHO_group)         :: ORTHO
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
 Ainv  =Amat
 Adiago=Amat
 call LINEAR_ALGEBRA_driver(INV,Ainv,lib_in=USE_LK)
 call section("=",'Check A*Ainv=Id')
 call inverse_check( ndim, Amat, Ainv )
 !
 ! parallel inversion
 !
 call section("=",'Parallel Inversion')
 Ainv  =Amat
 Adiago=Amat
 call SLK_communicators_init(INV,SLK_COM_A2A(1),SLK_COM_INDEX(1),0)
 call LINEAR_ALGEBRA_driver(INV,Ainv,lib_in=USE_SLK)
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
 use pars,       ONLY:SP,DP
 use parallel_m, ONLY:PP_redux_wait
 use SLK_m,      ONLY:SLK_COM_A2A
 use com,        ONLY:msg
 !
 implicit none
 !
 integer           :: ndim,iseed(8)
 complex(SP)       :: Amat(ndim,ndim)
 character(12)     :: ch(3)
 real(DP),external :: dlaran
 !
 integer   :: i, j, ierr
 real(SP) :: scal=0.2_SP 
 !
 call date_and_time(ch(1),ch(2),ch(3),iseed)
 iseed=iabs(iseed)
 !
 iseed(4)=2*(iseed(4)/2)+1
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
       Amat(i,j) = Amat(i,j) + scal * dlaran(iseed(4:))+(0,1.)*dlaran(iseed(4:))
       Amat(j,i) = conjg(Amat(i,j))
     enddo
   enddo
   !
 endif
 !
 call PP_redux_wait(Amat,COMM=SLK_COM_A2A(1)%COMM)
 !
end subroutine build_matrix
!
!==================================================
 subroutine inverse_check( ndim, Amat, Ainv )
 !==================================================
 use pars,       ONLY:SP
 use com,        ONLY:error,msg
 use parallel_m, ONLY:mpi_comm_world
 use wrapper,    ONLY:M_by_M
 implicit none
 !
 integer            :: ndim
 complex(SP) :: Amat(ndim,ndim)
 complex(SP) :: Ainv(ndim,ndim)
 !
 integer   :: i, j, ierr
 logical   :: lerror
 real(SP)  :: toll=1._SP
 complex(SP), allocatable :: zmat(:,:)
 !
 allocate( zmat(ndim,ndim) )
 !
 zmat=0._SP
 !
 call M_by_M( 'n','n', ndim,  Amat, Ainv, zmat)
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
