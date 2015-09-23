subroutine SLK_test() 
 ! 
 ! simple program to invert a NxN matrix using scalapack 
 !
 use pars
 use parallel_m
 use com
 use SLK_m
 !
 implicit none
 !
 integer        :: iargc              ! function giving no of arguments
 integer        :: nargs
 character(256) :: str, str1 
 character(6)   :: subname="mytest"
 !
 integer        :: ndim, nprow, npcol
 integer        :: ivar
 real(DP),   allocatable :: Amat(:,:), Ainv(:,:)
 !
 integer        :: ind, ierr
 !
 type (POOL_group)  :: POOL
 type (ORTHO_group) :: ORTHO
 !
! ndim=0
! nprow=1
! npcol=1
! npool=1
 !
! ind=1
! do while (ind <= nargs)
!   !
!   call getarg(ind,str)
!   call getarg(ind+1,str1)
!   read(str1,*,iostat=ierr) ivar
!   if ( ierr /=0 ) call errore(subname,'reading arg',10)
!   !
!   select case ( trim(str) )
!   case ("-n", "--ndim") 
!     ndim=ivar
!   case ("-ng", "--ngrid") 
!     nprow=ivar
!     npcol=ivar
!   case ("-np", "--npool") 
!     npool=ivar
!   end select
!   !
!   ind=ind+2
!   !
! enddo 
! !
! np_ortho(1)=nprow
! np_ortho(2)=npcol
 !
 ! report
 !
 if ( master_cpu ) then
    write(0,"(/,2x,'INPUT report')")
    write(0,"(2x,'      ndim : ',i5)") ndim
    write(0,"(2x,'      ncpu : ',i5)") ncpu
    write(0,"(2x,' para grid : ',3x,i2,' x',i2)") nprow,npcol
    write(0,"(2x,'     npool : ',i5)") n_pools
    write(0,"()")
 endif
 !
! if ( nprow*npcol*npool /= ncpu ) CALL errore(subname,"invalid number of MPI tasks",10)
 !
 ! setup parallel structure
 !
 call parallel_structure()
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
 use parallel_m
 use SLK_m,    ONLY: POOL_group,ORTHO_group
 !
 implicit none
 !
 type(POOL_group)  :: POOL
 type(ORTHO_group) :: ORTHO
 !
 ! init pools
 call SLK_POOLS_init( mpi_comm_world, POOL )
 !
 ! init the scalapack grid
 call SLK_ORTHO_init( product(ORTHO%grid), POOL%INTRA_comm )
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
 !use kinds
 !use util_module
 !use para_module
 use pars
 use parallel_m
 use SLK_m,    ONLY: POOL_group
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
 if ( me_pool == root_pool ) then 
    write(0,"(5x,i2,' Amat Inv Eigenvalues')") POOL%id
    write(0,"(5x,i2,15f10.6)") my_pool_id, 1.0d0/w(1:ndim)
 endif
 !
 deallocate( w, zmat )
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine build_matrix
!
!===========================================
 subroutine serial_inverse( ndim, Amat, Ainv )
 !===========================================
 use kinds
 use util_module
 use para_module
 implicit none
 !
 integer   :: ndim
 real(DP) :: Amat(ndim,ndim)
 real(DP) :: Ainv(ndim,ndim)
 integer   :: ierr
 !
 real(DP), allocatable :: w(:), zmat(:,:)
 !
 call mat_inv( ndim, Amat, Ainv, IERR=ierr )
 if (ierr/=0 ) call errore('serial_inverse','inverting Amat',10)
 !
 ! compute and report eigenvalues of Ainv
 !
 allocate( w(ndim), zmat(ndim,ndim) )
 call mat_hdiag( zmat, w, Ainv, ndim )
 !
 if ( me_pool == root_pool ) then 
   write(0,"(5x,i2,' Ainv Eigenvalues')") my_pool_id
   write(0,"(5x,i2,15f10.6)") my_pool_id, w(ndim:1:-1)
 endif
 !
 deallocate( w, zmat )
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine serial_inverse
!
!===========================================
 subroutine para_inverse( ndim, Amat, Ainv )
 !===========================================
 !
 ! perform the inversion by using scalapack
 !
 use kinds
 use util_module
 use para_module, ONLY : ortho_cntx,np_ortho,me_ortho,MPI_DOUBLE_PRECISION,MPI_SUM,intra_pool_comm,root_pool,my_pool_id,me_pool,mpi_comm_world
 implicit none
 !
 integer, parameter :: dlen_ = 9
 !
 integer   :: ndim
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
 real(DP), allocatable :: Amat_loc(:,:), Ainv_loc(:,:)
 real(DP), allocatable :: buff(:,:)
 integer,   allocatable :: ipiv(:)
 integer,   allocatable :: iwork(:)
 real(DP), allocatable :: work(:)
 real(DP), allocatable :: w(:), zmat(:,:)
 !
 ! init global blacs grid
 !
 call BLACS_GRIDINFO( ortho_cntx, np_ortho(1), np_ortho(2), me_ortho(1), me_ortho(2) )
 !
 nprow=np_ortho(1)
 npcol=np_ortho(2)
 myrow=me_ortho(1)
 mycol=me_ortho(2)
 !
 ! spectator tasks
 if ( me_ortho(1) == -1 ) return   ! or do something else
 !
 ! distribute the matrix on the process grid
 ! Initialize the array descriptors for the matrices A and B
 !
 ndim_blc = int(ndim/nprow)
 if (ndim_blc*nprow < ndim ) ndim_blc=ndim_blc+1
 !
 lld = ndim_blc
 !
 call DESCINIT( descA, ndim, ndim, ndim_blc, ndim_blc, 0, 0, ortho_cntx, lld, info )
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
 if ( info /= 0 ) call errore('para_inverse','performing PDGETRF',10)
 !
 CALL PDGETRI( ndim, Amat_loc, 1, 1, descA, &
               ipiv, work, lwork, iwork, liwork, info )
 if ( info /= 0 ) call errore('para_inverse','performing PDGETRI',10)
 !
 ! gather the inverse matrix
 !
 Ainv=0.0d0
 Ainv(ils:ile,jls:jle)=Amat_loc(:,:)
 !
 allocate( buff(ndim,ndim) )
 !
 buff = Ainv
 call MPI_ALLREDUCE( buff, Ainv, ndim*ndim, MPI_DOUBLE_PRECISION, MPI_SUM, intra_pool_comm, info)
 if ( info /= 0 ) call errore('para_inverse','performing MPIALLGATHER',10)
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
 if ( me_pool == root_pool ) then
   write(0,"(5x,i2,' Ainv Eigenvalues')") my_pool_id
   write(0,"(5x,i2,15f10.6)") my_pool_id,w(ndim:1:-1)
 endif
 !
 deallocate( w, zmat )
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine para_inverse
!
!===========================================
 subroutine inverse_check( ndim, Amat, Ainv )
 !===========================================
 use kinds
 use util_module
 use para_module
 implicit none
 !
 integer   :: ndim
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
 !if ( lerror ) call errore('inverse_check','A * Ainv /= Id',10)
 !
 do i = 1, ndim
     if ( abs( zmat(i,i)-1.0d0 ) > toll ) then 
         lerror = .true.
         exit
     endif
 enddo
 !if ( lerror ) call errore('inverse_check','A * Ainv /= Id',11)
 !
 if ( .not. lerror ) then 
    if (me_pool==root_pool) write(0,"(5x,i2,' Inverse_check:   passed')") my_pool_id
 else
    if (me_pool==root_pool) write(0,"(5x,i2,' Inverse_check:   failed')") my_pool_id
 endif
 !
 deallocate( zmat )
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine inverse_check
