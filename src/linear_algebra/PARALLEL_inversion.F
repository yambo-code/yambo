!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, AF
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
subroutine PARALLEL_inversion(POOL, ORTHO, ndim, Amat, Ainv )
 !
 ! perform the inversion by using scalapack
 !
 use com,            ONLY:error
 use pars,           ONLY:DP
 use parallel_m,     ONLY:mpi_comm_world,MPI_DOUBLE_PRECISION, MPI_SUM
 use SLK_m,          ONLY:POOL_group,ORTHO_group
 use linear_algebra, ONLY:WS
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
 integer   :: descA(dlen_)
 !
 integer   :: nprow, npcol, myrow, mycol
 integer   :: ndim_blc, lld
 integer   :: info, ierr
 integer   :: lwork, liwork
 integer   :: ils, ile, jls, jle
 logical   :: do_SLK_job
 !
 real(DP), allocatable :: Amat_loc(:,:), Ainv_loc(:,:)
 real(DP), allocatable :: buff(:,:)
 integer,  allocatable :: ipiv(:)
 integer,  allocatable :: iwork(:)
 real(DP), allocatable :: work(:)
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
 do_SLK_job = (ORTHO%coordinate(1) /= -1)
 !
 if (do_SLK_job) then
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
 endif
 !
 ! gather the inverse matrix
 !
 Ainv=0.0d0
 if ( do_SLK_job ) then
   Ainv(ils:ile,jls:jle)=Amat_loc(:,:)
 endif
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
 if ( do_SLK_job ) then
   deallocate( Amat_loc, Ainv_loc )
   deallocate( ipiv, work, iwork )
 endif
 !
 call MPI_barrier( mpi_comm_world, ierr)
 return
 !
end subroutine PARALLEL_inversion
