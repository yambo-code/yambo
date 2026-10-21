!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): CR AF DS
!
! Block-block ChASE. Eigenvectors stay row-distributed: only mycol==0
! ranks deposit local rows into V_blc (no gather). Residuals Allreduce scalars.
!
subroutine PARALLEL_HERMITIAN_chase(Mat, V_blc, N, E, neig)
 !
 use pars,           ONLY:SP,cZERO,rZERO
 use SLK_m,          ONLY:SLK_ORTHO
 use matrix,         ONLY:PAR_matrix
 use timing_m,       ONLY:timing
 use chase_diag
 !
 implicit none
 !
 integer,          intent(in)   :: N,neig
 type(PAR_matrix), intent(in)   :: Mat
 complex(SP),      intent(out)  :: V_blc(N,neig)
 real(SP),         intent(out)  :: E(neig)
 !
 integer   :: nbr,nrows,ncols,rows(2),nprow,npcol,myrow,mycol,nev,nex,ierr,deg,comm
 character :: mode, opt, qr, grid_major
 complex(SP), allocatable :: Vtemp(:,:)
 real(SP),    allocatable :: Etemp(:)
 !
 V_blc=cZERO
 E=rZERO
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
 call timing('PARALLEL_HERMITIAN_chase',OPR='start')
 !
 nev = min(neig,N)
#ifdef _DOUBLE
 deg = 20
#else
 deg = 10
#endif
 mode = 'R'
 opt = 'S'
 qr = 'C'
 grid_major = 'R'
 !
 nprow = SLK_ORTHO%grid(1)
 npcol = SLK_ORTHO%grid(2)
 myrow = SLK_ORTHO%coordinate(1)
 mycol = SLK_ORTHO%coordinate(2)
 nbr   = Mat%BLCnrows
 nrows = Mat%nrows
 ncols = Mat%ncols
 rows  = Mat%rows
 comm  = SLK_ORTHO%COMM
 nex   = min(100,nev)
 !
 allocate(Vtemp(nrows,nev+nex))
 allocate(Etemp(nev+nex))
 Vtemp = cZERO
 Etemp = rZERO
 !
 call timing('ChASE init',OPR='start')
#if defined _DOUBLE
 call pzchase_init( N, nev, nex, nrows, ncols, Mat%blc(:,:,Mat%I), nbr, &
 &                  Vtemp, Etemp, nprow, npcol, grid_major, comm, ierr)
#else
 call pcchase_init( N, nev, nex, nrows, ncols, Mat%blc(:,:,Mat%I), nbr, &
 &                  Vtemp, Etemp, nprow, npcol, grid_major, comm, ierr)
#endif
 call timing('ChASE init',OPR='stop')
 !
 if (ierr/=1) call warning("ChASE did not init properly")
 !
#if defined _DOUBLE
 call pzchase(deg,1.0e-10_SP,mode,opt,qr)
#else
 call pcchase(deg,1.0e-5_SP,mode,opt,qr)
#endif
 !
#if defined _DOUBLE
 call pzchase_finalize(ierr)
#else
 call pcchase_finalize(ierr)
#endif
 if (ierr/=0) call warning("ChASE did not finalize properly")
 !
 E(1:nev) = Etemp(1:nev)
 ! Row-distributed deposit (column-0 only; V is replicated across process columns).
 if (mycol==0 .and. nrows>0) V_blc(rows(1):rows(2),1:nev) = Vtemp(1:nrows,1:nev)
 !
 deallocate(Etemp)
 deallocate(Vtemp)
 call timing('PARALLEL_HERMITIAN_chase',OPR='stop')
 !
end subroutine PARALLEL_HERMITIAN_chase
