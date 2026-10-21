!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): CR AF DS
!
! Parallel pseudo-Hermitian ChASE. Returns right eigenvectors only,
! row-distributed (mycol==0). Left vectors L=S R are built by the caller.
!
subroutine PARALLEL_PSEUDOHERM_chase(Mat, V_blc, N, E, neig)
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
 real(SP)  :: tol
 character :: mode, opt, qr, grid_major
 complex(SP), allocatable :: Vtemp(:,:)
 real(SP),    allocatable :: Etemp(:)
 !
 V_blc=cZERO
 E=rZERO
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
 call timing('PARALLEL_PSEUDOHERM_chase',OPR='start')
 !
 nev = min(neig,N)
#ifdef _DOUBLE
 deg = 20
 tol = 1.0e-8_SP
#else
 deg = 10
 tol = 1.0e-4_SP
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
 allocate(Vtemp(nrows,2*(nev+nex)))
 allocate(Etemp(2*(nev+nex)))
 Vtemp = cZERO
 Etemp = rZERO
 !
 call timing('ChASE_PS init',OPR='start')
#if defined _DOUBLE
 call pzchase_init_pseudo( N, nev, nex, nrows, ncols, Mat%blc(:,:,Mat%I), nbr, &
 &                         Vtemp, Etemp, nprow, npcol, grid_major, comm, ierr)
#else
 call pcchase_init_pseudo( N, nev, nex, nrows, ncols, Mat%blc(:,:,Mat%I), nbr, &
 &                         Vtemp, Etemp, nprow, npcol, grid_major, comm, ierr)
#endif
 call timing('ChASE_PS init',OPR='stop')
 !
 if (ierr/=1) call warning("ChASE_PS did not init properly")
 !
#if defined _DOUBLE
 call pzchase(deg,tol,mode,opt,qr)
#else
 call pcchase(deg,tol,mode,opt,qr)
#endif
 !
#if defined _DOUBLE
 call pzchase_finalize(ierr)
#else
 call pcchase_finalize(ierr)
#endif
 if (ierr/=0) call warning("ChASE_PS did not finalize properly")
 !
 E(1:nev) = Etemp(1:nev)
 if (mycol==0 .and. nrows>0) V_blc(rows(1):rows(2),1:nev) = Vtemp(1:nrows,1:nev)
 !
 deallocate(Etemp)
 deallocate(Vtemp)
 call timing('PARALLEL_PSEUDOHERM_chase',OPR='stop')
 !
end subroutine PARALLEL_PSEUDOHERM_chase
