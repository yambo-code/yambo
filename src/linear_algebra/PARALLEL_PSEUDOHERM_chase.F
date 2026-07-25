!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): CR AF
!
subroutine PARALLEL_PSEUDOHERM_chase(Mat, V_left, V_right, N, E, neig)
 !
 use pars,           ONLY:SP
 use parallel_int,   ONLY:PP_redux_wait
 use SLK_m,          ONLY:SLK_POOL,SLK_ORTHO
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use parallel_m,     ONLY:PAR_COM_WORLD
 use matrix,         ONLY:PAR_matrix
 use timing_m,       ONLY:timing
 use chase_diag
 !
#include<y_memory.h>
 !
 integer           :: N,neig
 type(PAR_matrix)  :: Mat,V_left,V_right
 real(SP)          :: E(neig)

 integer   :: i,j,k,is,ie,nr,nb,nprow,npcol,m,myrow,mycol,nl
 integer   :: nev, nex, ierr, deg
 integer   :: comm
 real(SP)  :: tol
 character :: mode, opt, qr, grid_major
 !
 character(64)     :: subname="PARALLEL_PSEUDOHERM_chase"
 complex(SP), allocatable :: Vtemp(:,:)
 real(SP),    allocatable :: Etemp(:)
 !
 integer :: myrank_intra, myrank_inter, intra_size, inter_size,&
&           myrank_slk_ortho_comm, slk_ortho_comm_size

 call timing('PARALLEL_PSEUDOHERM_chase',OPR='start')
 !
 ! 
 E=0.0
 !
 ! AF: Warning if neig too large (eg > 25-30% of n)
 ! AF: neig is here assumed to be the number of positive eigs
 !
 nev = min(neig,N)
 nex = min(100,nev)
 !
#ifdef _DOUBLE
 deg = 20
 tol = 1e-8
#else
 deg = 10
 tol = 1e-4
#endif
 !
 mode = 'R'
 opt = 'S'
 qr = 'C'
 grid_major = 'R' !----- Check grid major /!\. -- Checked in SLK_ORTHO_init
 !
 nprow = SLK_ORTHO%grid(1)
 npcol = SLK_ORTHO%grid(2)
 myrow = SLK_ORTHO%coordinate(1)
 mycol = SLK_ORTHO%coordinate(2)
 nb    = Mat%BLCnrows
 m     = Mat%nrows
 nl    = Mat%ncols
 comm  = SLK_ORTHO%COMM
 !
 YAMBO_ALLOC(Vtemp,(m,nev+nex))
 YAMBO_ALLOC(Etemp,(nev+nex))
 !
 Vtemp = 0.0
 Etemp = 0.0
 ! 
 call mpi_comm_rank(comm,myrank_slk_ortho_comm,ierr)
 call mpi_comm_size(comm,slk_ortho_comm_size  ,ierr)
 call mpi_barrier(comm,ierr)
 !
 call timing('ChASE_INIT',OPR='start')
 !
#if defined _DOUBLE
 call pzchase_init_pseudo(N, nev, nex, nb, nb, Mat%blc(:,:,Mat%I), m, Vtemp, Etemp,&
&                         nprow, npcol, grid_major, comm, init=ierr)
#else
 call pcchase_init_pseudo(N, nev, nex, nb, nb, Mat%blc(:,:,Mat%I), m, Vtemp, Etemp,&
&                         nprow, npcol, grid_major, comm, init=ierr)
#endif
 !
 call timing('ChASE_INIT',OPR='stop')
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
 !
 if (ierr/=0) call warning("[WARNING] ChASE did not finalize properly")
 !
 call timing('ChASE_Reorganize',OPR='start')
 !
 E(1:nev) = Etemp(1:nev)
 !
 ! Left eigenvectors: L = S R with S=diag(I,-I) for BSE pseudo-Hermitian.
 ! If R=(X,Y), then L=(X,-Y). Must copy the upper half as well.
 !
 if (nev > nl * mycol) then
   !
   is=N/2+1
   ie=N
   !
   do i = 1,min(n,nev-n*mycol)
     !
     V_right%blc(:,i,V_right%I)   =  Vtemp(:, nl * mycol + i)
     !
     V_left%blc(1:N/2,i,V_left%I) =  Vtemp(1:N/2, nl * mycol + i)
     V_left%blc(is:ie,i,V_left%I) = -Vtemp(is:ie, nl * mycol + i)
     !
   enddo
 endif
 ! 
 call timing('ChASE_Reorganize',OPR='stop')
 !
 !call PP_redux_wait(E,COMM=SLK_POOL%INTRA_comm)
 !
 YAMBO_FREE(Etemp)
 YAMBO_FREE(Vtemp)
 !
 call timing('PARALLEL_PSEUDOHERM_chase',OPR='stop')
 !
end subroutine PARALLEL_PSEUDOHERM_chase
