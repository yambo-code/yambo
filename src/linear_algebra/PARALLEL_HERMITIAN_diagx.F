!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
! Copyright (C) 2004 WanT Group
!
! Authors (see AUTHORS file for details): CR AF
!
subroutine PARALLEL_HERMITIAN_diagx(M, V, N, E, neig)
 !
 ! perform the diagonalization by using scalapack pheevcx
 ! and conputes only neig eigenpairs
 !
 use pars,           ONLY:SP
 use parallel_int,   ONLY:PP_redux_wait
 use SLK_m,          ONLY:SLK_POOL,SLK_ORTHO
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use matrix,         ONLY:PAR_matrix
 use timing_m,       ONLY:timing
 !
#include<y_memory.h>
 !
 integer           :: N,neig
 type(PAR_matrix)  :: M,V
 real(SP)          :: E(neig)
 !
 character(64)      :: subname="PARALLEL_HERMITIAN_diagx"
 integer            :: lwork,lrwork,liwork,neigsl,neigsV,info,nprow,npcol,ii
 integer            :: ivar(1)
 real(SP)           :: rvar(1)
 complex(SP)        :: cvar(1)
 type(LALGEBRA_WS)  :: WS
 real(SP), external :: PDLAMCH,PSLAMCH 
 real(SP)           :: abstol,orfac
 real(SP), allocatable :: Etemp(:),gap(:)
 integer,  allocatable :: iwork(:),ifail(:),iclustr(:)
 integer :: comm,ierr,myrank_slk_ortho_comm,slk_ortho_comm_size,nl
 !
 E=0.0_SP
 !
 nprow = SLK_ORTHO%grid(1)
 npcol = SLK_ORTHO%grid(2)
 orfac  = 0.001
 !
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
 call timing('PARALLEL_HERMITIAN_diagx',OPR='start')
 !
 allocate(Etemp(N))
 allocate(ifail(N))
 allocate(iclustr(2*nprow*npcol))
 allocate(gap(nprow*npcol))
 !
 lwork =-1
 lrwork=-1
 liwork=-1
 !
#if defined _DOUBLE
 abstol = PDLAMCH(SLK_ORTHO%ortho_cntx,'U')
 call PZHEEVx&
#else
 abstol = PSLAMCH(SLK_ORTHO%ortho_cntx,'U')
 call PCHEEVx&
#endif
&('V','I','U',N,M%blc(:,:,M%I),1,1,M%desc,1.0,1.0,1,neig,abstol,neigsl,neigsV,Etemp,&
&  orfac,V%blc(:,:,V%I),1,1,V%desc,cvar,lwork,rvar,lrwork,ivar,liwork,ifail,iclustr,gap,info)

 !
 lwork = nint(real(cvar(1)))
 !lrwork=2*n + 2*n-2
 lrwork = nint(real(rvar(1)))
 liwork = nint(real(ivar(1)))
 !
 YAMBO_ALLOC(WS%v_cmplx,(lwork))
 YAMBO_ALLOC(WS%v_real,(lrwork))
 YAMBO_ALLOC(iwork,(liwork))
 !
 Etemp = 0.0_SP
 V%blc(:,:,V%I) = 0.0
 !
#if defined _DOUBLE
 call PZHEEVx&
#else
 call PCHEEVx&
#endif
&('V','I','U',N,M%blc(:,:,M%I),1,1,M%desc,1.0,1.0,1,neig,abstol,neigsl,neigsV,Etemp&
&,orfac,V%blc(:,:,V%I),1,1,V%desc,WS%v_cmplx,lwork,WS%v_real,lrwork,iwork,liwork,ifail,iclustr,gap,info)
 !
 do ii = 1,neigsl
   E(ii) = Etemp(ii)
 enddo
 !
 if(info/=0) call LINEAR_ALGEBRA_error(subname,'performing P(Z/C)HEEVX')
 !
 if (SLK_ORTHO%task/=0) E=0.0_SP
 !
 call PP_redux_wait(E,COMM=SLK_POOL%INTRA_comm)
 !
 call LINEAR_ALGEBRA_WS_reset(WS) 
 YAMBO_FREE(iwork)
 !
 call timing('PARALLEL_HERMITIAN_diagx',OPR='stop')
 !
end subroutine PARALLEL_HERMITIAN_diagx
