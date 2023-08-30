!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
! Copyright (C) 2004 WanT Group
!
! Authors (see AUTHORS file for details): AM AF
!
subroutine PARALLEL_HERMITIAN_diagonalization(M, V, N, E)
 !
 ! perform the diagonalization by using scalapack
 !
 use pars,           ONLY:SP
 use parallel_int,   ONLY:PP_redux_wait
 use SLK_m,          ONLY:SLK_POOL,SLK_ORTHO
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use matrix,         ONLY:PAR_matrix
 use timing_m,       ONLY:timing
#include<memory.h>
 !
 integer           :: N
 type(PAR_matrix)  :: M,V
 real(SP)          :: E(N)
 !
 character(64)     :: subname="PARALLEL_HERMITIAN_diagonalization"
 integer           :: lwork,lrwork
 real(SP)          :: rvar(1)
 complex(SP)       :: cvar(1)
 type(LALGEBRA_WS) :: WS
 !
 E=0.0_SP
 !
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
 call timing('PARALLEL_HERMITIAN_diagonalization',OPR='start')
 !
 lwork=-1
 lrwork=-1
 !
#if defined _DOUBLE
 call PZHEEV&
#else
 call PCHEEV&
#endif
&  ('V','U',N,M%blc(:,:,M%I),1,1,M%desc,E,V%blc(:,:,V%I),1,1,V%desc,cvar,lwork,rvar,lrwork,WS%i_fail)
 !
 ! AF: A better estimates should be used
 lwork=2*n**2
 lrwork=2*n + 2*n-2
 YAMBO_ALLOC(WS%v_cmplx,(lwork))
 YAMBO_ALLOC(WS%v_real,(lrwork))
 !
#if defined _DOUBLE
 call PZHEEV&
#else
 call PCHEEV&
#endif
& ('V','U',N,M%blc(:,:,M%I),1,1,M%desc,E,V%blc(:,:,V%I),1,1,V%desc,WS%v_cmplx,lwork,WS%v_real,lrwork,WS%i_fail)
 !
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing P(Z/C)HEEV')
 !
 if (SLK_ORTHO%task/=0) E=0.0_SP
 !
 call PP_redux_wait(E,COMM=SLK_POOL%INTRA_comm)
 !
 call LINEAR_ALGEBRA_WS_reset(WS) 
 !
 call timing('PARALLEL_HERMITIAN_diagonalization',OPR='stop')
 !
end subroutine PARALLEL_HERMITIAN_diagonalization

