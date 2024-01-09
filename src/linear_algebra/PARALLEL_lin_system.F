!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AF AM
!
subroutine PARALLEL_lin_system(A,B,n,nrhs)
 !
 !  Solve the linear system
 !
 !                   A X = B
 !
 !  where A is a n-x-n matrix and B is n-x-nrhs
 !
 use SLK_m,          ONLY:SLK_ORTHO
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,LALGEBRA_WS,&
&                         LINEAR_ALGEBRA_WS_reset
 use matrix,         ONLY:PAR_matrix
 use timing_m,       ONLY:timing
#include<memory.h>
 !
 integer           :: n,nrhs
 type(PAR_matrix)  :: A, B
 !
 character(64)     :: subname="PARALLEL_lin_system"
 type(LALGEBRA_WS) :: WS
 !
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
 call timing('PARALLEL_lin_system',OPR='start')
 !
 ! better estimate of the dimensions to be included
 !
 YAMBO_FREE(WS%v_int)
 YAMBO_ALLOC(WS%v_int,(n+A%nrows))
 !
#if defined _DOUBLE
 CALL PZGESV&
#else
 CALL PCGESV&
#endif
& ( n, nrhs, A%blc(:,:,A%I), 1, 1, A%desc, WS%v_int, B%blc(:,:,B%I), 1, 1, B%desc, WS%i_fail)
 !
 if (WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing P(Z/C)GESV')
 !
 call LINEAR_ALGEBRA_WS_reset(WS) 
 ! 
 call timing('PARALLEL_lin_system',OPR='stop')
 !
end subroutine
