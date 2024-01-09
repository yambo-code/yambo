!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SERIAL_diagonalization(n,M,E,V_left,V_right)
 !
 !  The right eigenvector V_right(j) of M satisfies
 !                   M * V_right(j) = E(j) * V_right(j)
 !  where E(j) is its eigenvalue.
 !
 !  The left eigenvector V_left(j) of M satisfies
 !                V_left(j)**H * M = E(j) * V_left(j)**H
 !  where V_left(j)**H denotes the conjugate transpose of V_left(j).
 !
 use pars,          ONLY:SP
 use linear_algebra,ONLY:LINEAR_ALGEBRA_error,M_eigenvalues,LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use timing_m,      ONLY:timing
#include<memory.h>
 !
 integer           :: n
 complex(SP)       :: M(n,n),E(n),V_left(n,n),V_right(n,n)
 type(LALGEBRA_WS) :: WS
 !
 allocate(WS%v_cmplx(1))
 YAMBO_ALLOC(WS%v_real,(2*n))
 call M_eigenvalues(n,M,E,V_left,V_right,WS%v_cmplx,-1,WS%v_real,WS%i_fail)
 !
 call timing('SERIAL_diagonalization',OPR='start')
 !
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error('M_eigenvalues (SD)','WS%v_cmplxspace failed')
 !
 WS%dim=int(real(WS%v_cmplx(1)))
 deallocate(WS%v_cmplx)
 YAMBO_ALLOC(WS%v_cmplx,(WS%dim))
 call M_eigenvalues(n,M,E,V_left,V_right,WS%v_cmplx,WS%dim,WS%v_real,WS%i_fail)
 !
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error('M_eigenvalues (SD)','failed')
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 !
 call timing('SERIAL_diagonalization',OPR='stop')
 !
end subroutine
