!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AF AM
!
subroutine PARALLEL_M_by_M(A,B,C,transa,transb,m,n,k)
 !
 !  Perform the parallel matrix multiplication
 !
 !          A x B = C
 !
 !  where A(m,k), B(k,n), C(m,n)
 !
 use pars,           ONLY:cONE,cZERO
 use SLK_m,          ONLY:SLK_ORTHO
 use matrix,         ONLY:PAR_matrix
 use timing_m,       ONLY:timing
 implicit none
 !
 integer           :: n,m,k
 type(PAR_matrix)  :: A, B, C
 character         :: transa,transb
 !
 character(64) :: subname="PARALLEL_M_by_M"
 !
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
 call timing('PARALLEL_M_by_M',OPR='start')
 !
#if defined _DOUBLE
 CALL PZGEMM&
#else
 CALL PCGEMM&
#endif
& ( transa, transb, m, n, k, cONE, A%blc(:,:,A%I), 1, 1, A%desc, &
    B%blc(:,:,B%I), 1, 1, B%desc, cZERO, C%blc(:,:,C%I), 1, 1, C%desc)
 !
 call timing('PARALLEL_M_by_M',OPR='stop')
 !
end subroutine
