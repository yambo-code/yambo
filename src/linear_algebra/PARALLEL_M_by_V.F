!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_M_by_V(transa,N,A,X,Y)
 !
 !  Perform the parallel matrix multiplication
 !
 !          Y = A X 
 !
 !  where A(n,m), X(i:i+m-1), Y(j:j+n-1)
 !
 use pars,           ONLY:SP,cONE,cZERO
 use wrapper_omp,    ONLY:M_by_V_omp
 use parallel_int,   ONLY:PP_redux_wait
 use SLK_m,          ONLY:SLK_POOL,SLK_ORTHO
 use matrix,         ONLY:PAR_matrix
 use timing_m,       ONLY:timing
 implicit none
 !
 integer           :: N
 type(PAR_matrix)  :: A
 character(1)      :: transa
 complex(SP)       :: X(N)
 complex(SP)       :: Y(N)
 !
 Y=cZERO
 !
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
 call timing('PARALLEL_M_by_V',OPR='start')
 !
 if (transa=="C") then
   call M_by_V_omp("n",A%nrows,A%ncols,cONE,conjg(A%blc(A%rows(1):A%rows(2),A%cols(1):A%cols(2),A%I)),&
&                  A%nrows,X(A%cols(1):A%cols(2)),1,cZERO,Y(A%rows(1):A%rows(2)),1)
 else if (transa=="c") then
   call M_by_V_omp("c",A%nrows,A%ncols,cONE,A%blc(A%rows(1):A%rows(2),A%cols(1):A%cols(2),A%I),&
&                  A%nrows,X(A%rows(1):A%rows(2)),1,cZERO,Y(A%cols(1):A%cols(2)),1)
 else
   call M_by_V_omp("n",A%nrows,A%ncols,cONE,A%blc(A%rows(1):A%rows(2),A%cols(1):A%cols(2),A%I),&
&                  A%nrows,X(A%cols(1):A%cols(2)),1,cZERO,Y(A%rows(1):A%rows(2)),1)
 endif
 !
 call PP_redux_wait(Y,COMM=SLK_POOL%INTRA_comm)
 ! 
 call timing('PARALLEL_M_by_V',OPR='stop')
 !
end subroutine
