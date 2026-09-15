!
! License-Identifier: GPL
!
! Copyright (C) 2020 the YAMBO team
!
! Authors (see AUTHORS file for details): AM, MG, DS
! 
subroutine K_diago_overlap_matrix(BS_V_left,BS_V_right,BS_overlap,l_partial_rows)
 !
 use pars,           ONLY:SP,cZERO,cONE
 use wrapper_omp,    ONLY:M_by_M_omp
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver 
 use LIVE_t,         ONLY:live_timing
 use linear_algebra, ONLY:INV
 use BS,             ONLY:BS_H_dim
 use BS_solvers,     ONLY:BSS_n_eig
 use parallel_m,     ONLY:ncpu
 use parallel_int,   ONLY:PP_redux_wait
 !
 implicit none
 !
 complex(SP),intent(in)  :: BS_V_left(BS_H_dim,BSS_n_eig),BS_V_right(BS_H_dim,BSS_n_eig)
 complex(SP),intent(out) :: BS_overlap(BSS_n_eig,BSS_n_eig)
 logical,    intent(in)  :: l_partial_rows
 !
 ! BS_overlap(i,j)=sum_k conjg(BS_V_left(k,i))*BS_V_right(k,j)
 ! If eigenvectors are row-distributed (zeros elsewhere), each rank contributes
 ! a partial gemm and we Allreduce before inversion.
 !
 BS_overlap=cZERO
 !
 call live_timing('BSK overlap mat',1)
 !
 call M_by_M_omp('c','n',BSS_n_eig,BSS_n_eig,BS_H_dim,cONE,BS_V_left,BS_H_dim,BS_V_right,BS_H_dim,cZERO,BS_overlap,BSS_n_eig)
 !
 if (l_partial_rows.and.ncpu>1) call PP_redux_wait(BS_overlap)
 !
 call LINEAR_ALGEBRA_driver(INV,M=BS_overlap)
 call live_timing(steps=1)
 call live_timing()
 !
end subroutine K_diago_overlap_matrix
