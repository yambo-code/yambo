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
 use pars,          ONLY: SP
 use linear_algebra,ONLY: LINEAR_ALGEBRA_error,LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS,&
&                         magma_init_done,magma_setup,have_magma
 use timing_m,      ONLY: timing
 use com,           ONLY: msg
#ifdef _MAGMA
 use magma,         ONLY: magmaf_cgeev_m,magmaf_zgeev_m,MagmaVec
#endif
#ifdef _OPENMP
 use omp_lib
#endif
 !
#include<y_memory.h>
 !
 integer           :: n
 complex(SP)       :: M(n,n),E(n),V_left(n,n),V_right(n,n)
 type(LALGEBRA_WS) :: WS
 !
 integer :: nthreads_save

 !
 ! name aliasing
 !
#if defined _DOUBLE
#  define _XGEEV_ ZGEEV
#  define _MAGMAF_XGEEV_ magmaf_ZGEEV_m
#else
#  define _XGEEV_ CGEEV
#  define _MAGMAF_XGEEV_ magmaf_CGEEV_m
#endif
 !
 call timing('SERIAL_diagonalization',OPR='start')
 !
 ! Thread safety
#if defined _OPENMP
 nthreads_save=omp_get_max_threads()
 call omp_set_num_threads(1)
#endif

 !
 ! Workspace
 !
 allocate(WS%v_cmplx(1))
 YAMBO_ALLOC(WS%v_real,(2*n))
 !
#if defined _MAGMA
   !
   if (.not.magma_init_done) then
     call msg('sr','Initializing magma library in geev')
     call magma_setup()
   endif
   !   
   call _MAGMAF_XGEEV_(MagmaVec,MagmaVec, n,M,n,E,V_left,n,V_right,n,&
&                      WS%v_cmplx,-1,WS%v_real,WS%i_fail)
#else
   call _XGEEV_('V','V',n,M,n,E,V_left,n,V_right,n,WS%v_cmplx,-1,WS%v_real,WS%i_fail)
#endif
 !
 if(WS%i_fail /=0 ) call LINEAR_ALGEBRA_error(' _XGEEV_ ','WS%v_cmplx size query failed')
 !
 WS%dim=int(real(WS%v_cmplx(1)))
 deallocate(WS%v_cmplx)
 YAMBO_ALLOC(WS%v_cmplx,(WS%dim))

 !   
 ! use magma (if available)
 !   
#if defined _MAGMA
 !   
 call msg('sr','SERIAL diagonalization with magma')
 !
 call _MAGMAF_XGEEV_(MagmaVec,MagmaVec, n,M,n,E,V_left,n,V_right,n,&
&                    WS%v_cmplx,WS%dim,WS%v_real,WS%i_fail)
 !   
#else
 !   
 ! use lapack (otherwise)
 !   
 call msg('sr','SERIAL diagonalization with lapack')
 !
 call _XGEEV_('V','V',n,M,n,E,V_left,n,V_right,n,WS%v_cmplx,WS%dim,WS%v_real,WS%i_fail)
 !   
#endif
 !
 if(WS%i_fail /= 0) call LINEAR_ALGEBRA_error(' _XGEEV_ ','failed')
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 !
 ! Thread safety
#if defined _OPENMP
 call omp_set_num_threads(nthreads_save)
#endif
 !   
 !
 call timing('SERIAL_diagonalization',OPR='stop')
 !
end subroutine
