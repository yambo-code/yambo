!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AF
!
subroutine SERIAL_HERMITIAN_diagonalization(n,M,E)
 !
 use pars,           ONLY: SP
 use drivers,        ONLY: l_nl_optics,l_real_time
 use linear_algebra, ONLY: LINEAR_ALGEBRA_error,LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS,&
&                          magma_init_done,magma_setup,la_xheev
#ifdef _MAGMA
 use magma,          ONLY: magmaf_cheevd_m, magmaf_zheevd_m
#endif
 use timing_m,       ONLY: timing
 use com,            ONLY: msg
#ifdef _OPENMP
 use omp_lib
#endif
 !
#include<y_memory.h>
 !
 integer     :: n
 complex(SP) :: M(n,n)
 real(SP)    :: E(n)
 !
 character(64)     :: subname="SERIAL_HERMITIAN_diagonalization"
 integer           :: lwork, lrwork, liwork
 type(LALGEBRA_WS) :: WS
 !
 integer     :: nthreads_save

#if defined _DOUBLE
#  define magmaf_xheevd_m magmaf_zheevd_m
#else
#  define magmaf_xheevd_m magmaf_cheevd_m
#endif

 if(.not.(l_nl_optics.or.l_real_time)) call timing('SER_HERM_diago',OPR='start')
 !
 ! Thread safety
#if defined _OPENMP
 nthreads_save=omp_get_max_threads()
 call omp_set_num_threads(1)
#endif

 !
 ! Workspace
 !
 lwork=-1
 allocate(WS%v_cmplx(1))
 !
 lrwork=max(1,3*n-2)
#if defined _MAGMA
 lrwork=1
#endif
 !
 allocate(WS%v_real(lrwork))
 !
#if defined _MAGMA
 !
 lrwork=-1
 liwork=-1
 allocate(WS%v_int(1))
 !
 if (.not.magma_init_done) then
   call msg('sr','Initializing magma library in geev')
   call magma_setup()
 endif
 !   
 call magmaf_xheevd_m(1,'v','u', n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,&
 &                       lrwork,WS%v_int,liwork,WS%i_fail)
#else
 call la_xheev('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
#endif
 !
 if(WS%i_fail /=0 ) call LINEAR_ALGEBRA_error(' _XHEEV_ ','WS%v_cmplx size query failed')
 !
 lwork=nint(real(WS%v_cmplx(1)))
 deallocate(WS%v_cmplx)
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   YAMBO_ALLOC(WS%v_cmplx,(lwork))
 else
   allocate(WS%v_cmplx(lwork))
 endif
 !
#if defined _MAGMA
 !
 lrwork=nint(WS%v_real(1))
 deallocate(WS%v_real)
 liwork=WS%v_int(1)
 deallocate(WS%v_int)
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   YAMBO_ALLOC(WS%v_real,(lrwork))
   YAMBO_ALLOC(WS%v_int,(liwork))
 else
   allocate(WS%v_real(lrwork))
   allocate(WS%v_int(liwork))
 endif
 !   
 ! use magma (if available)
 !   
 ! 
 call magmaf_xheevd_m(1,'v','u',n, M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,&
&                     lrwork,WS%v_int,liwork,WS%i_fail)
 !
#else
 !
 call la_xheev('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
 !
#endif
 !
 if(WS%i_fail /= 0) call LINEAR_ALGEBRA_error(' XHEEV ','failed')
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 !
 ! Thread safety
#if defined _OPENMP
 call omp_set_num_threads(nthreads_save)
#endif
 !
 if(.not.(l_nl_optics.or.l_real_time)) call timing('SER_HERM_diago',OPR='stop')
 !
end subroutine

