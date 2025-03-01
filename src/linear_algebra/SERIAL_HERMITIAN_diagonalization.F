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
&                          magma_init_done,magma_setup
 use timing_m,       ONLY: timing
 use com,            ONLY: msg 
#ifdef _MAGMA
 use magma,          ONLY: magmaf_cheevd_m,magmaf_zheevd_m,MagmaVec
#endif
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

 !
 ! name aliasing
 !
#if defined _DOUBLE
#  define _XHEEV_ ZHEEV
#  define _MAGMAF_XHEEV_ magmaf_ZHEEVD_m
#else
#  define _XHEEV_ CHEEV
#  define _MAGMAF_XHEEV_ magmaf_CHEEVD_m
#endif
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_HERMITIAN_diagonalization',OPR='start')
 endif
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
 lrwork=-1
 liwork=-1
 allocate(WS%v_cmplx(1))
 allocate(WS%v_real(1))
 allocate(WS%v_int(1))
 !
#if defined _MAGMA
   !
   if (.not.magma_init_done) then
     call msg('sr','Initializing magma library in geev')
     call magma_setup()
   endif
   !   
   call _MAGMAF_XHEEV_(1,'v','u', n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,&
&                      lrwork,WS%v_int,liwork,WS%i_fail)
#else
   call _XHEEV_('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
#endif
 !
 if(WS%i_fail /=0 ) call LINEAR_ALGEBRA_error(' _XHEEV_ ','WS%v_cmplx size query failed')
 !
 !
 lwork=nint(real(WS%v_cmplx(1)))
 lrwork=nint(WS%v_real(1))
 liwork=WS%v_int(1)
 deallocate(WS%v_cmplx)
 deallocate(WS%v_real)
 deallocate(WS%v_int)
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   YAMBO_ALLOC(WS%v_cmplx,(lwork))
   YAMBO_ALLOC(WS%v_real,(lrwork))
   YAMBO_ALLOC(WS%v_int,(liwork))
 else
   allocate(WS%v_cmplx(lwork))
   allocate(WS%v_real(lrwork))
   allocate(WS%v_int(liwork))
 endif

 !   
 ! use magma (if available)
 !   
#if defined _MAGMA
 ! 
 call _MAGMAF_XHEEV_(1,'v','u',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,&
&                    lrwork,WS%v_int,liwork,WS%i_fail)
 !
#else
 !
 call _XHEEV_('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
 !
#endif
 !
 if(WS%i_fail /= 0) call LINEAR_ALGEBRA_error(' _XHEEV_ ','failed')
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 !
 ! Thread safety
#if defined _OPENMP
 call omp_set_num_threads(nthreads_save)
#endif
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_HERMITIAN_diagonalization',OPR='stop')
 endif
 !
end subroutine

