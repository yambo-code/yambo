!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AF
!
subroutine SERIAL_HERMITIAN_diagonalization(n,M,E)
 !
 use magma
 use pars,           ONLY:SP
 use drivers,        ONLY:l_nl_optics,l_real_time
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,&
&                         LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use timing_m,       ONLY:timing
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
 ! MAGMA
 call magmaf_init()
 !
 lwork=-1
 lrwork=-1
 liwork=-1
 allocate(WS%v_cmplx(1))
 allocate(WS%v_real(1))
 allocate(WS%v_int(1))
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_HERMITIAN_diagonalization',OPR='start')
 endif
 !
 ! Query lwork, lrwork, liwork
 !
#if defined _DOUBLE
 call ZHEEV('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
#else
 call magmaf_cheevd_m(2,'v','u',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,lrwork,WS%v_int,liwork,WS%i_fail)
#endif
 !
 ! Allocate v_cmplx, v_real, v_int
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
 ! Compute eigenvalues
 !
#if defined _DOUBLE
 call ZHEEV('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing ZHEEV')
#else
 call magmaf_cheevd_m(2,'v','u',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,lrwork,WS%v_int,liwork,WS%i_fail)
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing CHEEV')
#endif
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_HERMITIAN_diagonalization',OPR='stop')
 endif
 !
 call magmaf_finalize()
end subroutine
