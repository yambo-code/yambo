!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM AF
!
subroutine SERIAL_HERMITIAN_diagonalization(n,M,E)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_nl_optics,l_real_time
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,&
&                         LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use timing_m,       ONLY:timing
#include<memory.h>
 !
 integer     :: n
 complex(SP) :: M(n,n)
 real(SP)    :: E(n)
 !
 character(64)     :: subname="SERIAL_HERMITIAN_diagonalization"
 integer           :: lwork
 type(LALGEBRA_WS) :: WS
 !
 lwork=-1
 allocate(WS%v_cmplx(1))
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_HERMITIAN_diagonalization',OPR='start')
   YAMBO_ALLOC(WS%v_real,(max(1,3*n-2)))
 else
   allocate(WS%v_real(max(1,3*n-2)))
 endif
 !
#if defined _DOUBLE
 call ZHEEV('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
#else
 call CHEEV('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
#endif
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
#if defined _DOUBLE
 call ZHEEV('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing ZHEEV')
#else
 call CHEEV('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing CHEEV')
#endif
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_HERMITIAN_diagonalization',OPR='stop')
 endif
 !
end subroutine
