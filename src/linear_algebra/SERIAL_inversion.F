!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SERIAL_inversion(n,M,det,eval_det)
 !
 use pars,          ONLY:SP,cONE
 use drivers,       ONLY:l_nl_optics,l_real_time
 use linear_algebra,ONLY:LINEAR_ALGEBRA_error,LU_factorization,LU_inversion,&
&                        LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use timing_m,       ONLY:timing
#include<memory.h>
 !
 integer           :: n
 complex(SP)       :: M(n,n),det
 logical           :: eval_det
 !
 !ws
 integer           :: i_loop 
 type(LALGEBRA_WS) :: WS
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing('SERIAL_inversion',OPR='start')
   YAMBO_ALLOC(WS%v_int,(n))
 else
   allocate(WS%v_int(n))
 endif
 !
 call LU_factorization(n,M,WS%v_int,WS%i_fail)
 !
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error('GETRF (SI)','WS%v_cmplxspace failed')
 !
 ! Evaluate determinant if required
 !
 det=cONE
 !
 if(eval_det) then
   do i_loop=1,n
     det=det*M(i_loop,i_loop)
     if(WS%v_int(i_loop).ne.i_loop) det=-det
   enddo
 endif
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   YAMBO_ALLOC(WS%v_cmplx,(1))
 else
   allocate(WS%v_cmplx(1))
 endif
 call LU_inversion(n,M,WS%v_int,WS%v_cmplx,-1,WS%i_fail)
 WS%dim=int(real(WS%v_cmplx(1)))
 if(.not.(l_nl_optics.or.l_real_time)) then
   YAMBO_FREE(WS%v_cmplx)
   YAMBO_ALLOC(WS%v_cmplx,(WS%dim))
 else
   deallocate(WS%v_cmplx)
   allocate(WS%v_cmplx(WS%dim))
 endif
 call LU_inversion(n,M,WS%v_int,WS%v_cmplx,WS%dim,WS%i_fail)
 !
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error('GETRI (SI)','failed')
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 !
 if(.not.(l_nl_optics.or.l_real_time)) call timing('SERIAL_inversion',OPR='stop')
 !
end subroutine
