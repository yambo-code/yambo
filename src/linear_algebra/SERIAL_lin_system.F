!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AF
!
subroutine SERIAL_lin_system(n,nrhs,A,B)
 !
 !  Solve the linear system
 !                   A X = B
 !  where A is a n-x-n matrix and B is n-x-nrhs
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_nl_optics,l_real_time
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use timing_m,       ONLY:timing
 !
#include<y_memory.h>
 !
 integer     :: n,nrhs
 complex(SP) :: A(n,n),B(n,nrhs)
 !
 !ws
 character(64)     :: subname="SERIAL_lin_system"
 type(LALGEBRA_WS) :: WS
 !
 WS%dim=n
 !
 if(.not.(l_nl_optics.or.l_real_time)) then
   call timing(trim(subname),OPR='start')
   YAMBO_FREE(WS%v_int)
   YAMBO_ALLOC(WS%v_int,(WS%dim))
 else
   if(allocated(WS%v_int)) deallocate(WS%v_int)
   allocate(WS%v_int(WS%dim))
 endif
 !
#if defined _DOUBLE
 CALL ZGESV(n,nrhs,A,n,WS%v_int,B,n,WS%i_fail)
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'ZGESV failed')
#else
 CALL CGESV(n,nrhs,A,n,WS%v_int,B,n,WS%i_fail)
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'CGESV failed')
#endif
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 !
 if(.not.(l_nl_optics.or.l_real_time)) call timing(trim(subname),OPR='stop')
 !
end subroutine SERIAL_lin_system

