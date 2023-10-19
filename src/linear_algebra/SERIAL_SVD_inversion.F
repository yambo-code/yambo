!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SERIAL_SVD_inversion(n,M,SVD_digits)
 !
 use pars,           ONLY:SP,cZERO
 use wrapper,        ONLY:M_by_M
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,SV_decomposition,&
&                         LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use timing_m,       ONLY:timing
#include<memory.h>
 !
 integer     :: n
 complex(SP) :: M(n,n)
 integer     :: SVD_digits 
 !
 !ws
 integer           :: i_loop 
 real(SP)          :: SV_min
 type(LALGEBRA_WS) :: WS
 !
 call timing('SERIAL_SVD_inversion',OPR='start')
 !
 YAMBO_ALLOC(WS%v_real,(5*n))
 YAMBO_ALLOC(WS%vp_real,(n))
 YAMBO_ALLOC(WS%m2_cmplx,(n,n))
 YAMBO_ALLOC(WS%m1_cmplx,(n,n))
 allocate(WS%v_cmplx(1))
 !
 call SV_decomposition(n,M,WS%vp_real,WS%m2_cmplx,WS%m1_cmplx,WS%v_cmplx,-1,WS%v_real,WS%i_fail)
 !
 if(WS%i_fail/=0) call LINEAR_ALGEBRA_error('GESVD (SVD)','WS%v_cmplxspace failed')
 !
 WS%dim=int(real(WS%v_cmplx(1)))
 deallocate(WS%v_cmplx)
 YAMBO_ALLOC(WS%v_cmplx,(WS%dim))
 call SV_decomposition(n,M,WS%vp_real,WS%m2_cmplx,WS%m1_cmplx,WS%v_cmplx,WS%dim,WS%v_real,WS%i_fail)
 !
 if(WS%i_fail/=0) call LINEAR_ALGEBRA_error('GESVD (SVD)','failed')
 !
 SV_min=10._SP**(-SVD_digits)
 !
 do i_loop=1,n
   if (abs(WS%vp_real(i_loop))<SV_min) then
     WS%m1_cmplx(i_loop,:)=cZERO
   else
     WS%m1_cmplx(i_loop,:)=WS%m1_cmplx(i_loop,:)/WS%vp_real(i_loop)
   endif
 enddo
 !
 call M_by_M('c','c',n,WS%m1_cmplx,WS%m2_cmplx,M)
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 !
 call timing('SERIAL_SVD_inversion',OPR='stop')
 !
end subroutine
