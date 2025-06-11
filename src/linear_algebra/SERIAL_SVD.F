!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SERIAL_SVD(n,M,mode,SVD_digits)
 !
 use pars,           ONLY:SP,cZERO,cONE
 use wrapper,        ONLY:M_by_M
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,la_xgesvd,&
&                         LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use timing_m,       ONLY:timing
#include<y_memory.h>
 !
 integer,     intent(in)    :: n
 complex(SP), intent(inout) :: M(n,n)
 character(3),intent(in)    :: mode
 integer,     intent(in)    :: SVD_digits
 !
 !ws
 integer           :: i_loop 
 real(SP)          :: SV_min
 type(LALGEBRA_WS) :: WS
 !
 call timing('SERIAL_SVD',OPR='start')
 !
 YAMBO_ALLOC(WS%v_real,(5*n))
 YAMBO_ALLOC(WS%vp_real,(n))
 YAMBO_ALLOC(WS%m2_cmplx,(n,n))
 YAMBO_ALLOC(WS%m1_cmplx,(n,n))
 allocate(WS%v_cmplx(1))
 !
 call la_xgesvd('S','A',n,n,M,n,WS%vp_real,WS%m2_cmplx,n,WS%m1_cmplx,n,&
&               WS%v_cmplx,-1,WS%v_real,WS%i_fail)
 !
 if(WS%i_fail/=0) call LINEAR_ALGEBRA_error('GESVD (SVD)','WS%v_cmplxspace failed')
 !
 WS%dim=int(real(WS%v_cmplx(1)))
 deallocate(WS%v_cmplx)
 YAMBO_ALLOC(WS%v_cmplx,(WS%dim))
 !
 call la_xgesvd('S','A',n,n,M,n,WS%vp_real,WS%m2_cmplx,n,WS%m1_cmplx,n,&
&               WS%v_cmplx,WS%dim,WS%v_real,WS%i_fail)
 !
 if(WS%i_fail/=0) call LINEAR_ALGEBRA_error('GESVD (SVD)','failed')
 !
 if(index(mode,'inv')/=0) then   ! Matrix inversion
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
 elseif(index(mode,'uni')/=0)  then  
   !      
   ! Transform in a unitary matrix
   ! U= M1*I*M2
   !
   call M_by_M('n','n',n,WS%m2_cmplx,WS%m1_cmplx,M)
   !
 else
   call error('Error SVD mode ')
 endif
 !
 call LINEAR_ALGEBRA_WS_reset(WS)
 !
 call timing('SERIAL_SVD',OPR='stop')
 !
end subroutine
