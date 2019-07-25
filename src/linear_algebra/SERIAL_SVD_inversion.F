!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine SERIAL_SVD_inversion(n,M,SVD_digits)
 !
 use pars,           ONLY:SP,cZERO
 use wrapper,        ONLY:M_by_M
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,WS,SV_decomposition,&
&                         LINEAR_ALGEBRA_WS_clean_up
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
#include<memory.h>
 !
 integer     :: n
 complex(SP) :: M(n,n)
 integer     :: SVD_digits 
 integer     :: i_loop !ws
 real(SP)    :: SV_min !ws
 !
#if defined _TIMING
 call timing('SERIAL_SVD_inversion',OPR='start')
#endif
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
 call LINEAR_ALGEBRA_WS_clean_up(WS)
 !
#if defined _TIMING
 call timing('SERIAL_SVD_inversion',OPR='stop')
#endif
 !
end subroutine
