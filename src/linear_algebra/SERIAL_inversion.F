!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine SERIAL_inversion(n,M,det,eval_det)
 !
 use pars,          ONLY:SP,cONE
 use linear_algebra,ONLY:LINEAR_ALGEBRA_error,LU_factorization,LU_inversion,&
&                        LINEAR_ALGEBRA_WS_clean_up,LALGEBRA_WS
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
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
#if defined _TIMING
 call timing('SERIAL_inversion',OPR='start')
#endif
 !
 YAMBO_ALLOC(WS%v_int,(n))
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
 allocate(WS%v_cmplx(1))
 call LU_inversion(n,M,WS%v_int,WS%v_cmplx,-1,WS%i_fail)
 WS%dim=int(real(WS%v_cmplx(1)))
 deallocate(WS%v_cmplx)
 YAMBO_ALLOC(WS%v_cmplx,(WS%dim))
 call LU_inversion(n,M,WS%v_int,WS%v_cmplx,WS%dim,WS%i_fail)
 !
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error('GETRI (SI)','failed')
 !
 call LINEAR_ALGEBRA_WS_clean_up(WS)
 !
#if defined _TIMING
 call timing('SERIAL_inversion',OPR='stop')
#endif
 !
end subroutine
