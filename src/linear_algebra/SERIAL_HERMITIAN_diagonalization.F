!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,AF
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
subroutine SERIAL_HERMITIAN_diagonalization(n,M,E)
 !
 use pars,           ONLY:SP
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,&
&                         LINEAR_ALGEBRA_WS_clean_up,LALGEBRA_WS
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
 call timing('SERIAL_HERMITIAN_diagonalization',OPR='start')
 !
 lwork=-1
 allocate(WS%v_cmplx(1))
 YAMBO_ALLOC(WS%v_real,(max(1,3*n-2)))
 !
#if defined _DOUBLE
 call ZHEEV('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
#else
 call CHEEV('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
#endif
 !
 lwork=nint(real(WS%v_cmplx(1)))
 deallocate(WS%v_cmplx)
 YAMBO_ALLOC(WS%v_cmplx,(lwork))
 !
#if defined _DOUBLE
 call ZHEEV('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing ZHEEV')
#else
 call CHEEV('V','U',n,M,size(M,1),E,WS%v_cmplx,lwork,WS%v_real,WS%i_fail)
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing CHEEV')
#endif
 !
 call LINEAR_ALGEBRA_WS_clean_up(WS)
 !
 call timing('SERIAL_HERMITIAN_diagonalization',OPR='stop')
 !
end subroutine
