!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF
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
subroutine SERIAL_lin_system(n,nrhs,A,B)
 !
 !  Solve the linear system
 !                   A X = B
 !  where A is a n-x-n matrix and B is n-x-nrhs
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_nl_optics,l_real_time
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,LINEAR_ALGEBRA_WS_clean_up,LALGEBRA_WS
 use timing_m,       ONLY:timing
 !
#include<memory.h>
 !
 integer     :: n,nrhs
 complex(SP) :: A(n,n),B(n,nrhs)
 !
 !ws
 character(64)     :: subname="SERIAL_lin_system"
 type(LALGEBRA_WS) :: WS
 !
 if(.not.(l_nl_optics.or.l_real_time)) call timing(trim(subname),OPR='start')
 !
 WS%dim=n
 YAMBO_FREE(WS%v_int)
 YAMBO_ALLOC(WS%v_int,(WS%dim))
 !
#if defined _DOUBLE
 CALL ZGESV(n,nrhs,A,n,WS%v_int,B,n,WS%i_fail)
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'ZGESV failed')
#else
 CALL CGESV(n,nrhs,A,n,WS%v_int,B,n,WS%i_fail)
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'CGESV failed')
#endif
 !
 call LINEAR_ALGEBRA_WS_clean_up(WS)
 !
 if(.not.(l_nl_optics.or.l_real_time)) call timing(trim(subname),OPR='stop')
 !
end subroutine SERIAL_lin_system
