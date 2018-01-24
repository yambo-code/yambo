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
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
subroutine PARALLEL_HERMITIAN_diagonalization(M, V, N, E)
 !
 ! perform the diagonalization by using scalapack
 !
 use pars,           ONLY:SP
 use parallel_m,     ONLY:PP_redux_wait
 use SLK_m,          ONLY:SLK_POOL,SLK_ORTHO
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,LINEAR_ALGEBRA_WS_clean_up,LALGEBRA_WS
 use matrix,         ONLY:PAR_matrix
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
#include<memory.h>
 !
 integer           :: N
 type(PAR_matrix)  :: M,V
 real(SP)          :: E(N)
 !
 character(64)     :: subname="PARALLEL_HERMITIAN_diagonalization"
 integer           :: lwork,lrwork
 real(SP)          :: rvar
 complex(SP)       :: cvar
 type(LALGEBRA_WS) :: WS
 !
 E=0.0_SP
 !
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
#if defined _TIMING
 call timing('PARALLEL_HERMITIAN_diagonalization',OPR='start')
#endif
 !
 lwork=-1
 lrwork=-1
 !
#if defined _DOUBLE
 call PZHEEV&
#else
 call PCHEEV&
#endif
&  ('V','U',N,M%blc(:,:,M%I),1,1,M%desc,E,V%blc(:,:,V%I),1,1,V%desc,cvar,lwork,rvar,lrwork,WS%i_fail)
 !
 ! AF: A better estimates should be used
 lwork=2*n**2
 lrwork=2*n + 2*n-2
 YAMBO_ALLOC(WS%v_cmplx,(lwork))
 YAMBO_ALLOC(WS%v_real,(lrwork))
 !
#if defined _DOUBLE
 call PZHEEV&
#else
 call PCHEEV&
#endif
& ('V','U',N,M%blc(:,:,M%I),1,1,M%desc,E,V%blc(:,:,V%I),1,1,V%desc,WS%v_cmplx,lwork,WS%v_real,lrwork,WS%i_fail)
 !
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing P(Z/C)HEEV')
 !
 if (SLK_ORTHO%task/=0) E=0.0_SP
 !
 call PP_redux_wait(E,COMM=SLK_POOL%INTRA_comm)
 !
 call LINEAR_ALGEBRA_WS_clean_up(WS) 
 !
#if defined _TIMING
 call timing('PARALLEL_HERMITIAN_diagonalization',OPR='stop')
#endif
 !
end subroutine PARALLEL_HERMITIAN_diagonalization

