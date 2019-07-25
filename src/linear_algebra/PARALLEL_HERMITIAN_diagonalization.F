!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine PARALLEL_HERMITIAN_diagonalization(ORTHO, POOL, M_slk, EIGEN_slk, N, E)
 !
 ! perform the diagonalization by using scalapack
 !
 use pars,           ONLY:SP
 use parallel_m,     ONLY:PP_redux_wait
 use SLK_m,          ONLY:POOL_group,ORTHO_group,SLK_matrix
 use linear_algebra, ONLY:WS,LINEAR_ALGEBRA_error,WS,LINEAR_ALGEBRA_WS_clean_up
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
#include<memory.h>
 !
 integer           :: N
 type(POOL_group)  :: POOL
 type(ORTHO_group) :: ORTHO
 type(SLK_matrix)  :: M_slk,EIGEN_slk
 real(SP)          :: E(N)
 !
 character(64) :: subname="PARALLEL_HERMITIAN_diagonalization"
 integer :: lwork,lrwork
 real(SP):: rvar
 complex(SP) :: cvar
 !
 E=0.0_SP
 !
 if (ORTHO%coordinate(1)==-1) return
 !
#if defined _TIMING
 call timing('PARALLEL_HERMITIAN_diagonalization',OPR='start')
#endif
 !
 lwork=-1
 lrwork=-1
 !
#if defined _DOUBLE
 call PZHEEV('V','U',N,M_slk%blc,1,1,M_slk%desc,E,EIGEN_slk%blc,1,1,EIGEN_slk%desc,&
             cvar,lwork,rvar,lrwork,WS%i_fail)
#else
 call PCHEEV('V','U',N,M_slk%blc,1,1,M_slk%desc,E,EIGEN_slk%blc,1,1,EIGEN_slk%desc,&
             cvar,lwork,rvar,lrwork,WS%i_fail)
#endif
 !
 !lwork=nint(real(cvar))
 !lrwork=nint(rvar)
 ! AF: better estimates should be used
 lwork=2*n**2
 lrwork=2*n + 2*n-2
 YAMBO_ALLOC(WS%v_cmplx,(lwork))
 YAMBO_ALLOC(WS%v_real,(lrwork))
 !
#if defined _DOUBLE
 call PZHEEV('V','U',N,M_slk%blc,1,1,M_slk%desc,E,EIGEN_slk%blc,1,1,EIGEN_slk%desc,&
             WS%v_cmplx,lwork,WS%v_real,lrwork,WS%i_fail)
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing PZHEEV')
#else
 call PCHEEV('V','U',N,M_slk%blc,1,1,M_slk%desc,E,EIGEN_slk%blc,1,1,EIGEN_slk%desc,&
             WS%v_cmplx,lwork,WS%v_real,lrwork,WS%i_fail)
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing PCHEEV')
#endif
 !
 if (ORTHO%task/=0) E=0.0_SP
 !
 call PP_redux_wait(E,COMM=POOL%INTRA_comm)
 ! 
#if defined _TIMING
 call timing('PARALLEL_HERMITIAN_diagonalization',OPR='stop')
#endif
 !
end subroutine PARALLEL_HERMITIAN_diagonalization

