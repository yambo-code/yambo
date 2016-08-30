!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine PARALLEL_HERMITIAN_diagonalization(ORTHO, POOL, M_slk, EIGEN_slk, n, M, E)
 !
 ! perform the diagonalization by using scalapack
 !
 use pars,           ONLY:SP
 use parallel_m,     ONLY:PP_redux_wait,myid
 use SLK_m,          ONLY:POOL_group,ORTHO_group,SLK_matrix
 use linear_algebra, ONLY:WS,LINEAR_ALGEBRA_error,WS,LINEAR_ALGEBRA_WS_clean_up
 !
 implicit none
 !
 integer           :: n
 type(POOL_group)  :: POOL
 type(ORTHO_group) :: ORTHO
 type(SLK_matrix)  :: M_slk,EIGEN_slk
 complex(SP)       :: M(n,n)
 real(SP)          :: E(n)
 !
 character(64) :: subname="PARALLEL_HERMITIAN_diagonalization"
 logical :: do_SLK
 integer :: lwork,lrwork
 real(SP):: rvar
 complex(SP) :: cvar

 !
 ! spectator tasks
 !
 M=0.0_SP
 E=0.0_SP
 !
 do_SLK=.TRUE.
 if (ORTHO%coordinate(1)==-1) do_SLK=.FALSE.
 !
 if ( do_SLK ) then
   !
   lwork=-1
   lrwork=-1
   !
#if defined _DOUBLE
   call PZHEEV('V','U',n,M_slk%blc,1,1,M_slk%desc,E,EIGEN_slk%blc,1,1,EIGEN_slk%desc,&
               cvar,lwork,rvar,lrwork,WS%i_fail)
#else
   call PCHEEV('V','U',n,M_slk%blc,1,1,M_slk%desc,E,EIGEN_slk%blc,1,1,EIGEN_slk%desc,&
               cvar,lwork,rvar,lrwork,WS%i_fail)
#endif
   !
   !lwork=nint(real(cvar))
   !lrwork=nint(rvar)
   ! AF: better estimates should be used
   lwork=2*n**2
   lrwork=2*n + 2*n-2
   allocate(WS%v_cmplx(lwork))
   allocate(WS%v_real(lrwork))
   !
#if defined _DOUBLE
   call PZHEEV('V','U',n,M_slk%blc,1,1,M_slk%desc,E,EIGEN_slk%blc,1,1,EIGEN_slk%desc,&
               WS%v_cmplx,lwork,WS%v_real,lrwork,WS%i_fail)
   if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing PZHEEV')
#else
   call PCHEEV('V','U',n,M_slk%blc,1,1,M_slk%desc,E,EIGEN_slk%blc,1,1,EIGEN_slk%desc,&
               WS%v_cmplx,lwork,WS%v_real,lrwork,WS%i_fail)
   if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing PCHEEV')
#endif
   !
   ! gather eigenvalues and eigenvectors
   !
   if (ORTHO%task/=0) E=0.0_SP
   M(M_slk%rows(1):M_slk%rows(2),M_slk%cols(1):M_slk%cols(2))=EIGEN_slk%blc(:M_slk%nrows_used,:M_slk%ncols_used)
   !
 endif
 !
 call PP_redux_wait(M,COMM=POOL%INTRA_comm)
 call PP_redux_wait(E,COMM=POOL%INTRA_comm)
 !
 call LINEAR_ALGEBRA_WS_clean_up(WS)
 ! 
end subroutine PARALLEL_HERMITIAN_diagonalization

