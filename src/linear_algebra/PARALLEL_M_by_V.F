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
subroutine PARALLEL_M_by_V(transa,N,A_slk,X,Y)
 !
 !  Perform the parallel matrix multiplication
 !
 !          Y = A X 
 !
 !  where A(n,m), X(i:i+m-1), Y(j:j+n-1)
 !
 use pars,           ONLY:SP,cONE,cZERO
 use wrapper_omp,    ONLY:M_by_V_omp
 use parallel_m,     ONLY:PP_redux_wait
 use SLK_m,          ONLY:SLK_matrix,SLK_POOL,SLK_ORTHO
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 implicit none
 !
 integer           :: N
 type(SLK_matrix)  :: A_slk
 character(1)      :: transa
 complex(SP)       :: X(N)
 complex(SP)       :: Y(N)
 !
 Y=cZERO
 !
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
#if defined _TIMING
 call timing('PARALLEL_M_by_V',OPR='start')
#endif
 !
 if (transa=="C") then
   call M_by_V_omp("n",A_slk%nrows_used,A_slk%ncols_used,cONE,conjg(A_slk%blc),&
&                  A_slk%nrows_used,X(A_slk%cols(1):A_slk%cols(2)),1,cZERO,&
&                  Y(A_slk%rows(1):A_slk%rows(2)),1)
 else if (transa=="c") then
   call M_by_V_omp("c",A_slk%nrows_used,A_slk%ncols_used,cONE,A_slk%blc,&
&                  A_slk%nrows_used,X(A_slk%rows(1):A_slk%rows(2)),1,cZERO,&
&                  Y(A_slk%cols(1):A_slk%cols(2)),1)
 else
   call M_by_V_omp("n",A_slk%nrows_used,A_slk%ncols_used,cONE,A_slk%blc,&
&                  A_slk%nrows_used,X(A_slk%cols(1):A_slk%cols(2)),1,cZERO,&
&                  Y(A_slk%rows(1):A_slk%rows(2)),1)
 endif
 !
 call PP_redux_wait(Y,COMM=SLK_POOL%INTRA_comm)
 ! 
#if defined _TIMING
 call timing('PARALLEL_M_by_V',OPR='stop')
#endif
 !
end subroutine
