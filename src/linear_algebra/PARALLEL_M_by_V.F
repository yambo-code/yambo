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
subroutine PARALLEL_M_by_V(transa,N,A,X,Y)
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
 use SLK_m,          ONLY:SLK_POOL,SLK_ORTHO
 use matrix,         ONLY:PAR_matrix
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 implicit none
 !
 integer           :: N
 type(PAR_matrix)  :: A
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
   call M_by_V_omp("n",A%nrows,A%ncols,cONE,conjg(A%blc(A%rows(1):A%rows(2),A%cols(1):A%cols(2),A%I)),&
&                  A%nrows,X(A%cols(1):A%cols(2)),1,cZERO,Y(A%rows(1):A%rows(2)),1)
 else if (transa=="c") then
   call M_by_V_omp("c",A%nrows,A%ncols,cONE,A%blc(A%rows(1):A%rows(2),A%cols(1):A%cols(2),A%I),&
&                  A%nrows,X(A%rows(1):A%rows(2)),1,cZERO,Y(A%cols(1):A%cols(2)),1)
 else
   call M_by_V_omp("n",A%nrows,A%ncols,cONE,A%blc(A%rows(1):A%rows(2),A%cols(1):A%cols(2),A%I),&
&                  A%nrows,X(A%cols(1):A%cols(2)),1,cZERO,Y(A%rows(1):A%rows(2)),1)
 endif
 !
 call PP_redux_wait(Y,COMM=SLK_POOL%INTRA_comm)
 ! 
#if defined _TIMING
 call timing('PARALLEL_M_by_V',OPR='stop')
#endif
 !
end subroutine
