!
!        Copyright (C) 2000-2017 the YAMBO team
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
SUBROUTINE MATRIX_SER_to_from_PAR(N, M, mode, M_par )
 !
 ! SER <-> M_par
 !
 ! I assume here that M is common to all CPU's entering here. So not SND specific operation
 ! is implmented.
 !
 use pars,           ONLY:SP,cZERO
 use linear_algebra, ONLY:PAR_matrix,I_do_RCV
 use parallel_m,     ONLY:PP_redux_wait
 !
 implicit none
 !
 integer          :: N
 complex(SP)      :: M(N,N)
 character(1)     :: mode
 type(PAR_matrix) :: M_par
 !
 ! Work Space
 !
 integer     :: i_row,i_col
 complex(SP) :: SWAP(M_par%N)
 !
#if defined _OPENMP
 !$omp parallel default(shared), private(i_row,i_col)
 !$omp do
#endif
 ! 
 if (mode==">".and.I_do_RCV) then
   do i_row=M_par%rows(1),M_par%rows(2)
     do i_col=M_par%cols(1),M_par%cols(2)
       M_par%blc(i_row,i_col,M_par%I)=M(i_row,i_col)
     enddo
   enddo
 endif
 !
 if (mode=="<".and.I_do_RCV) then
   M=cZERO
   do i_row=M_par%rows(1),M_par%rows(2)
     do i_col=M_par%cols(1),M_par%cols(2)
       M(i_row,i_col)=M_par%blc(i_row,i_col,M_par%I)
     enddo
   enddo
   do i_row=1,M_par%N
     SWAP=M(i_row,:)
     call PP_redux_wait(SWAP,COMM=M_par%INTER_comm%COMM)
     M(i_row,:)=SWAP
   enddo
 endif
 !
#if defined _OPENMP
 !$omp end do
#endif
 !
end SUBROUTINE
