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
SUBROUTINE MATRIX_SER_to_from_PAR(N, M, mode, M_par, i_MAT )
 !
 ! SER <-> M_par
 !
 use pars,           ONLY:SP,cZERO
 use linear_algebra, ONLY:PAR_matrix
 use parallel_m,     ONLY:PP_redux_wait
 !
 implicit none
 !
 complex(SP)      :: M(N,N)
 character(1)     :: mode
 type(PAR_matrix) :: M_par
 integer          :: i_MAT,N
 !
 ! Work Space
 !
 integer         :: i_row,i_col
 !
#if defined _OPENMP
 !$omp parallel default(shared), private(i_row,i_col)
 !$omp do
#endif
 !
 if (mode==">") then
   do i_row=M_par%rows(1),M_par%rows(2)
     do i_col=M_par%cols(1),M_par%cols(2)
       if (i_MAT>0) then
         M_par%blc_3d(i_row,i_col,i_MAT)=M(i_row,i_col)
       else
         M_par%blc(i_row,i_col)         =M(i_row,i_col)
       endif
     enddo
   enddo
 endif
 !
 if (mode=="<") then
   M=cZERO
   do i_row=M_par%rows(1),M_par%rows(2)
     do i_col=M_par%cols(1),M_par%cols(2)
       if (i_MAT>0) then
         M(i_row,i_col)=M_par%blc_3d(i_row,i_col,i_MAT)
       else
         M(i_row,i_col)=M_par%blc(i_row,i_col)
       endif
     enddo
   enddo
   call PP_redux_wait(M,COMM=M_par%INTRA_comm%COMM)
 endif
 !
#if defined _OPENMP
 !$omp end do
#endif
 !
end SUBROUTINE
