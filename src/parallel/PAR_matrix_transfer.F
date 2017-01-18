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
SUBROUTINE PAR_matrix_transfer( mode, M_slk, M_par, X_up, X_dn, M_bse, M ,i_MAT )
 !
 ! mode can be:
 !
 ! "<": M_bse->M_slk,M->M_slk,M_par->M_slk
 !
 ! ">": M_slk->M,M_slk->M_par
 !      M_par->M
 !
 use pars,           ONLY:SP,cZERO
 use SLK_m,          ONLY:SLK_POOL
 use BS,             ONLY:BS_block
 use parallel_m,     ONLY:PP_redux_wait
 use X_m,            ONLY:X_FILL_UP_matrix_only,X_lower_triangle_matrix_in_use
 use D_lattice,      ONLY:i_space_inv
 use linear_algebra, ONLY:PAR_matrix
 !
 implicit none
 !
 character(*)               :: mode
 type(PAR_matrix), optional :: M_slk
 type(PAR_matrix), optional :: M_par
 type(PAR_matrix), optional :: X_up
 type(PAR_matrix), optional :: X_dn
 type(BS_block),   optional :: M_bse(:)
 complex(SP),      optional :: M(:,:)
 integer,          optional :: i_MAT
 !
 ! Work Space
 !
 integer :: i_row,i_col,cols(2)
 !
#if defined _SCALAPACK
 !
#if defined _OPENMP
 !$omp parallel default(shared), private(i_row,i_col)
#endif
 !
 if (present(M_slk)) then
   !
   if (present(M)) then
     !
     if (mode=="slk < M".or.mode=="M > slk") then
       !
#if defined _OPENMP
       !$omp do
#endif
       do i_row=1,M_slk%nrows_used
         do i_col=1,M_slk%nrows_used
           M_slk%blc(i_row,i_col)=M(M_slk%rows(1)+i_row-1,M_slk%cols(1)+i_col-1)
         enddo
       enddo
#if defined _OPENMP
       !$omp end do
#endif
       !
     endif
     !
     if (mode=="slk > M".or.mode=="M < slk") then
       !
       M=cZERO
       !
#if defined _OPENMP
       !$omp do
#endif
       do i_row=1,M_slk%nrows_used
         do i_col=1,M_slk%nrows_used
           M(M_slk%rows(1)+i_row-1,M_slk%cols(1)+i_col-1)=M_slk%blc(i_row,i_col)
         enddo
       enddo
#if defined _OPENMP
       !$omp end do
#endif
       !
       call PP_redux_wait(M,COMM=SLK_POOL%INTRA_comm)
       !
     endif
     !
   endif
   !
   if (present(M_bse)) then
     !
     if (mode=="K > slk".or.mode=="slk < K") then
       call PAR_matrix_transfer_K_to_SLK(M_bse,M_slk)
     endif
     !
   endif
   !
   if (present(M_par)) then
     !
     if (mode=="par > slk".or.mode=="par < par") then
       call PAR_matrix_transfer_PAR_to_SLK("par",M_par,M_slk)
     endif
     !
   endif
   !
   if (present(X_up)) then
     !
     if (mode=="X > slk".or.mode=="slk < X") then
       if (present(X_dn)) then
         call PAR_matrix_transfer_PAR_to_SLK("X up",X_up,M_slk,i_MAT)
         call PAR_matrix_transfer_PAR_to_SLK("X dn",X_dn,M_slk,i_MAT)
       else
         call PAR_matrix_transfer_PAR_to_SLK("X",X_up,M_slk,i_MAT)
       endif
     endif
     !
   endif
   !
 endif
 !
#if defined _OPENMP
 !$omp end parallel
#endif
 !
#endif
 !
#if defined _OPENMP
 !$omp parallel default(shared), private(i_row,i_col,cols)
#endif
 !
 if (present(X_up)) then
   !
   if (present(M)) then
     !
     if (mode=="X > M".or.mode=="M < X") then
       !
       M=cZERO
       !
       do i_row=X_up%rows(1),X_up%rows(2)
         cols=X_up%cols
         if (X_FILL_UP_matrix_only.or.X_lower_triangle_matrix_in_use) cols=(/i_row,X_up%cols(2)/)
         do i_col=cols(1),cols(2)
           M(i_row,i_col)=X_up%blc_3d(i_row,i_col,i_MAT)
         enddo
         !
         if (.not.X_FILL_UP_matrix_only.and..not.X_lower_triangle_matrix_in_use) cycle
         !
         if (cols(1)==cols(2)) cycle
         !
         cols=(/i_row+1,X_up%cols(2)/)
         do i_col=cols(1),cols(2)
           if (present(X_dn)) then
             M(i_col,i_row)=X_dn%blc_3d(i_col,i_row,i_MAT)
           else
             if (i_space_inv==0) then
               M(i_col,i_row)=conjg(X_up%blc_3d(i_row,i_col,i_MAT))
             else
               M(i_col,i_row)=X_up%blc_3d(i_row,i_col,i_MAT)
             endif
           endif
         enddo
       enddo
       !
       call PP_redux_wait(M,COMM=X_up%COMM%COMM)
       !
     endif
     !
     return
     !
   endif
   !
 endif
 !
#if defined _OPENMP
 !$omp end parallel
#endif
 !
end SUBROUTINE
