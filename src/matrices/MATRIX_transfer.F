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
SUBROUTINE MATRIX_transfer( M_in, M_bse, M, M_out, INDEX_in, INDEX_out)
 !
 ! For the table of actions see below...
 !
 use pars,           ONLY:SP
 use BS,             ONLY:BS_block
 use linear_algebra, ONLY:PAR_matrix
#include<memory.h>
 !
 type(PAR_matrix), optional :: M_in
 type(PAR_matrix), optional :: M_out
 type(BS_block),   optional :: M_bse(:)
 integer,          optional :: INDEX_in
 integer,          optional :: INDEX_out
 complex(SP),      optional :: M(:,:)
 !
 ! Work Space
 !
 integer              :: M_index_in,M_index_out,N
 character(3)         :: M_in_kind, M_out_kind
 logical              :: l_MAT_ARRAY
 !
 ! KIND of the I/O matrices
 !
 l_MAT_ARRAY=.FALSE.
 if (present(M)) then
   l_MAT_ARRAY=.TRUE.
   N=size(M,1)
 endif
 !
 M_in_kind  ="NAN"
 if (present(M_in)) then
   M_in_kind=M_in%kind
 else if (present(M_bse)) then
   M_in_kind="BSE"
 endif
 if (l_MAT_ARRAY.and.present(M_out)) then
   M_in_kind="SER"
 endif
 !
 M_out_kind="NAN"
 if (present(M_out)) then
   M_out_kind=M_out%kind
 endif
 if (l_MAT_ARRAY.and.present(M_in)) then
   M_out_kind="SER"
 endif
 !
 ! INDEXes
 !
 M_index_in =0
 M_index_out=0
 if (present(INDEX_in)) then
   M_index_in=INDEX_in
 endif
 if (present(INDEX_out)) then
   M_index_out=INDEX_out
 endif
 !
 ! Table of actions
 !
 ! in\out NAN SER                     BSE PAR                     SLK                    XUP XDN 
 ! NAN     x   x                       x   x                       x                      x   x
 ! SER     x  MATRIX_SER_to_from_PAR   x  MATRIX_SER_to_from_PAR  MATRIX_SER_to_from_PAR  x   x
 ! BSE     x   x                       x   x                      MATRIX_BSE_to_PAR       x   x
 ! PAR     x  MATRIX_SER_to_from_PAR   x   x                      MATRIX_PAR_to_from_PAR  x   x  
 ! SLK     x  MATRIX_SER_to_from_PAR   x  MATRIX_PAR_to_from_PAR   x                      x   x
 ! XUP     x  ???                      x   x                      ???                     x   x
 ! XDN     x  ???                      x   x                      ???                     x   x
 !
 !
 if (M_in_kind=="SER") then
   !
   if (M_out_kind=="PAR".or.M_out_kind=="SLK".or.M_out_kind=="SER") then
     if (     l_MAT_ARRAY) call MATRIX_SER_to_from_PAR(N,M,">",M_out,M_index_out) 
     if (.not.l_MAT_ARRAY) then
       if (M_index_in>0) then                   
         call MATRIX_SER_to_from_PAR(M_in%N,M_in%blc_3d(:,:,M_index_in),">",M_out,M_index_out) 
       else
         call MATRIX_SER_to_from_PAR(M_in%N,M_in%blc,">",M_out,M_index_out) 
       endif
     endif
   endif
   !
 else if (M_in_kind=="BSE") then
   !
   if (M_out_kind=="SLK") call MATRIX_BSE_to_PAR(M_bse,M_out)
   !
 else if (M_in_kind=="PAR".or.M_in_kind=="SLK") then
   !
   if (M_out_kind=="SER") then
     if (     l_MAT_ARRAY) call MATRIX_SER_to_from_PAR(N,M,"<",M_in,M_index_in) 
     if (.not.l_MAT_ARRAY) then
       if (M_index_in>0) then                   
         call MATRIX_SER_to_from_PAR(M_in%N,M_in%blc_3d(:,:,M_index_in),"<",M_in,M_index_in) 
       else
         call MATRIX_SER_to_from_PAR(M_in%N,M_in%blc,"<",M_in,M_index_in) 
       endif
     endif
   endif
   if (M_out_kind=="SLK".or.M_out_kind=="PAR") call MATRIX_PAR_to_from_PAR(M_in,">",M_out,M_index_in,M_index_out)
   !
 endif
!   !
!   ! X > slk
!   !=========
!   !
!   if (present(X_up)) then
!     !
!     if (mode=="X > slk".or.mode=="slk < X") then
!       if (present(X_dn)) then
!         call MATRIX_PAR_to_from_SLK("X up","SLK",X_up,M_slk,i_MAT)
!         call MATRIX_PAR_to_from_SLK("X dn","SLK",X_dn,M_slk,i_MAT)
!       else
!         call MATRIX_PAR_to_from_SLK("X","SLK",X_up,M_slk,i_MAT)
!       endif
!     endif
!     !
!   endif
!   !
!   ! slk > PAR
!   !===========
!   !
!   if (present(M_par)) then
!     !
!     if (mode=="slk > par".or.mode=="par < slk") then
!       call MATRIX_PAR_to_from_SLK(" ","PAR",M_par,M_slk,i_MAT)
!     endif
!     !
!   endif
!   !
! endif
! !
!! if (present(M_par)) then
!!   !
!!   ! M > PAR
!!   !=========
!!   !
!!   if (present(M)) then
!!     !
!!     call PAR_matrix_transfer_PAR_to_SLK("X","PAR",X_up,M_slk,i_MAT)
!!     !
!!     N=size(M,1)
!!     Y_ALLOCATE(SWAP(N,N))
!!     Y_MEM(SWAP)
!!     SWAP=cZERO
!!     !
!!#if defined _OPENMP
!!     !$omp do
!#endif
!     do i_row=M_par%rows(1),M_par%rows(2)
!       do i_col=M_par%cols(1),M_par%cols(2)
!         M_par%blc_3d(i_row,i_col,i_MAT)=M(i_row,i_col)
!       enddo
!     enddo
!#if defined _OPENMP
!     !$omp end do
!#endif
!   endif
! endif
! !
!#if defined _OPENMP
! !$omp end parallel
!#endif
! !
! if (present(X_up)) then
!   !
!   ! X > M
!   !=======
!   !
!   if (present(M)) then
!     !
!     N=size(M,1)
!     Y_ALLOCATE(SWAP(N,N))
!     Y_MEM(SWAP)
!     SWAP=cZERO
!     !
!     if (mode=="X > M".or.mode=="M < X") then
!       !
!#if defined _OPENMP
!       !$omp parallel default(shared), private(i_row,i_col)
!#endif
!       !
!       do i_row=X_up%rows(1),X_up%rows(2)
!         do i_col=X_up%cols(1),X_up%cols(2)
!           !
!           SWAP(i_row,i_col)=X_up%blc_3d(i_row,i_col,iabs(i_MAT))
!           !
!           if (X_FILL_UP_matrix_only.or.X_lower_triangle_matrix_in_use) then
!             if (present(X_dn)) then
!               SWAP(i_col,i_row)=X_dn%blc_3d(i_col,i_row,iabs(i_MAT))
!             else
!               if (i_space_inv==0) then
!                 SWAP(i_col,i_row)=conjg(X_up%blc_3d(i_row,i_col,iabs(i_MAT)))
!               else
!                 SWAP(i_col,i_row)=X_up%blc_3d(i_row,i_col,iabs(i_MAT))
!               endif
!             endif
!           endif
!           !
!         enddo
!       enddo
!       !
!#if defined _OPENMP
!       !$omp end parallel
!#endif
!       !
!       call PP_redux_wait(SWAP,COMM=X_up%INTER_comm%COMM)
!       !
!       if (i_MAT>0) M=SWAP
!       !
!     endif
!     !
!     Y_FREE(SWAP)
!     !
!   endif
!   !
!   !
! endif
! !
end SUBROUTINE
