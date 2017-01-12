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
SUBROUTINE PAR_matrix_transfer( mode, M_slk, M_par, M_bse, M ,i_MAT )
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
 use linear_algebra, ONLY:PAR_matrix
 !
 implicit none
 !
 character(1)               :: mode
 type(PAR_matrix), optional :: M_slk
 type(PAR_matrix), optional :: M_par
 type(BS_block),   optional :: M_bse(:)
 complex(SP),      optional :: M(:,:)
 integer,          optional :: i_MAT
 !
#if defined _SCALAPACK
 !
 if (present(M_slk)) then
   !
   if (present(M)) then
     !
     if (mode=="<") then
       !
       M_slk%blc(:M_slk%nrows_used,:M_slk%ncols_used)=M(M_slk%rows(1):M_slk%rows(2),M_slk%cols(1):M_slk%cols(2))
       !
     endif
     !
     if (mode==">") then
       !
       M=cZERO
       !
       M(M_slk%rows(1):M_slk%rows(2),M_slk%cols(1):M_slk%cols(2))=M_slk%blc(:M_slk%nrows_used,:M_slk%ncols_used)
       !
       call PP_redux_wait(M,COMM=SLK_POOL%INTRA_comm)
       !
     endif
     !
     return
     !
   endif
   !
   if (present(M_bse)) then
     !
     call PAR_matrix_transfer_K_to_SLK(M_bse,M_slk)
     !
   endif
   !
   if (present(M_par)) then
     !
     call PAR_matrix_transfer_PAR_to_SLK(M_par,M_slk)
     !
   endif
   !
 endif
 !
#endif
 !
 if (present(M_par)) then
   !
   if (present(M)) then
     !
     if (mode=="<") then
       !
       M=cZERO
       !
       if (present(i_MAT)) then
         M(M_par%rows(1):M_par%rows(2),M_par%cols(1):M_par%cols(2))=M_par%blc_3d(:,:,i_MAT)
       else 
         M(M_par%rows(1):M_par%rows(2),M_par%cols(1):M_par%cols(2))=M_par%blc(:,:)
       endif
       !
       call PP_redux_wait(M,COMM=M_par%COMM%COMM)
       !
     endif
     !
     return
     !
   endif
   !
 endif
 !
end SUBROUTINE
