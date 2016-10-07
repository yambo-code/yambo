!
!        Copyright (C) 2000-2016 the YAMBO team
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
SUBROUTINE SLK_matrix_transfer( mode, M_slk, M_bse, M )
 !
 ! mode can be "fill_the_SLK" "transfer_from_the_SLK"
 !
 use pars,           ONLY:SP,cZERO
 use SLK_m,          ONLY:SLK_matrix,SLK_POOL
 use BS,             ONLY:BS_block
 use parallel_m,     ONLY:PP_redux_wait
 !
 implicit none
 !
 character(*)     :: mode
 type(SLK_matrix) :: M_slk
 type(BS_block), optional :: M_bse(:)
 complex(SP),    optional :: M(:,:)
 !
 if (mode=="fill_the_SLK") then
   !
   if (present(M)) then
     !
     M_slk%blc(:M_slk%nrows_used,:M_slk%ncols_used)=M(M_slk%rows(1):M_slk%rows(2),M_slk%cols(1):M_slk%cols(2))
     !
     return
     !
   endif
   !
 endif
 !
 if (mode=="transfer_from_the_SLK") then
   !
   if (present(M)) then
     !
     M=cZERO
     !
     M(M_slk%rows(1):M_slk%rows(2),M_slk%cols(1):M_slk%cols(2))=M_slk%blc(:M_slk%nrows_used,:M_slk%ncols_used)
     !
     call PP_redux_wait(M,COMM=SLK_POOL%INTRA_comm)
     !
     return
     !
   endif
   !
 endif
 ! 
end SUBROUTINE
