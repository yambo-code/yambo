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
SUBROUTINE MATRIX_duplicate( operation, M_in, M_out )
 !
 ! operation can be "none/+1/-1/-C(= -1 * conjg)"
 !
 use pars,           ONLY:cZERO
 use SLK_m,          ONLY:SLK_matrix,SLK_matrix_reset
 !
 implicit none
 !
 character(*)     :: operation
 type(SLK_matrix) :: M_in,M_out
 !
 ! Work Space
 !
 integer :: BLK_size
 !
 BLK_size=size(M_in%blc,1)
 call SLK_matrix_reset(M_out,BLK_size,BLK_size)
 !
 M_out%N          = M_in%N         
 M_out%desc       = M_in%desc      
 M_out%lwork      = M_in%lwork
 M_out%lrwork     = M_in%lrwork
 M_out%liwork     = M_in%liwork
 M_out%info       = M_in%info
 M_out%rows       = M_in%rows
 M_out%cols       = M_in%cols
 M_out%nrows      = M_in%nrows
 M_out%ncols      = M_in%ncols
 M_out%nrows_used = M_in%nrows_used
 M_out%ncols_used = M_in%ncols_used
 !
 M_out%blc=cZERO
 !
 if (operation=="none") return
 !
 if (operation=="+1") then
   M_out%blc(:M_out%nrows_used,:M_out%ncols_used)=M_in%blc(:M_in%nrows_used,:M_in%ncols_used)
 else if (operation=="-1") then
   M_out%blc(:M_out%nrows_used,:M_out%ncols_used)=-M_in%blc(:M_in%nrows_used,:M_in%ncols_used)
 else if (operation=="-C") then
   M_out%blc(:M_out%nrows_used,:M_out%ncols_used)=-conjg(M_in%blc(:M_in%nrows_used,:M_in%ncols_used))
 endif
 !
end SUBROUTINE
