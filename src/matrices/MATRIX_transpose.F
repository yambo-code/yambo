!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine MATRIX_transpose( operation, M_in, M_out )
 !
 ! performs the transpose (or transpose conjugate) of a distributed
 ! matrix.
 !
 ! operation = "T" | "TC" 
 !
 ! M_in is overwritten in output unless M_out is present
 !
 use interfaces,     ONLY:MATRIX_duplicate
 use matrix,         ONLY:PAR_matrix,MATRIX_reset
#include<memory.h>
 !
 character(*)               :: operation
 type(PAR_matrix), optional :: M_in
 type(PAR_matrix)           :: M_out
 !
 ! Work Space
 !
 type(PAR_matrix) :: M_tmp
 !
 ! note that here the parallel distribution is changed
 ! is order to minimize (i.e. avoid) data transfer
 !
 if (trim(operation)/="T".and.trim(operation)/="TC") &
&  call error("[MATRIX] invalid operation in MATRIX_transpose")
 !
 call MATRIX_reset(M_tmp)
 !
 if (present(M_in)) then
   call MATRIX_duplicate(operation, M_in=M_in, M_out=M_tmp)
 else
   call MATRIX_duplicate(operation, M_in=M_out, M_out=M_tmp)
 endif
 !
 call MATRIX_reset(M_out)
 call MATRIX_duplicate("+1", M_in=M_tmp, M_out=M_out)
 !
 call MATRIX_reset(M_tmp)
 !
end subroutine
