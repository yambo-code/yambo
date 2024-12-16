!
! License-Identifier: GPL
!
! Copyright (C) 2015 the Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
#include<y_memory.h>
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
