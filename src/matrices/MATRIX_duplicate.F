!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine MATRIX_duplicate( operation, M_in, INDEX_in, M_out )
 !
 ! operation can be "none/free/+1/-1/-C/+T/+TC"
 !
 use parallel_m,     ONLY:COMM_copy
 use pars,           ONLY:cZERO,SP
 use matrix,         ONLY:PAR_matrix,MATRIX_reset
#include<memory.h>
 !
 character(*)               :: operation
 type(PAR_matrix), optional :: M_in
 type(PAR_matrix)           :: M_out
 integer         , optional :: INDEX_in
 !
 ! Work Space
 !
 integer     :: i_row,i_col,i_r,i_c,i_b,Nb_i(2),Nb_o(2)
 real(SP)    :: pre_factor
 !
 call MATRIX_reset(M_out)
 !
 if (operation=="free") then
   YAMBO_FREE(M_out%blc)
   return
 endif
 !
 M_out%kind       = M_in%kind      
 M_out%I          = M_in%I
 M_out%N          = M_in%N         
 M_out%desc       = M_in%desc      
 M_out%lwork      = M_in%lwork
 M_out%lrwork     = M_in%lrwork
 M_out%liwork     = M_in%liwork
 M_out%info       = M_in%info
 !
 ! Dimensions
 !
 M_out%nrows      = M_in%nrows
 M_out%ncols      = M_in%ncols
 M_out%rows       = M_in%rows
 M_out%cols       = M_in%cols
 M_out%BLCnrows   = M_in%BLCnrows
 M_out%BLCncols   = M_in%BLCncols
 M_out%BLCrows    = M_in%BLCrows
 M_out%BLCcols    = M_in%BLCcols
 if (operation=="+TC".or.operation=="+T") then
   M_out%rows       = M_in%cols
   M_out%cols       = M_in%rows
   M_out%nrows      = M_in%ncols
   M_out%ncols      = M_in%nrows
   M_out%BLCrows    = M_in%BLCcols
   M_out%BLCcols    = M_in%BLCrows
   M_out%BLCnrows   = M_in%BLCncols
   M_out%BLCncols   = M_in%BLCnrows
 endif
 !
 Nb_i=(/1, M_in%Nb /)
 Nb_o=(/1, M_out%Nb/)
 if (present(INDEX_in)) then
   Nb_i=(/INDEX_in,INDEX_in/)
   Nb_o=(/1,1/)
   M_out%I =1
 endif
 M_out%Nb = Nb_o(2)-Nb_o(1)+1
 !
 YAMBO_ALLOC(M_out%blc,(M_out%BLCrows(1):M_out%BLCrows(2),M_out%cols(1):M_out%BLCcols(2),Nb_o(1):Nb_o(2)))
 M_out%blc=cZERO
 !
 call COMM_copy(M_in%INTER_comm,M_out%INTER_comm)
 call COMM_copy(M_in%INTRA_comm,M_out%INTRA_comm)
 !
 if (operation=="none") return
 !
 pre_factor=1._SP
 if (index(operation,"-")>0 ) pre_factor=-1._SP
 !
#if defined _OPENMP
 !$omp parallel default(shared), private(i_row,i_col,i_r,i_c,i_b)
 !$omp do
#endif
 do i_row=M_in%rows(1),M_in%rows(2)
   do i_col=M_in%cols(1),M_in%cols(2)
     do i_b=Nb_i(1),Nb_i(2)
       !
       i_r=i_row
       i_c=i_col
       !
       if (index(operation,"T")>0) then
         i_r=i_col
         i_c=i_row
       endif
       !
       if (index(operation,"C")>0 ) then
         M_out%blc(i_r,i_c,i_b+Nb_o(1)-Nb_i(1))=pre_factor*conjg(M_in%blc(i_row,i_col,i_b))
       else
         M_out%blc(i_r,i_c,i_b+Nb_o(1)-Nb_i(1))=pre_factor*M_in%blc(i_row,i_col,i_b)
       endif
       !
     enddo
   enddo
 enddo
#if defined _OPENMP
 !$omp enddo
 !$omp end parallel
#endif
 !
end subroutine
