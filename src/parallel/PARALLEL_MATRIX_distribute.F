!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_MATRIX_distribute(COMM,PAR_IND,nb,PAR_index,PAR_ID,PAR_n_elements)
 !
 use parallel_m,    ONLY:yMPI_comm,PP_indexes
 use parallel_int,  ONLY:PARALLEL_index
 use hamiltonian,   ONLY:B_mat_index
 !
 implicit none
 !
 type(yMPI_comm)   :: COMM
 type(PP_indexes) :: PAR_IND
 integer          :: nb(2)
 !
 integer, optional:: PAR_index(:,:)
 integer, optional:: PAR_ID
 integer, optional:: PAR_n_elements
 !
 ! Work space
 !
 integer        :: ib1,ib2,Nel
 !
 call PARALLEL_index(PAR_IND,(/ (nb(2)-nb(1)+1)**2 /),COMM=COMM,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 !
 if (present(PAR_ID)) then
   PAR_ID=COMM%CPU_id
 endif
 !
 if (present(PAR_index)) then
   Nel=PAR_IND%n_of_elements(COMM%CPU_id+1)
   PAR_index=0
   Nel=0
   do ib1=nb(1),nb(2)
     do ib2=nb(1),nb(2)
       if (PAR_IND%element_1D( B_mat_index(ib1,ib2,nb) ) ) then
         Nel=Nel+1
         PAR_index(ib1,ib2)=Nel
       endif
     enddo
   enddo
 endif
 !
 if (present(PAR_n_elements)) then
   PAR_n_elements=PAR_IND%n_of_elements(COMM%CPU_id+1)
 endif
 !
end subroutine
