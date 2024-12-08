!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module common_sc_rt
 !
 implicit none
 !
 interface 
   !
   subroutine PARALLEL_MATRIX_distribute(COMM,PAR_IND,nb,PAR_index,PAR_ID,PAR_n_elements)
     use parallel_m,    ONLY:yMPI_comm,PP_indexes
     type(yMPI_comm)   :: COMM
     type(PP_indexes) :: PAR_IND
     integer          :: nb(2)
     integer, optional:: PAR_index(:,:)
     integer, optional:: PAR_ID
     integer, optional:: PAR_n_elements
   end subroutine
   !
 end interface 
 !
end module common_sc_rt
