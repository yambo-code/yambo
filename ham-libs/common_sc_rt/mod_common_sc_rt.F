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
   subroutine PARALLEL_global_indexes_ham(E,Xk,q,ENVIRONMENT,X,Dip,RESET,Dip_limits_pre_defined)
     use X_m,           ONLY:X_t
     use DIPOLES,       ONLY:DIPOLE_t
     use electrons,     ONLY:levels
     use R_lattice,     ONLY:bz_samp
     implicit none
     type(levels)             ::E
     type(bz_samp)            ::Xk,q
     character(*)             ::ENVIRONMENT
     type(X_t),       optional::X
     type(DIPOLE_t),  optional::Dip
     logical,         optional::RESET,Dip_limits_pre_defined
   end subroutine PARALLEL_global_indexes_ham
   !
 end interface 
 !
end module common_sc_rt
