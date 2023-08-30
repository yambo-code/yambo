!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM 
!
module RT_iterative
 !
 use pars,           ONLY:SP
 !
 ! Beyond Complete Collision Approx 
 !----------------------------------
 logical     :: l_RT_iterative_ELPH_lifetimes=.FALSE.
 ! ... THETA matrix
 complex(SP), allocatable :: THETA_matrix(:,:,:,:)
 ! ... indexes ...
 integer,     allocatable :: THETA_matrix_index(:,:,:)
 !
end module
