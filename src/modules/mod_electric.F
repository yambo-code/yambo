! 
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!
module electric
 !
 use pars,       ONLY:SP,schlen,lchlen
 use electrons,  ONLY:levels
 !
#include<memory.h>
 !
 complex(SP)            :: NL_initial_P(3) ! in principle it can be different from zero
 !
 ! Variable for overlaps and Berry phase
 !
 complex(SP), allocatable :: All_S_det(:,:,:)
 complex(SP), allocatable :: Sm1_plus(:,:,:,:,:)
 complex(SP), allocatable :: Sm1_minus(:,:,:,:,:)
 !
 ! Electric Field operator used in SC
 !
 complex(SP), allocatable :: W_electric(:,:,:,:)
 !
 logical  :: l_force_SndOrd  ! Use second order dipoles as in the old code (default 4th order)
 !
 ! Polarization
 !
 logical :: l_P_periodic ! Enforce periodicity in the polarization
 !
 contains
   !
   subroutine ELECTRIC_alloc(en)
     !
     use pars,           ONLY:SP
     use electrons,      ONLY:levels,n_sp_pol
     use R_lattice,      ONLY:nXkbz
     !
     implicit none
     !
     type(levels), intent(in) :: en
     !
     YAMBO_ALLOC(All_S_det,(6,nXkbz,n_sp_pol))
     YAMBO_ALLOC(Sm1_plus,(maxval(en%nbf),maxval(en%nbf),6,nXkbz,n_sp_pol))
     YAMBO_ALLOC(Sm1_minus,(maxval(en%nbf),maxval(en%nbf),6,nXkbz,n_sp_pol))
     !
   end subroutine ELECTRIC_alloc
   
   subroutine ELECTRIC_free
     !
     implicit none
     !
     YAMBO_FREE(All_S_det)
     YAMBO_FREE(Sm1_plus)
     YAMBO_FREE(Sm1_minus)
     !
   end subroutine ELECTRIC_free
   !
end module
