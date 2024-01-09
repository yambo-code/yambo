!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!
module Overlaps_m
 !
 use pars,  ONLY:SP
#include<memory.h>
 !
 complex(SP), allocatable :: S_det(:,:)        
 complex(SP), allocatable :: Sm1_plus(:,:,:,:)
 complex(SP), allocatable :: Sm1_minus(:,:,:,:)
 !
 interface
   !
   integer function io_Overlaps(X,Xen,ID,DIP_S)
     !
     use pars,          ONLY:SP
     use X_m,           ONLY:X_t
     use electrons,     ONLY:levels,n_sp_pol
     use R_lattice,     ONLY:nXkbz
     use matrix_operate,ONLY:mat_c2r,mat_r2c
     !
     implicit none
     type(X_t)   ::X
     type(levels)::Xen
     integer     ::ID
     complex(SP),  optional :: DIP_S(X%ib(2),X%ib(2),Xen%nk,3,n_sp_pol)
     !
   end function io_Overlaps
     !
 end interface
 !
 contains
   !
   subroutine S_alloc(Xen,Xk)
     !
     use R_lattice, ONLY:bz_samp
     use electrons, ONLY:levels
     use drivers,   ONLY:l_real_time
     !
     type(bz_samp), intent(in) :: Xk
     type(levels),  intent(in) :: Xen
     !
     YAMBO_ALLOC(S_det,(3,Xk%nbz))
     YAMBO_ALLOC(Sm1_plus,(3,maxval(Xen%nbf),maxval(Xen%nbf),Xk%nbz))
     YAMBO_ALLOC(Sm1_minus,(3,maxval(Xen%nbf),maxval(Xen%nbf),Xk%nbz))
     !
   end subroutine S_alloc
   !
   subroutine S_dealloc()
     !
     use drivers,   ONLY:l_real_time
     !
     YAMBO_FREE(S_det)
     YAMBO_FREE(Sm1_plus)
     YAMBO_FREE(Sm1_minus)
     !
   end subroutine S_dealloc    
   !
end module
