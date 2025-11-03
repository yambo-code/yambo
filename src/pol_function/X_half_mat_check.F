!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine X_half_mat_check(Xen,Xk,Xw)
 !
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:i_space_inv
 use X_m,           ONLY:X_FILL_UP_matrix_only
 !
 implicit none
 !
 type(levels) ,intent(in)   :: Xen
 type(bz_samp),intent(in)   :: Xk
 type(w_samp) ,intent(in)   :: Xw
 !
 if (i_space_inv==1) then
   X_FILL_UP_matrix_only= .TRUE.
 else
   X_FILL_UP_matrix_only= all( aimag(Xw%p(:))<1.E-4 ).and. all( real(Xw%p(:))<1.E-4 )
 endif
 !
end subroutine
