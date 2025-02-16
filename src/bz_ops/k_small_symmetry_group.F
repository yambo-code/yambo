!
! License-Identifier: GPL
!
! Copyright (C) 2025 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!
subroutine k_small_symmetry_group(k)
 !
 ! Outputs:
 ! 
 !   k%star k%sstar k%nstar k%weights k%nbz
 !
 !Find the small symmetry group for each k-point
 !
 use pars,           ONLY:SP
 use vec_operate,    ONLY:rlu_v_is_zero,c2a,k2bz
 use D_lattice,      ONLY:nsym
#include<y_memory.h>
 !
 type(bz_samp)::k
 !
end subroutine
