!
! License-Identifier: GPL
!
! Copyright (C) 2025 The Yambo Team
!
! Authors (see AUTHORS file for details): CA
!
subroutine k_small_symmetry_group(k,k_symm_grp)
 !
 ! Outputs:
 ! 
 !   k_symm_grp%nsymm, k_symm_grp%indx
 !
 !Find the small symmetry group for each k-point
 !
 use pars,           ONLY:SP
 use vec_operate,    ONLY:rlu_v_is_zero,c2a,k2bz
 use D_lattice,      ONLY:nsym
#include<y_memory.h>
 !
 type(bz_samp)::k
 type(k_symm_sm_grp), intent(out) :: k_symm_grp
 !
 if(allocated(k_symm_grp%nsymm)) then
   YAMBO_FREE(k_symm_grp%nsymm)
   YAMBO_FREE(k_symm_grp%indx)
 endif
 !
 YABMO_ALLOC(k_symm_grp%nsymm(k%nibz))
 YAMBO_ALLOC(k_symm_grp%indx(nsymm,k%nibz))
 !
 ! Initialize with only the idensity
 !
 k_symm_grp%nsymm=1
 k_symm_grp%idx  =1     
 !
end subroutine
