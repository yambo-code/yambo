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
 use R_lattice,      ONLY:bz_samp,k_symm_small_grp
 !     
#include<y_memory.h>
 !
 type(bz_samp), intent(in) ::k
 type(k_symm_small_grp), intent(out) :: k_symm_grp
 !
 ! Work space
 !
 integer :: ik
 !
 if (.not.allocated(k%sstar)) call k_expand(k)
 !
 if(allocated(k_symm_grp%nsym)) then
   YAMBO_FREE(k_symm_grp%nsym)
   YAMBO_FREE(k_symm_grp%indx)
 endif
 !
 YAMBO_ALLOC(k_symm_grp%nsym,(k%nibz))
 YAMBO_ALLOC(k_symm_grp%indx,(nsym,k%nibz))
 !
 ! Initialize with only the idensity
 !
 k_symm_grp%nsym=1
 k_symm_grp%indx=1     
 !
 write(*,*) 'size nsymm ',nsym
 do ik=1,k%nibz
   write(*,*) ik,k%nstar(ik)
 enddo
 !
end subroutine
