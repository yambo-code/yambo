!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
! headers
!
#include<dev_defs.h>
#include<y_memory.h>
!
!
subroutine eval_minus_G()
 !
 ! Output: minus_G(ng_closed)
 !
 ! G_{minus_G(ig)} = -G_ig
 !
 use D_lattice,    ONLY:inv_index
 use R_lattice,    ONLY:ng_closed,g_rot,minus_G
 use R_lattice,    ONLY:DEV_VAR(minus_G)
 !
 USE_MEMORY
 !
 implicit none
 !
 ! Work Space
 !
 integer  :: ig
 !
 YAMBO_FREE_GPU(DEV_VAR(minus_G))
 YAMBO_FREE(minus_G)
 !
 YAMBO_ALLOC(minus_G,(ng_closed))
 !
 minus_G=0
 !
 if (inv_index>0) then
   do ig=1,ng_closed
     minus_G(ig)=g_rot(ig,inv_index)
   enddo
 else
   minus_G(1)=1
   if (ng_closed>1) call G_rot_grid(-1,'set_table')
 endif
 !
#ifdef _GPU
 YAMBO_ALLOC_GPU_SOURCE(DEV_VAR(minus_G),minus_G)
#endif
 !
end subroutine

