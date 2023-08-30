!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine eval_minus_G()
 !
 ! Output: minus_G(ng_closed)
 !
 ! G_{minus_G(ig)} = -G_ig
 !
 use D_lattice,    ONLY:inv_index
 use R_lattice,    ONLY:ng_closed,g_rot,minus_G
#ifdef _CUDA
 use R_lattice,    ONLY:minus_G_d
#endif
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer  :: ig
 !
 YAMBO_FREE(minus_G)
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
#ifdef _CUDA
 YAMBO_FREE(minus_G_d)
 allocate(minus_G_d,source=minus_G)
#endif
 !
end subroutine
