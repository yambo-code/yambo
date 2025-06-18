!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AF
!

!
!====================
! scatter_Gamp (cpu)
!====================
!
#include<dev_defs_nogpu.h>
#undef _GPU_LOC
#include"scatter_Gamp.F"
#include<dev_undefs.h>

!
!====================
! scatter_Gamp_gpu 
!====================
!
! This includes a host-fallback when GPUs are not
! available
!
#if defined _GPU
#  include<dev_defs.h>
#  define _GPU_LOC
#  include"scatter_Gamp.F"
#else
#  include<dev_defs_nogpu.h>
#  undef _GPU_LOC
#  undef  DEV_SUB
#  define DEV_SUB(x)        CAT(x,_gpu)
#  include"scatter_Gamp.F"
#  include<dev_undefs.h>
#endif
