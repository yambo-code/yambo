!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AF
!
!====================
! scatter_Bamp (cpu)
!====================
!
#include<dev_defs_nogpu.h>
#undef _GPU_LOC
#include"scatter_Bamp.F"
#include<dev_undefs.h>

!
!====================
! scatter_Bamp_gpu 
!====================
!
! This includes a host-fallback when GPUs are not
! available
!
#if defined _GPU
#  include<dev_defs.h>
#  define _GPU_LOC
#  include"scatter_Bamp.F"
#else
#  include<dev_defs_nogpu.h>
#  undef _GPU_LOC
#  define DEV_SUB(x)        CAT(x,_gpu)
#  include"scatter_Bamp.F"
#  include<dev_undefs.h>
#endif
