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
!
#include<dev_undefs.h>

!
!====================
! scatter_Gamp_gpu 
!====================
!
#if defined _GPU
#  include<dev_defs.h>
#  define _GPU_LOC
#  include"scatter_Gamp.F"
#endif

