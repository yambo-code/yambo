!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AF
!

!
!====================
! WF_apply_symm_cpu
!====================
!
#include<dev_defs_nogpu.h>
#undef _GPU_LOC
#include"WF_apply_symm.F"
!
#include<dev_undefs.h>

!
!====================
! WF_apply_symm_gpu 
!====================
!
#if defined _GPU

#include<dev_defs.h>
#define _GPU_LOC
#include"WF_apply_symm.F"

#endif

