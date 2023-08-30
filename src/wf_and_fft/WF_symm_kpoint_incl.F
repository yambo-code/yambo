!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AF
!

!
!====================
! WF_symm_kpoint_cpu
!====================
!
#include<dev_defs_nocuda.h>
#undef _CUDA_LOC
#include"WF_symm_kpoint.F"
!
#include<dev_undefs.h>

!
!====================
! WF_symm_kpoint_gpu
!====================
!
#if defined _CUDA

#include<dev_defs.h>
#define _CUDA_LOC
#include"WF_symm_kpoint.F"

#endif

