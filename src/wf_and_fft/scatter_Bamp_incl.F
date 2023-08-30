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
#include<dev_defs_nocuda.h>
#undef _CUDA_LOC
#include"scatter_Bamp.F"
#include<dev_undefs.h>
!
!====================
! scatter_Bamp_gpu 
!====================
!
#if defined _CUDA
#include<dev_defs.h>
#define _CUDA_LOC
#include"scatter_Bamp.F"
#endif
