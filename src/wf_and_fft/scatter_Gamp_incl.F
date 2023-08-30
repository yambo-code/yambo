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
#include<dev_defs_nocuda.h>
#undef _CUDA_LOC
#include"scatter_Gamp.F"
!
#include<dev_undefs.h>

!
!====================
! scatter_Gamp_gpu 
!====================
!
#if defined _CUDA

#include<dev_defs.h>
#define _CUDA_LOC
#include"scatter_Gamp.F"

#endif

