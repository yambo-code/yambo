/*
  License-Identifier: GPL
 
  Copyright (C) 2016 The Yambo Team
 
  Authors (see AUTHORS file for details): AF
*/


#ifdef __STDC__
#  define CAT(a,b) a##b 
#else
#  define PASTE(a) a
#  define CAT(a,b) PASTE(a)b
#endif

#  define DEV_SUBNAME(x)        x
#  define DEV_SUBNAME_ALT(x)    CAT(x,_cpu)
#  define DEV_VARNAME(x)        x
#  define DEV_ATTRIBUTE
#  define DEV_PINNED

#define DEV_SUB(x)          DEV_SUBNAME(x)
#define DEV_SUB_ALT(x)      DEV_SUBNAME_ALT(x)
#define DEV_VAR(x)          DEV_VARNAME(x)
#define DEV_ATTR            DEV_ATTRIBUTE
#define DEV_PIN             DEV_PINNED

!#define YAMBO_CUDA_OR_OMP(priv_list,nloop)  !$omp parallel do default(shared), private(priv_list), collapse(nloop)
!#define YAMBO_CUDA_OR_OMP_END               !$omp end parallel do

