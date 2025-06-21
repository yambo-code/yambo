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

#ifdef _CUDAF
#  define DEV_SUBNAME(x)        CAT(x,_gpu)
#  define DEV_SUBNAME_ALT(x)    CAT(x,_gpu)
#  define DEV_VARNAME(x)        CAT(x,_d)
#  define DEV_ATTRIBUTE         , device
#  define DEV_PINNED            , pinned

#elif defined _OPENACC || defined _OPENMP_GPU
#  define DEV_SUBNAME(x)        CAT(x,_gpu)
#  define DEV_SUBNAME_ALT(x)    CAT(x,_cpu)
#  define DEV_VARNAME(x)        x
#  define DEV_ATTRIBUTE
#  define DEV_PINNED

#else
#  define DEV_SUBNAME(x)        x
#  define DEV_SUBNAME_ALT(x)    CAT(x,_cpu)
#  define DEV_VARNAME(x)        x
#  define DEV_ATTRIBUTE
#  define DEV_PINNED
#endif

#define DEV_SUB(x)          DEV_SUBNAME(x)
#define DEV_SUB_ALT(x)      DEV_SUBNAME_ALT(x)
#define DEV_VAR(x)          DEV_VARNAME(x)
#define DEV_ATTR            DEV_ATTRIBUTE
#define DEV_PIN             DEV_PINNED


#if defined _OPENACC
#  define DEV_ACC $acc
#  define DEV_ACC_DEBUG !!!!
#else
#  define DEV_ACC !!!!
#  define DEV_ACC_DEBUG !!!!
#endif

#if defined _CUDAF
#  define DEV_CUF $cuf
#else
#  define DEV_CUF !!!!
#endif

#if defined _OPENMP_GPU
#  define DEV_OMPGPU $omp
#else
#  define DEV_OMPGPU !!!!
#endif

#if defined _OPENMP && !defined (_GPU)
#  define DEV_OMP $omp
#else
#  define DEV_OMP !!!!
#endif
