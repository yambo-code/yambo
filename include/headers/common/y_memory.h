/*
  License-Identifier: GPL
 
  Copyright (C) 2016 The Yambo Team
 
  Authors (see AUTHORS file for details): HM AM
 
*/

 use pars,         ONLY:IPL
 use y_memory,     ONLY:MEM_err,MEM_msg,MEM_count,MEM_global_mesg
#if defined _OPENACC || defined _OPENMP_GPU
 use y_memory,     ONLY:MEM_count_d
 use devxlib,      ONLY:devxlib_map,devxlib_unmap,devxlib_mapped,devxlib_memcpy_h2d
#endif

 implicit none

#define YAMBO_ALLOC1(x,SIZE) \
    YAMBO_ALLOC(x,(SIZE(1)))
#define YAMBO_ALLOC2(x,SIZE) \
    YAMBO_ALLOC(x,(SIZE(1),SIZE(2)))
#define YAMBO_ALLOC3(x,SIZE) \
    YAMBO_ALLOC(x,(SIZE(1),SIZE(2),SIZE(3)))
#define YAMBO_ALLOC4(x,SIZE) \
    YAMBO_ALLOC(x,(SIZE(1),SIZE(2),SIZE(3),SIZE(4)))
#define YAMBO_ALLOC5(x,SIZE) \
    YAMBO_ALLOC(x,(SIZE(1),SIZE(2),SIZE(3),SIZE(4),SIZE(5)))
#define YAMBO_ALLOC6(x,SIZE) \
    YAMBO_ALLOC(x,(SIZE(1),SIZE(2),SIZE(3),SIZE(4),SIZE(5),SIZE(6)))

#define SIMPLE_ALLOC(x,SIZE) \
  allocate(x SIZE, &NEWLINE& stat=MEM_err,errmsg=MEM_msg)

#define YAMBO_ALLOC_P(x,SIZE) \
  allocate(x SIZE, &NEWLINE& stat=MEM_err,errmsg=MEM_msg)NEWLINE \
  if (     associated(x)) &NEWLINE& call MEM_count(QUOTES x QUOTES,x)NEWLINE \
  if (.not.associated(x)) &NEWLINE& call MEM_error(QUOTES x QUOTES)

#define YAMBO_ALLOC_CHECK(x) \
  if (     allocated(x)) &NEWLINE& call MEM_count(QUOTES x QUOTES,x)NEWLINE \
  if (.not.allocated(x)) &NEWLINE& call MEM_error(QUOTES x QUOTES)
#define YAMBO_ALLOC(x,SIZE) \
  allocate(x SIZE,  &NEWLINE& stat=MEM_err,errmsg=MEM_msg)NEWLINE \
  YAMBO_ALLOC_CHECK(x)
#define YAMBO_ALLOC_SOURCE(x,y) \
  allocate(x, source=y,  &NEWLINE& stat=MEM_err,errmsg=MEM_msg)NEWLINE \
  YAMBO_ALLOC_CHECK(x)
#define YAMBO_ALLOC_MOLD(x,y) \
  allocate(x, mold=y,  &NEWLINE& stat=MEM_err,errmsg=MEM_msg)NEWLINE \
  YAMBO_ALLOC_CHECK(x)

 /* free */

#define YAMBO_FREE_NO_DEV_CHECK(x) \
  if (.not.allocated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,int(-1,KIND=IPL))NEWLINE \
  if (     allocated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,size(x,KIND=IPL))NEWLINE \
  if (     allocated(x)) &NEWLINE& deallocate(x)

#if defined _OPENACC || defined _OPENMP_GPU
#define YAMBO_FREE(x) \
  if ( .not.allocated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,int(-1,KIND=IPL))NEWLINE \
  if (      allocated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,size(x,KIND=IPL))NEWLINE \
  if ( devxlib_mapped(x)) &NEWLINE& call error(QUOTES Trying to deallocate var x still on device memory QUOTES)NEWLINE \
  if (      allocated(x)) &NEWLINE& deallocate(x)
#else
#define YAMBO_FREE(x) YAMBO_FREE_NO_DEV_CHECK(x)
#endif

#define YAMBO_FREE_P(x) \
  if (.not.associated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,int(-1,KIND=IPL))NEWLINE \
  if (     associated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,size(x,KIND=IPL))NEWLINE \
  if (     associated(x)) &NEWLINE& deallocate(x);nullify(x)

 /* device allocations, OPENACC/OPENMP_GPU, CUDAF */

#if defined _OPENACC || defined _OPENMP_GPU

#define YAMBO_ALLOC_CHECK_GPU(x) \
  if (     allocated(x)) &NEWLINE& call MEM_count_d(QUOTES x QUOTES,x)NEWLINE \
  if (.not.allocated(x)) &NEWLINE& call MEM_error(QUOTES x QUOTES)
#define YAMBO_ALLOC_GPU(x,SIZE) \
  if (.not.allocated(x)) then NEWLINE YAMBO_ALLOC(x,SIZE) NEWLINE \
  endif NEWLINE \
  call devxlib_map(x) NEWLINE \
  YAMBO_ALLOC_CHECK_GPU(x)
#define YAMBO_ALLOC_GPU_SOURCE(x,y) \
   if (.not.allocated(x)) then NEWLINE call error("[ALLOC] x not allocated") NEWLINE	\
  endif NEWLINE \
  call devxlib_map(x) NEWLINE \
  call devxlib_memcpy_h2d(x,y) NEWLINE \
  YAMBO_ALLOC_CHECK_GPU(x)
#define YAMBO_ALLOC_GPU_MOLD(x,y) \
  if (.not.allocated(x)) then NEWLINE YAMBO_ALLOC_MOLD(x,y) NEWLINE \
  endif NEWLINE \
  call devxlib_map(x) NEWLINE \
  YAMBO_ALLOC_CHECK_GPU(x)

#define YAMBO_FREE_GPU(x) \
  if (.not.allocated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,int(-1,KIND=IPL))NEWLINE \
  if (     allocated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,size(x,KIND=IPL))NEWLINE \
  if (     allocated(x)) &NEWLINE& call devxlib_unmap(x,MEM_err)

#else

#define YAMBO_ALLOC_GPU(x,SIZE) \
  allocate(x SIZE,  &NEWLINE& stat=MEM_err,errmsg=MEM_msg)NEWLINE \
  YAMBO_ALLOC_CHECK(x)
#define YAMBO_ALLOC_GPU_SOURCE(x,y) \
  allocate(x, source=y,  &NEWLINE& stat=MEM_err,errmsg=MEM_msg)NEWLINE \
  YAMBO_ALLOC_CHECK(x)
#define YAMBO_ALLOC_GPU_MOLD(x,y) \
  allocate(x, mold=y,  &NEWLINE& stat=MEM_err,errmsg=MEM_msg)NEWLINE \
  YAMBO_ALLOC_CHECK(x)

#define YAMBO_FREE_GPU(x) \
  if (.not.allocated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,int(-1,KIND=IPL))NEWLINE \
  if (     allocated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,size(x,KIND=IPL))NEWLINE \
  if (     allocated(x)) &NEWLINE& deallocate(x)

#endif
