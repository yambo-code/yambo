/*
  License-Identifier: GPL
 
  Copyright (C) 2016 The Yambo Team
 
  Authors (see AUTHORS file for details): HM AM
 
*/

 use pars,         ONLY:IPL
 use y_memory,     ONLY:MEM_err,MEM_msg,MEM_count,MEM_global_mesg

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
  allocate(x SIZE, &NEWLINE& stat=MEM_err,errmsg=MEM_msg)  NEWLINE \

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

#define YAMBO_FREE(x) \
  if (.not.allocated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,int(-1,KIND=IPL))NEWLINE \
  if (     allocated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,size(x,KIND=IPL))NEWLINE \
  if (     allocated(x)) &NEWLINE& deallocate(x)
#define YAMBO_FREE_P(x) \
  if (.not.associated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,int(-1,KIND=IPL))NEWLINE \
  if (     associated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,size(x,KIND=IPL))NEWLINE \
  if (     associated(x)) &NEWLINE& deallocate(x);nullify(x)
