/*
        Copyright (C) 2000-2020 the YAMBO team
              http://www.yambo-code.org

 Authors (see AUTHORS file for details): HM AM
 
 This file is distributed under the terms of the GNU 
 General Public License. You can redistribute it and/or 
 modify it under the terms of the GNU General Public 
 License as published by the Free Software Foundation; 
 either version 2, or (at your option) any later version.

 This program is distributed in the hope that it will 
 be useful, but WITHOUT ANY WARRANTY; without even the 
 implied warranty of MERCHANTABILITY or FITNESS FOR A 
 PARTICULAR PURPOSE.  See the GNU General Public License 
 for more details.

 You should have received a copy of the GNU General Public 
 License along with this program; if not, write to the Free 
 Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
 MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
 
*/
 use memory,     ONLY:MEM_err,MEM_msg,MEM_count,MEM_global_mesg,IPL
 implicit none

#if defined _MPI

 /* MPI only part */

#define CHECK_ALLOC_A(SIZE) \
  LOCAL_SIZE=SIZE NEWLINE \
  HOST_SIZE=SIZE NEWLINE \
  call PP_redux_wait(HOST_SIZE,COMM=PAR_COM_HOST%COMM) NEWLINE \
  if (PAR_COM_HOST%CPU_id==0 ) then

#define CHECK_ALLOC_B(x,HOST_SIZE) \
    call MEM_global_mesg(QUOTES x QUOTES,kind(x),HOST_SIZE) NEWLINE \
    deallocate(x) NEWLINE \
  endif NEWLINE \
  call PP_redux_wait(COMM=PAR_COM_HOST%COMM)


#define YAMBO_PAR_ALLOC_CHECK1(x,SIZE) \
  if (PAR_COM_HOST%n_CPU>1) then NEWLINE \
    CHECK_ALLOC_A(SIZE) NEWLINE \
    SIMPLE_ALLOC1(x,HOST_SIZE) NEWLINE \
    CHECK_ALLOC_B(x,HOST_SIZE) NEWLINE \
  endif
#define YAMBO_PAR_ALLOC_CHECK2(x,SIZE) \
  if (PAR_COM_HOST%n_CPU>1) then NEWLINE \
    CHECK_ALLOC_A(SIZE) NEWLINE \
    SIMPLE_ALLOC2(x,HOST_SIZE) NEWLINE \
    CHECK_ALLOC_B(x,HOST_SIZE) NEWLINE \
  endif
#define YAMBO_PAR_ALLOC_CHECK3(x,SIZE) \
  if (PAR_COM_HOST%n_CPU>1) then NEWLINE \
    CHECK_ALLOC_A(SIZE) NEWLINE \
    SIMPLE_ALLOC3(x,HOST_SIZE) NEWLINE \
    CHECK_ALLOC_B(x,HOST_SIZE) NEWLINE \
  endif
#define YAMBO_PAR_ALLOC_CHECK4(x,SIZE) \
  if (PAR_COM_HOST%n_CPU>1) then NEWLINE \
    CHECK_ALLOC_A(SIZE) NEWLINE \
    SIMPLE_ALLOC4(x,HOST_SIZE) NEWLINE \
    CHECK_ALLOC_B(x,HOST_SIZE) NEWLINE \
  endif
#define YAMBO_PAR_ALLOC_CHECK5(x,SIZE) \
  if (PAR_COM_HOST%n_CPU>1) then NEWLINE \
    CHECK_ALLOC_A(SIZE) NEWLINE \
    SIMPLE_ALLOC5(x,HOST_SIZE) NEWLINE \
    CHECK_ALLOC_B(x,HOST_SIZE) NEWLINE \
  endif
#define YAMBO_PAR_ALLOC_CHECK6(x,SIZE) \
  if (PAR_COM_HOST%n_CPU>1) then NEWLINE \
    CHECK_ALLOC_A(SIZE) NEWLINE \
    SIMPLE_ALLOC6(x,HOST_SIZE) NEWLINE \
    CHECK_ALLOC_B(x,HOST_SIZE) NEWLINE \
  endif

#else

 /* Without MPI */

#define YAMBO_PAR_ALLOC_CHECK1(x,SIZE)
#define YAMBO_PAR_ALLOC_CHECK2(x,SIZE)
#define YAMBO_PAR_ALLOC_CHECK3(x,SIZE)
#define YAMBO_PAR_ALLOC_CHECK4(x,SIZE)
#define YAMBO_PAR_ALLOC_CHECK5(x,SIZE)
#define YAMBO_PAR_ALLOC_CHECK6(x,SIZE)

#endif


#define YAMBO_PAR_ALLOC1(x,SIZE) \
  YAMBO_PAR_ALLOC_CHECK1(x,SIZE) NEWLINE \
  YAMBO_ALLOC1(x,LOCAL_SIZE)
#define YAMBO_PAR_ALLOC2(x,SIZE) \
  YAMBO_PAR_ALLOC_CHECK2(x,SIZE) NEWLINE \
  YAMBO_ALLOC2(x,LOCAL_SIZE)
#define YAMBO_PAR_ALLOC3(x,SIZE) \
  YAMBO_PAR_ALLOC_CHECK3(x,SIZE) NEWLINE \
  YAMBO_ALLOC3(x,LOCAL_SIZE)
#define YAMBO_PAR_ALLOC4(x,SIZE) \
  YAMBO_PAR_ALLOC_CHECK4(x,SIZE) NEWLINE \
  YAMBO_ALLOC4(x,LOCAL_SIZE)
#define YAMBO_PAR_ALLOC5(x,SIZE) \
  YAMBO_PAR_ALLOC_CHECK5(x,SIZE) NEWLINE \
  YAMBO_ALLOC5(x,LOCAL_SIZE)
#define YAMBO_PAR_ALLOC6(x,SIZE) \
  YAMBO_PAR_ALLOC_CHECK6(x,SIZE) NEWLINE \
  YAMBO_ALLOC6(x,LOCAL_SIZE)


#define SIMPLE_ALLOC1(x,SIZE) \
    SIMPLE_ALLOC(x,(SIZE(1)))
#define SIMPLE_ALLOC2(x,SIZE) \
    SIMPLE_ALLOC(x,(SIZE(1),SIZE(2)))
#define SIMPLE_ALLOC3(x,SIZE) \
    SIMPLE_ALLOC(x,(SIZE(1),SIZE(2),SIZE(3)))
#define SIMPLE_ALLOC4(x,SIZE) \
    SIMPLE_ALLOC(x,(SIZE(1),SIZE(2),SIZE(3),SIZE(4)))
#define SIMPLE_ALLOC5(x,SIZE) \
    SIMPLE_ALLOC(x,(SIZE(1),SIZE(2),SIZE(3),SIZE(4),SIZE(5)))
#define SIMPLE_ALLOC6(x,SIZE) \
    SIMPLE_ALLOC(x,(SIZE(1),SIZE(2),SIZE(3),SIZE(4),SIZE(5),SIZE(6)))

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


