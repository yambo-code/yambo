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
 use memory, ONLY:MEM_err,MEM_msg,MEM_count,IPL
 implicit none

#define YAMBO_ALLOC_P(x,SIZE) \
  allocate(x SIZE, &NEWLINE& stat=MEM_err,errmsg=MEM_msg)NEWLINE \
  if (     associated(x)) &NEWLINE& call MEM_count(QUOTES x QUOTES,x)NEWLINE \
  if (.not.associated(x)) &NEWLINE& call MEM_error(QUOTES x QUOTES)
#define YAMBO_ALLOC(x,SIZE) \
  allocate(x SIZE,  &NEWLINE& stat=MEM_err,errmsg=MEM_msg)NEWLINE \
  if (     allocated(x)) &NEWLINE& call MEM_count(QUOTES x QUOTES,x)NEWLINE \
  if (.not.allocated(x)) &NEWLINE& call MEM_error(QUOTES x QUOTES)

#define YAMBO_FREE(x) \
  if (.not.allocated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,int(-1,KIND=IPL))NEWLINE \
  if (     allocated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,size(x,KIND=IPL))NEWLINE \
  if (     allocated(x)) &NEWLINE& deallocate(x)
#define YAMBO_FREE_P(x) \
  if (.not.associated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,int(-1,KIND=IPL))NEWLINE \
  if (     associated(x)) &NEWLINE& call MEM_free(QUOTES x QUOTES,size(x,KIND=IPL))NEWLINE \
  if (     associated(x)) &NEWLINE& deallocate(x);nullify(x)

#if defined _MPI
#define YAMBO_PAR_ALLOC(x,SIZE) \
  if (PAR_COM_HOST%n_CPU>1) then NEWLINE \
    HOST_SIZE=SIZE NEWLINE \
    call PP_redux_wait(HOST_SIZE,COMM=PAR_COM_HOST%COMM) NEWLINE \
    if (.and.PAR_COM_HOST%CPU_id==0 ) then NEWLINE \
      allocate(x HOST_SIZE,stat=MEM_err,errmsg=MEM_msg)  NEWLINE \
      call MEM_global_msg(QUOTES x QUOTES) NEWLINE \
      deallocate(x) NEWLINE \
    endif NEWLINE \
    call PP_redux_wait(COMM=PAR_COM_HOST%COMM)  NEWLINE \
  endif NEWLINE \
  YAMBO_ALLOC(x,SIZE)
#endif
