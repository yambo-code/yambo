/*
        Copyright (C) 2000-2017 the YAMBO team
              http://www.yambo-code.org

 Authors (see AUTHORS file for details): HM
 
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

 ...Check if the allocations are sucessfull
 
*/
 integer :: alloc_err
/* 
 ...Check memory allocations 
*/
#if defined(CHECK_ALLOC)
/*
*/
#define YAMBO_ALLOCATE(x) \
 allocate(x, stat=alloc_err); \
 if (alloc_err) error('error in memory allocation')
#define YAMBO_DEALLOCATE_P(x) \
 if (associated(x)) then; \
   deallocate(x,stat=alloc_err); \
   nullify(x); \
 end if
 if (alloc_err) error('error in memory allocation')
#define YAMBO_DEALLOCATE_A(x) \
 if (allocated(x)) deallocate(x,stat=alloc_err) \
 if (alloc_err) error('error in memory allocation')
/*
  ...Don't check memory allocations 
*/
#else
#define YAMBO_ALLOCATE(x) allocate(x)
#define YAMBO_DEALLOCATE_P(x) \
 if(associated(x)) then; \
   deallocate(x,stat=alloc_err); \
   nullify(x); \
 end if
#define YAMBO_DEALLOCATE_A(x) \
 if (allocated(x)) then; \
   deallocate(x,stat=alloc_err); \
 end if
#endif

