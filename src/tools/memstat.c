/*
  Copyright (C) 2002 FPMD group
  This file is distributed under the terms of the
  GNU General Public License. See the file `License'
  in the root directory of the present distribution,
  or http://www.gnu.org/copyleft/gpl.txt .
*/

#include <wrapper.h>
#include <have_malloc.h>

/* 
  This function return the numer of kilobytes allocated by the calling process. 
  Author: Carlo Cavazzoni.
*/

#if defined (__SVR4) && defined (__sun)
#define SUN_MALLINFO
#endif

#if defined(HAVE_MALLINFO) && !defined(__QK_USER__) && !defined(SUN__MALLINFO) 
#include <malloc.h>

void C_FUNC(memstat,MEMSTAT)(int *kilobytes)
{

  struct mallinfo info;  
  info = mallinfo();

#if defined(__AIX)
  *kilobytes = (info.arena) / 1024 ;
#else
/*
    arena+hblkhd             total taken from the system
    uordblks+usmblks+hblkhd  total in use by program
    fordblks+fsmblks         total free within program

*/
  *kilobytes = (info.arena + info.hblkhd) / 1024 ;
#endif

#else
void C_FUNC(memstat,MEMSTAT)(int *kilobytes)
{
  *kilobytes = -1;
#endif
}
