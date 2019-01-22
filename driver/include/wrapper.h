/*
         Copyright (C) 2000-2019 the YAMBO team
               http://www.yambo-code.org
 
  Authors (see AUTHORS file for details): DS
  
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
#include <macros.h>
/* 
 Command line options
*/
 #include "yambo.h"
/* TEST */
#if defined _TEST_MAIN
 #if defined _FORTRAN_US
  int driver_
 #else
  int driver
 #endif
#endif
/* YAMBO */
#if defined _YAMBO_MAIN
 #include "yambo.h"
 #if defined _FORTRAN_US
  int yambo_driver_
 #else
  int yambo_driver
 #endif
#endif
/* YPP */
#if defined _YPP_MAIN
 #include "ypp.h"
 #if defined _FORTRAN_US
  int ypp_i_
 #else
  int ypp_i
 #endif
#endif
/* A2Y */
#if defined _a2y
 #include "a2y.h"
 #if defined _FORTRAN_US
  int a2y_i_
 #else
  int a2y_i
 #endif
#endif
/* C2Y */
#if defined _c2y
 #include "c2y.h"
 #if defined _FORTRAN_US
  int c2y_i_
 #else
  int c2y_i
 #endif
#endif
/* P2Y */
#if defined _p2y
 #include "p2y.h"
 #if defined _FORTRAN_US
  int p2y_i_
 #else
  int p2y_i
 #endif
#endif
/* E2Y */
#if defined _e2y
 #include "e2y.h"
 #if defined _FORTRAN_US
  int e2y_i_
 #else
  int e2y_i
 #endif
#endif
 (int *, int *,int *,int *,int *,int *,int *,int *,
  char *string, char *in_file, char *in_dir, char *out_dir, char *com_dir, char *job,
  int string_N, int in_file_N, int in_dir_N, int out_dir_N, int com_dir_N, int job_N);
