/*
         Copyright (C) 2000-2020 the YAMBO team
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
/* 
 Tool drivers 
*/
/* YAMBO */
#if defined _yambo
 #if defined _FORTRAN_US
  int yambo_
 #else
  int yambo
 #endif
#endif
/* YPP */
#if defined _ypp
 #if defined _FORTRAN_US
  int ypp_
 #else
  int ypp
 #endif
#endif
/* A2Y */
#if defined _a2y
 #if defined _FORTRAN_US
  int a2y_
 #else
  int a2y
 #endif
#endif
/* C2Y */
#if defined _c2y
 #if defined _FORTRAN_US
  int c2y_
 #else
  int c2y
 #endif
#endif
/* P2Y */
#if defined _p2y
 #if defined _FORTRAN_US
  int p2y_
 #else
  int p2y
 #endif
#endif
/* E2Y */
#if defined _e2y
 #if defined _FORTRAN_US
  int e2y_
 #else
  int e2y
 #endif
#endif
 (int *, int *,int *,int *,int *,int *,int *,int *,
  char *string, char *in_file, char *in_dir, char *out_dir, char *com_dir, char *job,
  int string_N, int in_file_N, int in_dir_N, int out_dir_N, int com_dir_N, int job_N);
