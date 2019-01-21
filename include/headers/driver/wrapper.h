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

/* 
 F90 wrapper
*/
#if defined _FORTRAN_US
 #define F90_FUNC(name) name ## _
#else
 #define F90_FUNC(name) name
#endif

/* 
 Test driver
*/
#if defined _FORTRAN_US
 int driver_
#else
 int driver
#endif
 (int *, int *,int *,int *,int *,int *,int *,int *,
  char *string, char *in_file, char *in_dir, char *out_dir, char *com_dir, char *job,
  int string_N, int in_file_N, int in_dir_N, int out_dir_N, int com_dir_N, int job_N);

/* 
 Yambo/Ypp driver flag
*/
#if defined _yambo  || _ELPH || _SC  || _RT || _QED || _NL
 #define _YAMBO_MAIN
#endif
#if defined _MAGNETIC || _KERR || _SURF
 #define _YAMBO_MAIN
#endif
#if defined _ypp  || _YPP_ELPH || _YPP_RT || _YPP_SC || _YPP_NL || _YPP_MAGNETIC || _YPP_SURF
 #define _YPP_MAIN
#endif

/* 
 Command line options
*/
/* TEST */
#if defined _TEST_MAIN
 char *tool="yambo";
 char *tool_desc="A shiny pot of fun and happiness [C.D.Hogan]";
 #include "yambo.h"
 #if defined _FORTRAN_US
  int driver_
 #else
  int driver
 #endif
#endif
/* YAMBO */
#if defined _YAMBO_MAIN
 char *tool="yambo";
 char *tool_desc="A shiny pot of fun and happiness [C.D.Hogan]";
 #include "yambo.h"
 #if defined _FORTRAN_US
  int yambo_driver_
 #else
  int yambo_driver
 #endif
#endif
/* YPP */
#if defined _YPP_MAIN
 char *tool="ypp";
 char *tool_desc="Y(ambo) P(ost) P(rocessor)";
 #include "ypp.h"
 #if defined _FORTRAN_US
  int ypp_i_
 #else
  int ypp_i
 #endif
#endif
/* A2Y */
#if defined _a2y
 char *tool="a2y";
 char *tool_desc="A(binit) 2 Y(ambo) interface";
 #include "a2y.h"
 #if defined _FORTRAN_US
  int a2y_i_
 #else
  int a2y_i
 #endif
#endif
/* C2Y */
#if defined _c2y
 char *tool="c2y";
 char *tool_desc="C(pmd) 2 Y(ambo) interface";
 #include "c2y.h"
 #if defined _FORTRAN_US
  int c2y_i_
 #else
  int c2y_i
 #endif
#endif
/* P2Y */
#if defined _p2y
 char *tool="p2y";
 char *tool_desc="P(Wscf) 2 Y(ambo) interface";
 #include "p2y.h"
 #if defined _FORTRAN_US
  int p2y_i_
 #else
  int p2y_i
 #endif
#endif
/* E2Y */
#if defined _e2y
 char *tool="e2y";
 char *tool_desc="E(TSF) 2 Y(ambo) interface (0.6)";
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

