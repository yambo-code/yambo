/*
  License-Identifier: GPL
 
  Copyright (C) 2019 The Yambo Team
 
  Authors (see AUTHORS file for details): DS
*/
/* 
 Tool drivers 
*/
/* YAMBO 
*/
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
