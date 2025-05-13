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
#pragma once

#if defined _FORTRAN_US
#define FORTRAN_DRIVER_MACRO(X)                                        \
    int X##_(int *, int *, int *, int *, int *, int *, int *, int *,   \
             char *string, char *in_file, char *in_dir, char *out_dir, \
             char *com_dir, char *job, int string_N, int in_file_N,    \
             int in_dir_N, int out_dir_N, int com_dir_N, int job_N);
#else
#define FORTRAN_DRIVER_MACRO(X)                                                \
    int X(int *, int *, int *, int *, int *, int *, int *, int *,              \
          char *string, char *in_file, char *in_dir, char *out_dir,            \
          char *com_dir, char *job, int string_N, int in_file_N, int in_dir_N, \
          int out_dir_N, int com_dir_N, int job_N);
#endif

FORTRAN_DRIVER_MACRO(yambo);
FORTRAN_DRIVER_MACRO(ypp);
FORTRAN_DRIVER_MACRO(a2y);
FORTRAN_DRIVER_MACRO(p2y);
FORTRAN_DRIVER_MACRO(c2y);
FORTRAN_DRIVER_MACRO(e2y);
