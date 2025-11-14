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
#include "wrapper.h"

// Common prototype for all FORTRAN driver entry points
typedef int FortranDriverFn(int *, int *, int *, int *, int *, int *, int *,
                            int *, char *string, char *in_file, char *in_dir,
                            char *out_dir, char *com_dir, char *job,
                            int string_N, int in_file_N, int in_dir_N,
                            int out_dir_N, int com_dir_N, int job_N);

// Declare driver functions
extern FortranDriverFn F90_FUNC(yambo);
extern FortranDriverFn F90_FUNC(ypp);
extern FortranDriverFn F90_FUNC(a2y);
extern FortranDriverFn F90_FUNC(p2y);
extern FortranDriverFn F90_FUNC(c2y);
extern FortranDriverFn F90_FUNC(e2y);
