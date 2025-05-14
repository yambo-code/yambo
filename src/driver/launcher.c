/*
  License-Identifier: GPL

  Copyright (C) 2020 The Yambo Team

  Authors (see AUTHORS file for details): AM

*/
#include <driver.h>
#include <fortran_driver.h>
#include <kind.h>
#include <stdio.h>
#include <stdlib.h>
#include <wrapper.h>
#include <yambo_driver.h>
#if defined _MPI
#include <mpi.h>
#endif

#define F90ARG_MACRO(X)                                                    \
    F90_FUNC(X)(&np, &pid, &y.string_N, &y.in_file_N, &y.in_dir_N,         \
                &y.out_dir_N, &y.com_dir_N, &y.job_N, y.string, y.in_file, \
                y.in_dir, y.out_dir, y.com_dir, y.job, y.string_N,         \
                y.in_file_N, y.in_dir_N, y.out_dir_N, y.com_dir_N, y.job_N);

void launcher(int argc, char *argv[], int np, int pid,
              struct yambo_seed_struct y, int *use_editor, int *use_mpi)
{
    int yambo_err;
    /*
      Par Environments? Yes? => Return
    */
#if defined _yambo
    if (y.parenv_file != NULL)
    {
        int env_editor = load_environments(y.parenv_file);
        if (env_editor == 1)
        {
            *use_editor = 1;
            return;
        };
    };
#endif
    /*
      MPI
    */
#if defined _MPI
    if (*use_mpi == 1)
    {
        MPI_Init(&argc, &argv);              /* starts MPI */
        MPI_Comm_rank(MPI_COMM_WORLD, &pid); /* get current process id */
        MPI_Comm_size(MPI_COMM_WORLD, &np);  /* get number of processes */
    };
#endif
#if defined _yambo
    /*
      Running the Fortran YAMBO driver
    ===========================================================================
    */
    yambo_err = F90ARG_MACRO(yambo);
    if (yambo_err == 2)
    {
        exit(0); /* DB listing mode */
    }
#endif
#if defined _ypp
    /*
      Running the Fortran YPP driver
    ===========================================================================
    */
    F90ARG_MACRO(ypp);
#endif
#if defined _c2y
    /*
      Running the Fortran c2y driver
    ===========================================================================
    */
    F90ARG_MACRO(c2y);
#endif
#if defined _a2y
    /*
      Running the Fortran a2y driver
    ===========================================================================
    */
    F90ARG_MACRO(a2y);
#endif
#if defined _p2y
    /*
      Running the Fortran p2y driver
    ===========================================================================
    */
    F90ARG_MACRO(p2y);
#endif
#if defined _e2y
    /*
      Running the Fortran p2y driver
    ===========================================================================
    */
    F90ARG_MACRO(e2y);
#endif
#if defined _eph2y
    /*
      Running the Fortran eph2y driver
    ===========================================================================
    */
    F90ARG_MACRO(eph2y);
#endif
    /*
      Input file edit ?
    ===========================================================================
    */
    if (y.in_file_N == 1 && *use_editor == 0)
    {
        *use_editor = 1;
    };
    if (y.in_file_N == 0 || y.in_file_N == 2)
    {
        *use_editor = 0;
    };
    /*
      Error message
    ===========================================================================
    */
    if (y.in_file_N < 0)
    {
        if (pid == 0 && y.in_file_N == -1)
        {
            fprintf(stderr, " \n%s\n\n",
                    "yambo: cannot access CORE database (SAVE/*db1 and/or "
                    "SAVE/*wf)");
        };
        if (pid == 0 && y.in_file_N == -2)
        {
            fprintf(stderr, " \n%s\n\n",
                    "yambo: invalid command line options and/or build");
        };
#if defined _MPI
        if (*use_mpi == 1)
        {
            MPI_Abort(MPI_COMM_WORLD, 1);
        };
#endif
    }
};
