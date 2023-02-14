/*
         Copyright (C) 2000-2022 the YAMBO team
               http://www.yambo-code.org
 
  Authors (see AUTHORS file for details): AM
  
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
#include <stdio.h>
#include <stdlib.h>
#include <wrapper.h>
#include <fortran_driver.h>
#include <kind.h>
#include <driver.h>
#if defined _yambo || defined _ypp || defined _a2y || defined _p2y
 #include <yambo_driver.h>
#endif
#if defined _MPI 
 #include <mpi.h>
#endif

void launcher(int argc, char *argv[],int np, int pid, struct yambo_seed_struct y,int *use_editor, int *use_mpi)
{
 int yambo_err;
 /* 
   Par Environments? Yes? => Return
 */
#if defined _yambo
 if (y.parenv_file !=NULL) 
 {
  int env_editor=load_environments(y.parenv_file);
  if (env_editor==1) 
  {
   *use_editor=1;
   return;
  };
 };
#endif
 /* 
   MPI
 */
#if defined _MPI
 if (*use_mpi==1) {
   MPI_Init(&argc,&argv);               /* starts MPI */
   MPI_Comm_rank(MPI_COMM_WORLD, &pid); /* get current process id */
   MPI_Comm_size(MPI_COMM_WORLD, &np);  /* get number of processes */
 };
#endif
#if defined _example_driver
 /* 
   Testing Driver
 ===========================================================================
 */
 yambo_err=F90_FUNC(main)(
#include <fortran_arguments.h>
 );
#endif
#if defined _yambo
 /* 
   Running the Fortran YAMBO driver 
 ===========================================================================
 */
 yambo_err=F90_FUNC(yambo)(
#include <fortran_arguments.h>
 );
 if(yambo_err==2) exit(0); /* DB listing mode */
#endif
#if defined _ypp
 /* 
   Running the Fortran YPP driver
 ===========================================================================
 */
 F90_FUNC(ypp)(
#include <fortran_arguments.h>
 );
#endif
#if defined _c2y
 /* 
   Running the Fortran c2y driver
 ===========================================================================
 */
 F90_FUNC(c2y)(
#include <fortran_arguments.h>
 );
#endif
#if defined _a2y
 /* 
   Running the Fortran a2y driver
 ===========================================================================
 */
 F90_FUNC(a2y)(
#include <fortran_arguments.h>
 );
#endif
#if defined _p2y
 /* 
   Running the Fortran p2y driver 
 ===========================================================================
 */
 F90_FUNC(p2y)(
#include <fortran_arguments.h>
 );
#endif
#if defined _e2y
 /* 
   Running the Fortran p2y driver 
 ===========================================================================
 */
 F90_FUNC(e2y)(
#include <fortran_arguments.h>
 );
#endif
#if defined _eph2y
 /* 
   Running the Fortran eph2y driver 
 ===========================================================================
 */
 F90_FUNC(eph2y)(
#include <fortran_arguments.h>
 );
#endif
 /* 
   Input file edit ?
 ===========================================================================
 */
 if ( y.in_file_N ==1 && *use_editor ==0 ) {*use_editor=1;};
 if ( y.in_file_N ==0 || y.in_file_N ==2 ) {*use_editor=0;};
 /* 
   Error message
 ===========================================================================
 */
 if ( y.in_file_N < 0 ) 
 {
  if (pid==0 && y.in_file_N == -1) {
   fprintf(stderr," \n%s\n\n","yambo: cannot access CORE database (SAVE/*db1 and/or SAVE/*wf)");
  };
  if (pid==0 && y.in_file_N == -2) {
   fprintf(stderr," \n%s\n\n","yambo: invalid command line options and/or build");
  };
#if defined _MPI
  if (*use_mpi==1) { MPI_Abort(MPI_COMM_WORLD,1); };
#endif 
 }
};
