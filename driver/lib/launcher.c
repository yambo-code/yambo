/*
         Copyright (C) 2000-2019 the YAMBO team
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
#include <tool.h>
#include <wrapper.h>
#include <kind.h>
#if defined _MPI 
 #include <mpi.h>
#endif

void launcher(int np, int pid, struct yambo_seed_struct y,int *use_editor, int *use_mpi)
{
 /* 
   MPI
 */
#if defined _MPI
 if (*use_mpi==1) {
   MPI_Init(NULL,NULL);                 /* starts MPI */
   MPI_Comm_rank(MPI_COMM_WORLD, &pid); /* get current process id */
   MPI_Comm_size(MPI_COMM_WORLD, &np);  /* get number of processes */
 };
#endif
/* 

  Note on passing characters from C to Fortran:
  For each CHARACTER*n argument passed to a Fortran subprogram, 
  two items are actually passed as arguments:
  - The address of the character argument in memory (that is, a pointer to the argument).
  - The arguments length in bytes. This is the "hidden" length argument 
  that is available to the subprogram from the stack.
  To pass a string argument from Fortran to C, you must explicitly prepare 
  the C function to receive the string address argument and the hidden argument. 
  The order of the address arguments in the argument list will be the same 
  in C as in Fortran. The hidden length arguments, however, will come at the end of the list. 
  If more than one string argument is passed, the length arguments will 
  follow the same order as the address arguments, but at the end of the C's argument list.
  Both C and Fortran both pass strings by reference. 
  See: http://docs.hp.com/en/B3909-90002/ch08s05.html

 */

#if defined _TEST_MAIN
 fprintf(stderr,"\n\n%s \n","C driver");
 fprintf(stderr,"%s %i\n","np:" ,np);
 fprintf(stderr,"%s %i\n","pid:",pid);
 fprintf(stderr,"%s %i %s\n","RUNSTRING :",y.string_N,y.string);
 fprintf(stderr,"%s %i %s\n","INPUT file:",y.in_file_N,y.in_file);
 fprintf(stderr,"%s %i %s\n","INPUT dir :",y.in_dir_N,y.in_dir);
 fprintf(stderr,"%s %i %s\n","OUT   dir :",y.out_dir_N,y.out_dir);
 fprintf(stderr,"%s %i %s\n","COM   dir :",y.com_dir_N,y.com_dir);
 fprintf(stderr,"%s %i %s\n","JOB       :",y.job_N,y.job);
 fprintf(stderr,"\n");

 F90_FUNC(yambo)(
#include <fortran_arguments.h>
 );
 exit(1);
#endif

#if defined _YAMBO_MAIN
 /* 
   Running the Fortran YAMBO driver 
 ===========================================================================
 */
 F90_FUNC(yambo_driver)(
#include <fortran_arguments.h>
 );
#endif
#if defined _YPP_MAIN
 /* 
   Running the Fortran YPP driver
 ===========================================================================
 */
 F90_FUNC(ypp_driver)(
#include <fortran_arguments.h>
 );
#endif
#if defined _c2y 
 /* 
   Running the Fortran c2y driver
 ===========================================================================
 */
 F90_FUNC(c2y_i)(
#include <fortran_arguments.h>
 );
#endif
#if defined _a2y 
 /* 
   Running the Fortran a2y driver
 ===========================================================================
 */
 F90_FUNC(a2y_i)(
#include <fortran_arguments.h>
 );
#endif
#if defined _p2y
 /* 
   Running the Fortran p2y driver 
 ===========================================================================
 */
 F90_FUNC(p2y_i)(
#include <fortran_arguments.h>
 );
#endif
#if defined _e2y 
 /* 
   Running the Fortran p2y driver 
 ===========================================================================
 */
 F90_FUNC(e2y_i)(
#include <fortran_arguments.h>
 );
#endif
 /* 
   Input file edit ?
 ===========================================================================
 */
 if ( y.in_file_N ==1 && *use_editor==0 ) {*use_editor=1;};
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
 /* 
   CLEAN & EXIT
 ===========================================================================
 */
#if defined _MPI
  if (*use_mpi==1) {
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
  };
#endif 
};
