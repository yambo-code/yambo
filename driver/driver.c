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

 ...SYSTEM...

*/
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <getopt.h>
#if defined _MPI 
 #include <mpi.h>
#endif
/*

 ...definitions
*/
#include <editor.h>
#include <codever.h>
#include <tool.h>
/* 

 Command line options structure
*/
#include <kind.h>
/* 

 Launcher
*/
#include <launcher.h>
/*

 ...Subroutines/functions
*/
#include <usage.h>
#include <load_environments.h>
#include <command_line_short.h>
#include <input_file.h>
/*

 ... Command line options
*/
#include <wrapper.h>
/*

  ====
  MAIN
  ====
*/
int main(int argc, char *argv[])
{
 /*
  Work Space
 */
 int np=1,pid=0,use_mpi=1,use_editor=1;
 /*
  Yambo and Tool structures
 */
 yambo_seed_struct y;
 tool_struct t;
 /* 
  TOOL initialization
 */
 strcpy(t.editor,editor);
 strcpy(t.tool,tool);
 strcpy(t.desc,tool_desc);
 strcpy(t.version,codever);
 /*
  Command line parsing
 */
 y=command_line_short(argc,argv,short_options,t,use_editor,use_mpi);
 /* 
   MPI
 */
#if defined _MPI
 if (use_mpi==1) {
   MPI_Init(&argc,&argv);               /* starts MPI */
   MPI_Comm_rank(MPI_COMM_WORLD, &pid); /* get current process id */
   MPI_Comm_size(MPI_COMM_WORLD, &np);  /* get number of processes */
 };
#endif
 /* 
   Launcher
 */
 launcher(np,pid,y,use_mpi);
 /* 
   INPUT FILE
 */
 input_file(y,t,use_editor);
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
  if (use_mpi==1) { MPI_Abort(MPI_COMM_WORLD,1); };
#endif 
 };
 /* 
   CLEAN & EXIT
 ===========================================================================
 */
#if defined _MPI
  if (use_mpi==1) {
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
  };
#endif 
 exit(0);
}

