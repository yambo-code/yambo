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
#include <string.h>
#include <kind.h>
#include <driver.h>
#if defined _MPI 
 #include <mpi.h>
#endif
/* 
  MAIN
*/
int main(int argc, char *argv[])
{
 /*
  Work Space
 */
 int np=1,pid=0,use_mpi=1,use_editor=1,n_options=200;
 /*
  Yambo and Tool structures
 */
 yambo_seed_struct y;
 tool_struct tool;
 struct options_struct options[n_options];
 /* 
  TOOL & Version initialization
 */
 tool=tool_init();
 /*
  Options "maker"
 */ 
 options_maker(options,n_options);
 /*
  Command line parsing
 */ 
 y=command_line(argc,argv,options,tool,&use_editor,&use_mpi,n_options);
 /*
  Launcher
 */
 launcher(argc,argv,np,pid,y,&use_editor,&use_mpi);
 /* 
   Input File
 */
 input_file(y,tool,&use_editor);
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
}

