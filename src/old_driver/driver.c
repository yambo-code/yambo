/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM

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

