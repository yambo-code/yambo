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
/* 

 Command line options structure
*/
#include <kind.h>
/*

 ...Subroutines/functions
*/
#include <usage.h>
#include <load_environments.h>
#include <command_line_short.h>
/*

 ... Command line options
*/
#include <yambo.h>

/*====
  MAIN
  ====*/
int main(int argc, char *argv[])
{
 /*
  Needed for the drivers call
 */
 int iif=0,iid=1,iod=1,icd=1,ijs=0,np=1,pid=0,lni=0;
 char rnstr2[500]={'\0'};
 char *inf=NULL,*od=NULL,*id=NULL,*js=NULL,*com_dir=NULL;
 /*
  Work Space
 */
 int mpi_init=0,use_editor=1,ttd;
 char edit_line[100]={'\0'};
 /*
  Yambo and Tool structures
 */
 yambo_seed_struct y;
 tool_struct t;
 /*
  External functions
 extern int guess_winsize();
 */
 extern int optind;
 /* 
  TOOL initializatio
 */
 strcpy(t.editor,editor);
 strcpy(t.tool,tool);
 strcpy(t.desc,tool_desc);
 strcpy(t.version,codever);
 /* 
  YAMBO seed initialization
 */
 strcpy(y.in_file,tool);
 strcat(y.in_file,".in");
 y.in_file_N=strlen(y.in_file);
 strcpy(y.in_dir,".");
 y.in_dir_N=strlen(y.in_dir);
 strcpy(y.out_dir,".");
 y.out_dir_N=strlen(y.out_dir);
 strcpy(y.com_dir,".");
 y.com_dir_N=strlen(y.com_dir);
 strcpy(y.job,"");
 y.job_N=strlen(y.job);
 strcpy(y.string,"");
 y.string_N=strlen(y.string);

 inf = malloc(strlen(tool)+4);
 strcpy(inf,tool);
 strcat(inf,".in");
 iif=strlen(inf);
 id       = malloc(2);
 od       = malloc(2);
 com_dir  = malloc(2);
 js  = malloc(2);
 strcpy(od,".");
 strcpy(js," ");
 strcpy(id,".");
 strcpy(com_dir,".");
 strcpy(rnstr2," ");
 /*
  stdlog?
 ttd=guess_winsize();
 */
 if (argc>1) {
  y=command_line_short_new(argc,argv,opts,t);
  command_line_short(argc,argv,opts,&lni,&iif,&iid,&iod,&icd,&ijs,rnstr2,inf,id,od,com_dir,js,tool,tool_desc,editor,codever);
 }
 /* 
   MPI
 ===========================================================================
 */
#if defined _MPI
 if (mpi_init==0) {
   MPI_Init(&argc,&argv);               /* starts MPI */
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
  fprintf(stderr,"\n","");
  fprintf(stderr,"%s %i\n","np:" ,np);
  fprintf(stderr,"%s %i\n","pid:",pid);
  fprintf(stderr,"%s %i %s\n","RUNSTRING (old):",lni,rnstr2);
  fprintf(stderr,"%s %i %s\n","RUNSTRING (new):",y.string_N,y.string);
  fprintf(stderr,"%s %i %s\n","INPUT file(old):",iif,inf);
  fprintf(stderr,"%s %i %s\n","INPUT file(new):",y.in_file_N,y.in_file);
  fprintf(stderr,"%s %i %s\n","INPUT dir (old):",iid,id);
  fprintf(stderr,"%s %i %s\n","INPUT dir (new):",y.in_dir_N,y.in_dir);
  fprintf(stderr,"%s %i %s\n","OUT   dir (old):",iod,od);
  fprintf(stderr,"%s %i %s\n","OUT   dir (new):",y.out_dir_N,y.out_dir);
  fprintf(stderr,"%s %i %s\n","COM   dir (old):",icd,com_dir);
  fprintf(stderr,"%s %i %s\n","COM   dir (new):",y.com_dir_N,y.com_dir);
  fprintf(stderr,"%s %i %s\n","JOB       (old):",ijs,js);
  fprintf(stderr,"%s %i %s\n","JOB       (new):",y.job_N,y.job);
  fprintf(stderr,"\n","");
#if defined _YAMBO_MAIN
 /* 
   Running the Fortran YAMBO driver 
 ===========================================================================
 */
 F90_FUNC(yambo_driver,YAMBO_DRIVER)(
         &np,&pid,&lni,&iif,&iid,&iod,&icd,&ijs,rnstr2,inf,id,od,com_dir,js,lni,iif,iid,iod,icd,ijs);
#endif
#if defined _YPP_MAIN
 /* 
   Running the Fortran YPP driver
 ===========================================================================
 */
 F90_FUNC(ypp_driver,YPP_DRIVER)(
         &np,&pid,&lni,&iif,&iid,&iod,&icd,&ijs,rnstr2,inf,id,od,com_dir,js,lni,iif,iid,iod,icd,ijs);
#endif
#if defined _c2y 
 /* 
   Running the Fortran c2y driver
 ===========================================================================
 */
 F90_FUNC(c2y_i,C2Y_I)(
         &np,&pid,&lni,&iif,&iid,&iod,&icd,&ijs,rnstr2,inf,id,od,com_dir,js,lni,iif,iid,iod,icd,ijs);
#endif
#if defined _a2y 
 /* 
   Running the Fortran a2y driver
 ===========================================================================
 */
 F90_FUNC(a2y_i,A2Y_I)(
         &np,&pid,&lni,&iif,&iid,&iod,&icd,&ijs,rnstr2,inf,id,od,com_dir,js,lni,iif,iid,iod,icd,ijs);
#endif
#if defined _p2y
 /* 
   Running the Fortran p2y driver 
 ===========================================================================
 */
 F90_FUNC(p2y_i,P2Y_I)(
         &np,&pid,&lni,&iif,&iid,&iod,&icd,&ijs,rnstr2,inf,id,od,com_dir,js,lni,iif,iid,iod,icd,ijs);
#endif
#if defined _e2y 
 /* 
   Running the Fortran p2y driver 
 ===========================================================================
 */
 F90_FUNC(e2y_i,E2Y_I)(
         &np,&pid,&lni,&iif,&iid,&iod,&icd,&ijs,rnstr2,inf,id,od,com_dir,js,lni,iif,iid,iod,icd,ijs);
#endif
 /* 
   INPUT FILE
 ===========================================================================
 */
 strcpy(edit_line,editor);
 strncat(edit_line,inf,strlen(inf));
#if defined _YAMBO_MAIN || defined _YPP_MAIN 
 if (iif == 1 && ttd>0)
 {
  if(strstr(editor,"none ")==0 && use_editor) { 
    system(edit_line);
  }
  else { 
   fprintf(stderr," \n%s %s %s\n\n","yambo: input file",inf,"created");
   exit (0);
  }
 };
#endif
 /* 
   Error message
 ===========================================================================
 */
 if ( iif < 0 ) 
 {
  if (pid==0 && iif == -1) {
   fprintf(stderr," \n%s\n\n","yambo: cannot access CORE database (SAVE/*db1 and/or SAVE/*wf)");
  };
  if (pid==0 && iif == -2) {
   fprintf(stderr," \n%s\n\n","yambo: invalid command line options and/or build");
  };
#if defined _MPI
  if (mpi_init==0) { MPI_Abort(MPI_COMM_WORLD,1); };
#endif 
 };
 /* 
   CLEAN & EXIT
 ===========================================================================
 */
 free(inf);
 free(id);
 free(js);
 free(od); 
#if defined _MPI
  if (mpi_init==0) {
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
  };
#endif 
 exit(0);
}

