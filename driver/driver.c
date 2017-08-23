/*
         Copyright (C) 2000-2017 the YAMBO team
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
/*
 INCLUDES
*/
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#if defined _MPI 
 #include <mpi.h>
#endif
#include "editor.h"
#include "codever.h"
/* 
 Command line options structure
*/
typedef struct 
{
        char *ln;
        char *sn;
        char *d;
        int   ni;
        int   nr;
        int   nc;
        int   st;
        int   mp;
} Ldes;
/* 
 Yambo/Ypp driver flag
*/
#if defined _yambo  || _ELPH || _SC  || _RT || _QED
 #define _YAMBO_MAIN
#endif
#if defined _MAGNETIC || _KERR || _SURF
 #define _YAMBO_MAIN
#endif
#if defined _ypp  || _YPP_ELPH || _YPP_RT || _YPP_SC || _YPP_MAGNETIC || _YPP_SURF
 #define _YPP_MAIN
#endif
/* 
 Includes (II)
*/
#if defined _YAMBO_MAIN
 #include "yambo_cpp.h"
#endif
#if defined _YPP_MAIN
 #include "ypp_cpp.h"
#endif
#if defined _a2y
 #include "a2y.h"
#endif
#if defined _c2y
 #include "c2y.h"
#endif
#if defined _p2y
 #include "p2y.h"
#endif
#if defined _e2y
 #include "e2y.h"
#endif
/* 
 Declarations 
*/
static void load_environments(char *file_name);
static void usage(int verbose);
static void title(FILE *file_name,char *cmnt);
/*
*/
/* 
 F90 wrapper
*/
#if defined _FORTRAN_US
 #define F90_FUNC(name,NAME) name ## _
 #define F90_FUNC_(name,NAME) name ## _
#else
 #define F90_FUNC(name,NAME) name
 #define F90_FUNC_(name,NAME) name
#endif
/* */
int main(int argc, char *argv[])
{
 int io,i,c,j,k,nf,lni,lnr,lnc,ttd,
     iif=0,iid=1,iod=1,icd=1,nr=0,ijs=0,np=1,pid=0;
/* 
 By default MPI_init is on. It is swiched off during the options scanning
*/
 int mpi_init=0;
 int use_editor=1;
 int iv[4];
 double rv[4];
 char *cv[4]; 
 char *fmt=NULL,*inf=NULL,*od=NULL,*id=NULL,*js=NULL,*db=NULL,*com_dir=NULL,*env_file=NULL;
 extern int optind;
 extern int guess_winsize();
 char rnstr1[500]={'\0'},rnstr2[500]={'\0'},edit_line[100]={'\0'};
 struct stat buf;
/* 
 Default input file, Job string, I/O directories
*/
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
 ttd=guess_winsize();
 strcpy(rnstr2," ");
 if (argc>1) {
   while(opts[nr].ln!=NULL) {nr++;};
   fmt = malloc(sizeof(char)*nr+1);
 /* 
  strcat needs fmt to be initialized 
 */
   fmt[0] = '\0' ;
   for(i=0;i<=nr-1;i++) {
     strcat(fmt,opts[i].sn);
   }
   while ((c = getopt(argc, argv, fmt)) != -1) {
     io=optind;
     if (io==1) {io++;};
     for(i=0;i<=nr-1;i++) {
       if (strstr(argv[io-1],opts[i].sn)!=0 && opts[i].st==0) { 
        j=i;
        break;};
     };
#if defined _NO_OPTIONS_CHECK 
     if (c=='?') {break;};
     nf=opts[j].ni+opts[j].nr+opts[j].nc;
     if (optind+nf>argc) {break;};
#else
     if (c=='?') {usage(1);exit(0);};
#endif
 /*
   Upper Case actions
   
   Help...
 */
     if (strcmp(opts[j].ln,"help")==0) {usage(1);exit(0);};
     if (strcmp(opts[j].ln,"lhelp")==0) {usage(2);exit(0);};
/* 
 ...switch off MPI_init for non-parallel options ...
*/
     if (opts[j].mp==0)  {mpi_init=-1;};
/* 
 ...or for an explicit request
*/
     if (strcmp(opts[j].ln,"nompi")==0) {mpi_init=-1;};
/*
 Switch off launch editor
*/
     if (strcmp(opts[j].ln,"quiet")==0)  {use_editor=0;};
/*
*/
     opts[j].st++; 
     lni=0;
     lnr=0;
     lnc=0;
     nf=opts[j].ni+opts[j].nr+opts[j].nc;
     if (optind+nf>argc) {
       fprintf(stderr,"\n%s : invalid option -- %s\n",tool,opts[j].sn); usage(1);exit(0);
     };
     for(i=1;i<=nf;i++) {
       k=0;
       if (strspn(argv[optind-1+i],"-")==1) {
#if defined _NO_OPTIONS_CHECK 
         break;
#else
         fprintf(stderr,"\n%s : invalid option -- %s\n",tool,opts[j].sn); usage(1);exit(0);
#endif
       };
       if (opts[j].ni!=0 && k==0) {lni++;iv[lni]=atoi(argv[optind-1+i]);opts[j].ni--;k=1;};
       if (opts[j].nr!=0 && k==0) {lnr++;rv[lnr]=atof(argv[optind-1+i]);opts[j].nr--;k=1;};
       if (opts[j].nc!=0 && k==0) {lnc++;cv[lnc]=argv[optind-1+i];opts[j].nc--;k=1; };
     };
/* 
 ...Parallel environments
*/
     if (strcmp(opts[j].ln,"parenv")==0) {
       free(env_file);
       env_file = malloc(strlen(cv[1])+1);
       strcpy(env_file,cv[1]);
       load_environments(env_file);
     };
 /* 
   Input File, i/o directory 
 
   REALLOC ! 
 */
     if (strcmp(opts[j].ln,"ifile")==0) {
       free(inf);
       inf = malloc(strlen(cv[1])+1);
       strcpy(inf,cv[1]);
       iif=strlen(inf);
     };
     if (strcmp(opts[j].ln,"idir")==0) {
       free(id);
       id = malloc(strlen(cv[1])+1);
       strcpy(id,cv[1]);
       iid=strlen(id);
     };
     if (strcmp(opts[j].ln,"odir")==0) {
       free(od);
       od = malloc(strlen(cv[1])+1);
       strcpy(od,cv[1]);
       iod=strlen(od);
     };
     if (strcmp(opts[j].ln,"cdir")==0) {
       free(com_dir);
       com_dir = malloc(strlen(cv[1])+1);
       strcpy(com_dir,cv[1]);
       icd=strlen(com_dir);
     };
     if (strcmp(opts[j].ln,"jobstr")==0) {
       free(js);
       js = malloc(strlen(cv[1])+1);
       strcpy(js,cv[1]);
       ijs=strlen(js);
     };
     /* ------------------------- */
     strcat(rnstr1," ");
     strcat(rnstr1,opts[j].ln);
     strcpy(rnstr2,rnstr1);
     for(i=1;i<=lni;i++) {sprintf(rnstr1,"%s %d ",rnstr2,iv[i]);strcpy(rnstr2,rnstr1);};
     for(i=1;i<=lnr;i++) {sprintf(rnstr1,"%s %f ",rnstr2,rv[i]);strcpy(rnstr2,rnstr1);};
     for(i=1;i<=lnc;i++) {sprintf(rnstr1,"%s %s ",rnstr2,cv[i]);strcpy(rnstr2,rnstr1);};

   };
 };
 lni=strlen(rnstr2);
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
 /* Note on passing characters from C to Fortran:
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
  if (mpi_init==0 && np>1) { MPI_Abort(MPI_COMM_WORLD,1); };
#endif 
 };
 /* 
   CLEAN & EXIT
 ===========================================================================
 */
 free(inf);
 free(fmt);
 free(id);
 free(js);
 free(od); 
 free(db);
#if defined _MPI
  if (mpi_init==0) {
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
  };
#endif 
 exit(0);
}
static void load_environments(char *file_name)
{
 FILE *fp;
 char edit_line[100]={'\0'},str[100];
 char* pch;
 char* token;
 char* var;
 char* value;
 fp = fopen(file_name, "r");
 if (fp) {
  while(fgets(str, 100, fp)) {
    pch=strchr(str,'#');
    if (!pch) {
      /* get the first token */
      token=strtok(str," ");
      var=token;
      /* walk through other tokens */
      if ( token != NULL ) 
      {
        token = strtok(NULL," ");
        value=token;
      }
      /* printf( " %s %s \n", var, value );*/
      setenv(var,value,1);
    }
  }
  /*exit(0);*/
 }else{
  fp = fopen(file_name, "w+");
  fputs("#\n",fp);
  fputs("# Edit it and use with -E during runtime\n#\n",fp);
  fputs("# CPU section (just edit, do not remove fields)\n",fp);
  fputs("setenv YAMBO_X_q_0_CPU 1.1.1.1\n",fp);
  fputs("setenv YAMBO_X_finite_q_CPU 1.1.1.1.1\n",fp);
  fputs("setenv YAMBO_X_all_q_CPU 1.1.1.1.1\n",fp);
  fputs("setenv YAMBO_BS_CPU 1.1.1.1\n",fp);
  fputs("setenv YAMBO_SE_CPU 1.1.1.1\n",fp);
  fputs("setenv YAMBO_RT_CPU 1.1.1.1\n",fp);
  fputs("# Scalapack section (leave unchanged if you wish)\n",fp);
  fputs("setenv YAMBO_X_q_0_nCPU_LinAlg_INV 1\n",fp);
  fputs("setenv YAMBO_X_finite_q_nCPU_LinAlg_INV 1\n",fp);
  fputs("setenv YAMBO_X_all_q_nCPU_LinAlg_INV 1\n",fp);
  fputs("setenv YAMBO_BS_nCPU_LinAlg_INV 1\n",fp);
  fputs("setenv YAMBO_BS_nCPU_LinAlg_DIAGO 1\n",fp);
  fputs("# ROLEs section (leave unchanged if you wish)\n",fp);
  fputs("setenv YAMBO_X_q_0_ROLEs g.k.c.v\n",fp);
  fputs("setenv YAMBO_X_finite_q_ROLEs q.g.k.c.v\n",fp);
  fputs("setenv YAMBO_X_all_q_ROLEs q.g.k.c.v\n",fp);
  fputs("setenv YAMBO_BS_ROLEs g.k.eh.t\n",fp);
  fputs("setenv YAMBO_SE_ROLEs q.g.qp.b\n",fp);
  fputs("setenv YAMBO_RT_ROLEs k.b.q.qp\n",fp);
  fclose(fp);
  strcpy(edit_line,editor);
  strncat(edit_line,file_name,strlen(file_name));
  system(edit_line);
  exit(0);
 }
};
static void usage(int verbose)
{
 int i,j,nr=0;
 while(opts[nr].ln!=NULL) {nr++;};
 if (verbose==1) {
  char* MPI_string="Serial";
#if defined _MPI
  MPI_string="MPI";
#endif
#if defined _SCALAPACK
  MPI_string="MPI+SLK";
#endif
#if defined _OPENMP
  char* OMP_string="+OpenMP";
#else
  char* OMP_string=" ";
#endif
  fprintf(stderr,"\nThis is %s %s - %s%s -\n",tool,codever,MPI_string,OMP_string); 
  fprintf(stderr,"Usage: %s",tool); 
  for(j=0;j<=nr-1;j++)
  {if (strcmp(opts[j].ln,"DESC")!=0) 
   {fprintf(stderr," -%s",opts[j].sn);
   for(i=1;i<=opts[j].ni;i++) {fprintf(stderr," %s","<int>");};
   for(i=1;i<=opts[j].nr;i++) {fprintf(stderr," %s","<real>");};
   for(i=1;i<=opts[j].nc;i++) {fprintf(stderr," %s","<opt>");};
   };
  };
  fprintf(stderr,"\n%s%s%s\n","Try `",tool," -H' for more information");exit(0);
 };
 if (verbose==2) {title(stderr,"");
 for(j=0;j<=nr-1;j++)
  {if (strcmp(opts[j].ln,"DESC")==0) 
   {
    fprintf(stderr,"\t\t %s\n",opts[j].d);
   }
   else
   {
    fprintf(stderr," -%s",opts[j].sn);
    for(i=1;i<=opts[j].ni;i++) {fprintf(stderr," %s","<int>");};
    for(i=1;i<=opts[j].nr;i++) {fprintf(stderr," %s","<real>");};
    for(i=1;i<=opts[j].nc;i++) {fprintf(stderr," %s","<opt>");};
    if (opts[j].ni==0 && opts[j].nr==0 && opts[j].nc==0) {fprintf(stderr,"\t");};
    fprintf(stderr,"\t:%s\n",opts[j].d);
   };
  };
  fprintf(stderr,"\n");
  fprintf(stderr,"%s\n\n"," YAMBO developers group (http://www.yambo-code.org)");
 };
};
static void title(FILE *file_name,char *cmnt)
{
 fprintf(file_name,"%s%s\n",cmnt,  " ___ __  _____  __ __  _____   _____ ");
 fprintf(file_name,"%s%s\n",cmnt,  "|   Y  ||  _  ||  Y  ||  _  \\ |  _  |");
 fprintf(file_name,"%s%s\n",cmnt,  "|   |  ||. |  ||.    ||. |  / |. |  |");
 fprintf(file_name,"%s%s\n",cmnt,  " \\   _/ |. _  ||.\\ / ||. _  \\ |. |  |");
 fprintf(file_name,"%s%s\n",cmnt,  "  |: |  |: |  ||: |  ||: |   \\|: |  |");
 fprintf(file_name,"%s%s\n",cmnt,  "  |::|  |:.|:.||:.|:.||::.   /|::.  |");
 fprintf(file_name,"%s%s\n",cmnt,  "  `--\"  `-- --\"`-- --\"`-----\" `-----\"");
 fprintf(file_name,"%s\n%s This is %s %s\n",cmnt,cmnt,tool,codever);
 fprintf(file_name,"%s %s \n\n",cmnt,tdesc);
};
