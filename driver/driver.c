/*
  Copyright (C) 2000-2005 A. Marini and the SELF team 
          http://www.fisica.uniroma2.it/~self
  
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
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#if defined MPI 
 #include <mpi.h>
#endif
typedef struct 
{
        char *ln;
        char *sn;
        char *d;
        int   ni;
        int   nr;
        int   nc;
        int   st;
} Ldes;
#if defined extfus
 #define F90_FUNC(name,NAME) name ## _
 #define F90_FUNC_(name,NAME) name ## _
#else
 #define F90_FUNC(name,NAME) name
 #define F90_FUNC_(name,NAME) name
#endif
#include "codever.h"
#if defined self  || PJ_RAS || PJ_REELS || PJ_PH || PJ_DFT || PJ_CYL_CUT || PJ_SPIN || PJ_SC
 #include "self_cpp.h"
#endif
#if defined spp  || SPP_PJ_PH || SPP_PJ_RAS
 #include "spp_cpp.h"
#endif
#if defined a2s
 #include "a2s.h"
#endif
#if defined f2s
 #include "f2s.h"
#endif
#if defined p2s
 #include "p2s.h"
#endif
#if defined e2s
 #include "e2s.h"
#endif
static void usage(int verbose);
static void title(FILE *file_name,char *cmnt);
main(int argc, char *argv[])
{
 int io,i,c,j,k,nf,lni,lnr,lnc,ttd,
     iif=0,iid=1,iod=1,nr=0,ijs=0,np=1,pid=0;
/* 
 By default MPI_init is on. It is swiched off during the options scanning
*/
 int mpi_init=0;
 int iv[4];
 double rv[4];
 char *cv[4];
 char *fmt=NULL,*inf=NULL,*od=NULL,*id=NULL,*js=NULL,*db=NULL;
 extern int optind, optopt;
 extern int guess_winsize();
 char rnstr1[500]={'\0'},rnstr2[500]={'\0'},edit_line[100]={'\0'};
 struct stat buf;

/* 
 Default input file, Job string, I/O directories
*/
 inf = (char *) malloc(strlen(tool)+4);
 strcpy(inf,tool);
 strcat(inf,".in");
 iif=strlen(inf);
 id  = (char *) malloc(2);strcpy(id,".");
 od  = (char *) malloc(2);strcpy(od,".");
 js  = (char *) malloc(2);strcpy(js," ");

 ttd=guess_winsize();

 strcpy(rnstr2," ");
 if (argc>1) {
   while(opts[nr].ln!=NULL) {nr++;};
   fmt = (char *) malloc(sizeof(char)*nr+1);
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
       if (strstr(argv[io-1],opts[i].sn)!=0 && opts[i].st==0) { j=i;break;};
     };
     if (c=='?') {usage(1);exit(0);};
 /*
   Upper Case actions
 */
     if (strcmp(opts[j].ln,"help")==0) {usage(1);exit(0);};
     if (strcmp(opts[j].ln,"lhelp")==0) {usage(2);exit(0);};
/* 
 Switch off MPI_init as I have options used to create the input file...
*/
#if defined self  || PJ_RAS || PJ_REELS || PJ_PH || PJ_DFT || PJ_CYL_CUT || PJ_SPIN || PJ_SC 
     if (j> 9) {mpi_init=-1;};
#endif
#if defined spp || SPP_PJ_PH || SPP_PJ_RAS
     if (j> 6) {mpi_init=-1;};
#endif
/* 
 ... or if nompi is given 
*/
     if (strcmp(opts[j].ln,"nompi")==0) {mpi_init=-1;};

     opts[j].st++; lni=0;lnr=0;lnc=0;
     nf=opts[j].ni+opts[j].nr+opts[j].nc;
     if (optind+nf>argc) {
       fprintf(stderr,"\n%s : invalid option -- %s\n",tool,opts[j].sn); usage(1);exit(0);
     };
     for(i=1;i<=nf;i++) {
       k=0;
       if (strspn(argv[optind-1+i],"-")==1) {
         fprintf(stderr,"\n%s : invalid option -- %s\n",tool,opts[j].sn); usage(1);exit(0);
       };
       if (opts[j].ni!=0 && k==0) {lni++;iv[lni]=atoi(argv[optind-1+i]);opts[j].ni--;k=1;};
       if (opts[j].nr!=0 && k==0) {lnr++;rv[lnr]=atof(argv[optind-1+i]);opts[j].nr--;k=1;};
       if (opts[j].nc!=0 && k==0) {lnc++;cv[lnc]=argv[optind-1+i];opts[j].nc--;k=1; };
     };
 /* 
   Input File, i/o directory 
 
   REALLOC ! 
 */
     if (strcmp(opts[j].ln,"ifile")==0) {
       free(inf);
       inf = (char *) malloc(strlen(cv[1])+1);  
       strcpy(inf,cv[1]);
       iif=strlen(inf);
     };
     if (strcmp(opts[j].ln,"idir")==0) {
       free(id);
       id = (char *) malloc(strlen(cv[1]));
       strcpy(id,cv[1]);
       iid=strlen(id);
     };
     if (strcmp(opts[j].ln,"odir")==0) {
       free(od);
       od = (char *) malloc(strlen(cv[1]));
       strcpy(od,cv[1]);
       iod=strlen(od);
     };
     if (strcmp(opts[j].ln,"jobstr")==0) {
       free(js);
       js = (char *) malloc(strlen(cv[1]));
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
/* 
 If id/od are not found switch to the deafult i/o directory 
*/
 lni=strlen(rnstr2);
 if (stat(id, &buf) != 0) {strcpy(id,".");iid=1;};
 if (stat(od, &buf) != 0) {strcpy(od,".");iod=1;};
 /* 
   MPI
 ===========================================================================
 */
#if defined MPI
 if (mpi_init==0) {
   MPI_Init(&argc,&argv);               /* starts MPI */
   MPI_Comm_rank(MPI_COMM_WORLD, &pid); /* get current process id */
   MPI_Comm_size(MPI_COMM_WORLD, &np);  /* get number of processes */
 };
#endif
#if defined self  || PJ_RAS || PJ_REELS || PJ_PH || PJ_DFT || PJ_CYL_CUT || PJ_SPIN || PJ_SC 
 /* 
   Running the Fortran SELF driver 
 ===========================================================================
 */
 F90_FUNC(self_driver,SELF_DRIVER)(
          rnstr2,&lni,inf,&iif,id,&iid,od,&iod,js,&ijs,&np,&pid);
#endif
#if defined spp  || SPP_PJ_PH || SPP_PJ_RAS
 /* 
   Running the Fortran SPP driver
 ===========================================================================
 */
 F90_FUNC(spp_i,SPP_I)(
          rnstr2,&lni,inf,&iif,id,&iid,od,&iod,js,&ijs,&np,&pid);
#endif
#if defined a2s 
 /* 
   Running the Fortran A2S driver
 ===========================================================================
 */
 F90_FUNC(a2s_i,A2S_I)(
          rnstr2,&lni,inf,&iif,id,&iid,od,&iod,js,&ijs,&np,&pid);
#endif
#if defined p2s
 /* 
   Running the Fortran P2S driver 
 ===========================================================================
 */
 F90_FUNC(p2s_i,P2S_I)(
          rnstr2,&lni,inf,&iif,id,&iid,od,&iod,js,&ijs,&np,&pid);
#endif
#if defined e2s 
 /* 
   Running the Fortran P2S driver 
 ===========================================================================
 */
 F90_FUNC(e2s_i,E2S_I)(
          rnstr2,&lni,inf,&iif,id,&iid,od,&iod,js,&ijs,&np,&pid);
#endif
#if defined f2s 
 /* 
   Running the Fortran F2S driver 
 ===========================================================================
 */
 F90_FUNC(f2s_i,F2S_I)(
          rnstr2,&lni,inf,&iif,id,&iid,od,&iod,js,&ijs,&np,&pid);
#endif
 /* 
   INPUT FILE
 ===========================================================================
 */
 strcpy(edit_line,"vi ");
 strncat(edit_line,inf,strlen(inf));
#if defined self  || PJ_RAS || PJ_REELS || spp || PJ_PH || PJ_DFT || PJ_CYL_CUT || PJ_SPIN || PJ_SC || SPP_PJ_PH || SPP_PJ_RAS 
 if (iif == 1 && ttd>0 ) 
 {
  system(edit_line);
  exit (0);
 };
#endif
 /* 
   Error message
 ===========================================================================
 */
 if ( iif < 0 ) 
 {
  if (pid==0 && iif == -1) {
   fprintf(stderr," \n%s\n\n","self: cannot access CORE database (SAVE/*db1 and/or SAVE/*wf)");
  };
  if (pid==0 && iif == -2) {
   fprintf(stderr," \n%s\n\n","self: invalid command line options and/or build");
  };
#if defined MPI
  if (np>1) {
   MPI_Barrier(MPI_COMM_WORLD);
   MPI_Finalize();
  };
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
 exit(0);
}
static void usage(int verbose)
{
 int i,j,nr=0;
 while(opts[nr].ln!=NULL) {nr++;};
 if (verbose==1) {
  fprintf(stderr,"\nThis is %s %s\n",tool,codever); 
  fprintf(stderr,"Usage: %s",tool); 
  for(j=0;j<=nr-1;j++)
   {fprintf(stderr," -%s",opts[j].sn);
   for(i=1;i<=opts[j].ni;i++) {fprintf(stderr," %s","<int>");};
   for(i=1;i<=opts[j].nr;i++) {fprintf(stderr," %s","<real>");};
   for(i=1;i<=opts[j].nc;i++) {fprintf(stderr," %s","<opt>");};
  };
  fprintf(stderr,"\n%s%s%s\n","Try `",tool," -H' for more information");exit(0);
 };
 if (verbose==2) {title(stderr,"");
 for(j=0;j<=nr-1;j++)
  {fprintf(stderr," -%s",opts[j].sn);
   for(i=1;i<=opts[j].ni;i++) {fprintf(stderr," %s","<int>");};
   for(i=1;i<=opts[j].nr;i++) {fprintf(stderr," %s","<real>");};
   for(i=1;i<=opts[j].nc;i++) {fprintf(stderr," %s","<opt>");};
   if (opts[j].ni==0 && opts[j].nr==0 && opts[j].nc==0) {fprintf(stderr,"\t");};
   fprintf(stderr,"\t:%s\n",opts[j].d);
  };
  fprintf(stderr,"\n");
  fprintf(stderr,"%s\t%s\n\t%s\n\n","By","SELF developers group",
         "http://www.fisica.uniroma2.it/~self");
 };
};
static void title(FILE *file_name,char *cmnt)
{
 fprintf(file_name,"%s%s\n",cmnt,  "   ______   ______   _       _______ ");
 fprintf(file_name,"%s%s\n",cmnt,  "  /  ____) |  ____) | |     |  _____)");
 fprintf(file_name,"%s%s\n",cmnt,  " (  (___   | (___   | |     | (_____ ");
 fprintf(file_name,"%s%s\n",cmnt,  "  \\___  \\  |  ___)  | |     |  _____)");
 fprintf(file_name,"%s%s\n",cmnt,  "  ____)  ) | (____  | |___  | |");
 fprintf(file_name,"%s%s%s\n",cmnt," (______/  |______) |_____) |_|",codever);
 fprintf(file_name,"%s\n%s Tool: %s\n",cmnt,cmnt,tool);
 fprintf(file_name,"%s Description: %s\n\n",cmnt,tdesc);
};
