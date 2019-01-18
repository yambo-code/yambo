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
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <getopt.h>
#include <kind.h>
#include <usage.h>
#include <load_environments.h>

void command_line_short(int argc, char *argv[], Ldes *opts,int *lni, int *iif, int *iid, int *iod, int *icd, int *ijs, char *rnstr2, char *inf, char *id, char *od, 
                        char *com_dir, char *js,char *tool, char *tool_desc,char *editor, char *codever)
{
 int io,i,c,j,k,nf,lnr,lnc,ttd,iv[4];
 int mpi_init=0,use_editor=1,nr=0;
 double rv[4];
 char *cv[4]; 
 char *fmt=NULL,*env_file=NULL;
 char rnstr1[500]={'\0'},edit_line[100]={'\0'};

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
   if (c=='?') {
    usage(opts,1,tool,codever,tool_desc);
    exit(0);
   };
#endif
/*
  Upper Case actions
  
  Help...
*/
   if (strcmp(opts[j].ln,"help")==0)  {usage(opts,1,tool,codever,tool_desc);exit(0);};
   if (strcmp(opts[j].ln,"lhelp")==0) {usage(opts,2,tool,codever,tool_desc);exit(0);};
/* 
 ...switch off MPI_init for non-parallel options ...
*/
   if (opts[j].mp==0)  {mpi_init=-1;};
/* 
 ...or for an explicit request
*/
   if (strcmp(opts[j].ln,"nompi")==0) {mpi_init=-1;};
/*
 ...switch off launch editor
*/
   if (strcmp(opts[j].ln,"quiet")==0)  {use_editor=0;};
/*
*/
   opts[j].st++; 
   *lni=0;
   lnr=0;
   lnc=0;
   nf=opts[j].ni+opts[j].nr+opts[j].nc;
   if (optind+nf>argc) {
     fprintf(stderr,"\n%s : invalid option -- %s\n",tool,opts[j].sn); usage(opts,1,tool,codever,tool_desc);exit(0);
   };
   for(i=1;i<=nf;i++) {
     k=0;
     if (strspn(argv[optind-1+i],"-")==1) {
#if defined _NO_OPTIONS_CHECK 
       break;
#else
       fprintf(stderr,"\n%s : invalid option -- %s\n",tool,opts[j].sn); usage(opts,1,tool,codever,tool_desc);exit(0);
#endif
     };
     if (opts[j].ni!=0 && k==0) {*lni++;iv[*lni]=atoi(argv[optind-1+i]);opts[j].ni--;k=1;};
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
     load_environments(env_file,editor);
   };
 /* 
   Input File, i/o directory 
 
   REALLOC ! 
 */
   if (strcmp(opts[j].ln,"ifile")==0) {
     free(inf);
     inf = malloc(strlen(cv[1])+1);
     strcpy(inf,cv[1]);
     *iif=strlen(inf);
   };
   if (strcmp(opts[j].ln,"idir")==0) {
     free(id);
     id = malloc(strlen(cv[1])+1);
     strcpy(id,cv[1]);
     *iid=strlen(id);
   };
   if (strcmp(opts[j].ln,"odir")==0) {
     free(od);
     od = malloc(strlen(cv[1])+1);
     strcpy(od,cv[1]);
     *iod=strlen(od);
   };
   if (strcmp(opts[j].ln,"cdir")==0) {
     free(com_dir);
     com_dir = malloc(strlen(cv[1])+1);
     strcpy(com_dir,cv[1]);
     *icd=strlen(com_dir);
   };
   if (strcmp(opts[j].ln,"jobstr")==0) {
     free(js);
     js = malloc(strlen(cv[1])+1);
     strcpy(js,cv[1]);
     *ijs=strlen(js);

   };
   /* ------------------------- */
   strcat(rnstr1," ");
   strcat(rnstr1,opts[j].ln);
   strcpy(rnstr2,rnstr1);
   for(i=1;i<=*lni;i++) {sprintf(rnstr1,"%s %d ",rnstr2,iv[i]);strcpy(rnstr2,rnstr1);};
   for(i=1;i<=lnr;i++) {sprintf(rnstr1,"%s %f ",rnstr2,rv[i]);strcpy(rnstr2,rnstr1);};
   for(i=1;i<=lnc;i++) {sprintf(rnstr1,"%s %s ",rnstr2,cv[i]);strcpy(rnstr2,rnstr1);};

 };
 *lni=strlen(rnstr2);
 free(fmt);
};
