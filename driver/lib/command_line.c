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

  https://www.gnu.org/software/libc/manual/html_node/Getopt-Long-Option-Example.html

*/
#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <string.h>
#include <kind.h>
#include <usage.h>
#include <load_environments.h>

void substring(char [], char[], int, int);

struct yambo_seed_struct command_line(int argc, char *argv[], options_struct *opts,  struct tool_struct t, int *use_editor, int *use_mpi)
{
 int n_options=0,i,n_vars,opt=0;
/*
 int io,i,c,j,k,nf,lnr,lnc,ttd,int_opt[4],lni,nr=0,flag[100];
 double real_opt[4];
 char *ch_opt[4]; 
 char *fmt=NULL,*env_file=NULL;
 char string[500]={'\0'},edit_line[100]={'\0'},ch[500]={'\0'};
*/
 yambo_seed_struct y;

 while(opts[n_options].runlevel!=NULL) {n_options++;};

 struct option long_options[n_options];

 for(i=0;i<=n_options-2;i++) {
   n_vars=opts[i].ni+opts[i].nr+opts[i].nc;
   long_options[i].name=opts[i].long_opt;
   /*no_argument, required_argument and optional_argument*/
   if (n_vars ==0) {long_options[i].has_arg=no_argument;};
   if (n_vars > 0) {long_options[i].has_arg=required_argument;};
   long_options[i].flag=0;
   long_options[i].val=atoi(opts[i].short_opt);
   /*long_options[i].val=opts[i].short_opt;*/
   printf (" %i %s %s\n",i,long_options[i].name,opts[i].short_opt);
 };
 long_options[n_options].name=0;
 long_options[n_options].has_arg=0;
 long_options[n_options].flag=0;
 long_options[n_options].val=0;

 int long_index =0;
 while ((opt = getopt_long_only(argc, argv,"",long_options, &long_index )) != -1) {
  printf (" %i %i",opt,long_index);
 };
 exit(0);
 

/*
 static struct option long_options[] = {
    {"area",      no_argument,       0,  'A' },
    {"perimeter", no_argument,       0,  'B' },
    {"length",    required_argument, 0,  'C' },
    {"breadth",   required_argument, 0,  'D' },
    {0,           0,                 0,  0   }
 };



*/
/*
 fmt = malloc(sizeof(char)*nr+1);
*/
 /* 
   strcat needs fmt to be initialized 
 */
/*
 fmt[0] = '\0' ;
 flag[100]=0;
 for(i=0;i<=nr-1;i++) {
   strcat(fmt,opts[i].short_opt);
 }
 while ((c = getopt(argc, argv, fmt)) != -1) {
   io=optind;
   if (io==1) {io++;};
   for(i=0;i<=nr-1;i++) {
     if (strstr(argv[io-1],opts[i].short_opt)!=0 && flag[i]==0) { 
      j=i;
      break;};
   };
#if defined _NO_OPTIONS_CHECK 
   if (c=='?') {break;};
   nf=opts[j].ni+opts[j].nr+opts[j].nc;
   if (optind+nf>argc) {break;};
#else
   if (c=='?') {
    usage(opts,1,t);
    exit(0);
   };
#endif
*/
  /* Upper Case actions */
  
  /* Help...*/
/*   if (strcmp(opts[j].runlevel,"help")==0)  {usage(opts,1,t);exit(0);};
   if (strcmp(opts[j].runlevel,"lhelp")==0) {usage(opts,2,t);exit(0);};*/
  /*
   ...switch off MPI_init for non-parallel options ...
  */
/*   if (opts[j].mpion==0)  {*use_mpi=-1;};*/
  /*
   ...or for an explicit request
  */
/*   if (strcmp(opts[j].runlevel,"nompi")==0) {*use_mpi=-1;};*/
  /*
   ...switch off launch editor
  */
/*   if (strcmp(opts[j].runlevel,"quiet")==0)  {*use_editor=-2;};*/
  /*
   ...options
  */
/*
   flag[j]++; 
   lni=0;
   lnr=0;
   lnc=0;
   nf=opts[j].ni+opts[j].nr+opts[j].nc;
   if (optind+nf>argc) {
     fprintf(stderr,"\n%s : invalid option -- %s\n",t.tool,opts[j].short_opt); usage(opts,1,t);exit(0);
   };
   for(i=1;i<=nf;i++) {
     k=0;
     if (strspn(argv[optind-1+i],"-")==1) {
#if defined _NO_OPTIONS_CHECK 
       break;
#else
       fprintf(stderr,"\n%s : invalid option -- %s\n",t.tool,opts[j].short_opt); usage(opts,1,t);exit(0);
#endif
     };
     if (opts[j].ni!=0 && k==0) {lni++;int_opt[lni]=atoi(argv[optind-1+i]);opts[j].ni--;k=1;};
     if (opts[j].nr!=0 && k==0) {lnr++;real_opt[lnr]=atof(argv[optind-1+i]);opts[j].nr--;k=1;};
     if (opts[j].nc!=0 && k==0) {lnc++;ch_opt[lnc]=argv[optind-1+i];opts[j].nc--;k=1; };
   };
*/
   /*
    ...Parallel environments
   if (strcmp(opts[j].runlevel,"parenv")==0) {
     free(env_file);
     env_file = malloc(strlen(ch_opt[1])+1);
     strcpy(env_file,ch_opt[1]);
     load_environments(env_file,t.editor);
   };
   */
   /*
   Input File, i/o directory 
   if (strcmp(opts[j].runlevel,"ifile")==0) {
     strcpy(y.in_file,ch_opt[1]);
     y.in_file_N=strlen(y.in_file);
   };
   if (strcmp(opts[j].runlevel,"idir")==0) {
     strcpy(y.in_dir,ch_opt[1]);
     y.in_dir_N=strlen(y.in_dir);
   };
   if (strcmp(opts[j].runlevel,"odir")==0) {
     strcpy(y.out_dir,ch_opt[1]);
     y.out_dir_N=strlen(y.out_dir);
   };
   if (strcmp(opts[j].runlevel,"cdir")==0) {
     strcpy(y.com_dir,ch_opt[1]);
     y.com_dir_N=strlen(y.com_dir);
   };
   if (strcmp(opts[j].runlevel,"jobstr")==0) {
     strcpy(y.job,ch_opt[1]);
     y.job_N=strlen(y.job);
   };
   strcat(string," ");
   strcat(string,opts[j].runlevel);
   strcpy(y.string,string);
   y.string_N=strlen(y.string);
   for(i=1;i<=lni;i++) {
    substring(y.string, ch,1, y.string_N);
    sprintf(string,"%s %d ",ch,int_opt[i]);
    strcpy(y.string,string);
    y.string_N=strlen(y.string);
   };
   for(i=1;i<=lnr;i++) {
    substring(y.string, ch,1, y.string_N);
    sprintf(string,"%s %f ",ch,real_opt[i]);
    strcpy(y.string,string);
    y.string_N=strlen(y.string);
   };
   for(i=1;i<=lnc;i++) {
    substring(y.string, ch,1, y.string_N);
    sprintf(string,"%s %s ",ch,ch_opt[i]);
    strcpy(y.string,string);
    y.string_N=strlen(y.string);
   };

 };
 free(fmt);
   */
 y.string_N=strlen(y.string);
 /* 
  Filling of empty input files
 */
 if (y.in_file_N==0){
  strcpy(y.in_file,t.tool);
  strcat(y.in_file,".in");
  y.in_file_N=strlen(y.in_file);
 };
 if (y.in_dir_N==0) { 
  strcpy(y.in_dir,".");
  y.in_dir_N=1;
 };
 if (y.out_dir_N==0) { 
  strcpy(y.out_dir,".");
  y.out_dir_N=1;
 }
 if (y.com_dir_N==0) { 
  strcpy(y.com_dir,".");
  y.com_dir_N=1;
 }
 if (y.job_N==0) { 
  strcpy(y.job,"");
  y.job_N=0;
 }
 if (y.string_N==0) { 
  strcpy(y.string,"");
  y.string_N=0;
 }
 /* */
 return(y);
};

void substring(char s[], char sub[], int p, int l) {
   int c = 0;
   
   while (c < l) {
      sub[c] = s[p+c-1];
      c++;
   }
   sub[c] = '\0';
}
