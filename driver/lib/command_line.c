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
  https://www.gnu.org/software/libc/manual/html_node/Getopt-Long-Options.html#Getopt-Long-Options

Data Type: struct option
This structure describes a single long option name for the sake of getopt_long. 
The argument longopts must be an array of these structures, one for each long option. Terminate the array with an element containing all zeros.

The struct option structure has these fields:

- const char *name
  This field is the name of the option. It is a string.

- int has_arg
  This field says whether the option takes an argument. It is an integer, and there are three legitimate values: no_argument, required_argument and optional_argument.

- int *flag

- int val
  These fields control how to report or act on the option when it occurs.

  If flag is a null pointer, then the val is a value which identifies this option. 
  Often these values are chosen to uniquely identify particular long options.

  If flag is not a null pointer, it should be the address of an int variable which is the flag for this option. 
  The value in val is the value to store in the flag to indicate that the option was seen.

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
 int n_options,n_vars,opt=0,i_opt;
 char opt_string[100],ch[3],*runlevel[100];
 yambo_seed_struct y;
 /* */
 n_options=0;
 for(i_opt=0;i_opt<=100;i_opt++) {
  if (opts[i_opt].long_opt== NULL ) {break;};
  if (strcmp(opts[i_opt].long_opt,"NULL")==0) {continue;};
  n_options++;
 }
 /* */
 struct option long_options[n_options+1];
 /* */
 n_options=0;
 for(i_opt=0;i_opt<=100-1;i_opt++) {
  /**/
  if (opts[i_opt].long_opt== NULL ) {break;};
  if (strcmp(opts[i_opt].long_opt,"NULL")==0) {continue;};
  /**/
  long_options[n_options].name=opts[i_opt].long_opt;
  long_options[n_options].flag=0;
  long_options[n_options].val=opts[i_opt].short_opt;
  sprintf(ch,"%c",opts[i_opt].short_opt);
  /*printf (" %i_opt => %i %c %d %s\n",i_opt,n_options,opts[i_opt].short_opt,long_options[n_options].val,ch);*/
  /* VARS */
  n_vars=opts[i_opt].ni+opts[i_opt].nr+opts[i_opt].nc;
  if (n_vars ==0) {
   long_options[n_options].has_arg=no_argument;
   strcat(opt_string,ch);
  }else{
   long_options[n_options].has_arg=required_argument;
   ch[1]=':';
   strcat(opt_string,ch);
  };
  n_options++;
 };
 long_options[n_options].name=0;
 long_options[n_options].has_arg=0;
 long_options[n_options].flag=0;
 long_options[n_options].val=0;
 int long_index =0;
 while ((opt = getopt_long_only(argc, argv,opt_string,long_options, &long_index )) != -1) {
  switch (opt){
   case '?':
   case 'h':
    usage(opts,1,t);
    exit(0);
    break;
   case 'H':
    usage(opts,2,t);
    exit(0);
    break;
   }
  if (opt == 0) {
   if (strcmp(long_options[long_index].name,"test")==0) {
    printf ("\n\n GETOPT ouput without short-opt: %s\n\n",long_options[long_index].name);
   }
  }else{
   printf ("\n\n GETOPT ouput: %c\n\n",opt);
  }
  if (optarg)
  printf (" with arg %s", optarg);
  printf ("\n");
 };
 /* */
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


