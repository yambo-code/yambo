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
#include <driver.h>

struct yambo_seed_struct command_line(int argc, char *argv[], struct n_options_struct opts[],  struct tool_struct t, int *use_editor, int *use_mpi)
{
 int n_options,n_vars,opt=0,i_opt;
 char opt_string[100],ch[3];
 /* */
 yambo_seed_struct y;
 /* 
  Pre-sets 
 */
 y.in_file=t.tool;
 y.in_dir=".";
 y.out_dir=".";
 y.com_dir=".";
 y.job="";
 /* */
 n_options=0;
 for(i_opt=0;i_opt<=100;i_opt++) {
  if (opts[i_opt].long_opt== NULL ) {break;};
  n_options++;
 }
 /* */
 struct option long_options[n_options+1];
 /* */
 n_options=0;
 for(i_opt=0;i_opt<=99;i_opt++) {
  /**/
  if (opts[i_opt].long_opt== NULL ) {break;};
  /**/
  long_options[n_options].name=opts[i_opt].long_opt;
  long_options[n_options].flag=0;
  long_options[n_options].val=opts[i_opt].short_opt;
  /* VARS */
  n_vars=opts[i_opt].n_int+opts[i_opt].n_float+opts[i_opt].n_char;
  sprintf(ch,"%c",opts[i_opt].short_opt);
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
  if (opt == '?') {
    /*usage()opts,1,t);*/
    exit(EXIT_FAILURE);
  }
  for(i_opt=0;i_opt<=100;i_opt++) {
   if (opts[i_opt].short_opt==opt) {break;};
  }

  if (opt > 0) {printf ("GETOPT ouput: %c %s",opts[i_opt].short_opt,opts[i_opt].long_opt);}
  if (optarg) {printf (" with arg %s", optarg);}
  printf ("\n");

  if (strcmp(opts[i_opt].long_opt,"help")==0 || strcmp(opts[i_opt].long_opt,"Help")==0){
    /*usage()opts,1,t);*/
    exit(0);
  }
  if (strcmp(opts[i_opt].long_opt,"Input")==0){y.in_file=optarg;continue;}
  if (strcmp(opts[i_opt].long_opt,"Job")==0){y.job=optarg;continue;}
  if (strcmp(opts[i_opt].long_opt,"dir")==0){y.in_dir=optarg;continue;}
  if (strcmp(opts[i_opt].long_opt,"com")==0){y.com_dir=optarg;continue;}
  if (strcmp(opts[i_opt].long_opt,"output")==0){y.out_dir=optarg;continue;}
  if (strcmp(opts[i_opt].long_opt,"nompi")==0){*use_mpi=-1;continue;}
  if (strcmp(opts[i_opt].long_opt,"Quiet")==0){*use_editor=-1;continue;}
  if (opt > 0) {
   strcat(y.string," ");
   strcat(y.string,opts[i_opt].long_opt);
  }
  if (optarg) {
   strcat(y.string," ");
   strcat(y.string,optarg);
  }
 };
 /*
  Sizes
 */
 y.string_N=strlen(y.string);
 y.in_file_N=strlen(y.in_file);
 y.out_dir_N=strlen(y.out_dir);
 y.in_dir_N=strlen(y.in_dir);
 y.com_dir_N=strlen(y.com_dir);
 y.job_N=strlen(y.job);
 /* */
 return(y);
};



