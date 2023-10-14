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

struct yambo_seed_struct command_line(int argc, char *argv[], struct options_struct opts[],  struct tool_struct t, int *use_editor, int *use_mpi, int n_options)
{
 int n_active,n_vars,opt=0,i_opt;
 char opt_string[100],ch[3];
 /* */
 yambo_seed_struct y;
 /* 
  Pre-sets 
 */
 y.in_file = malloc(strlen(t.tool)+3);
 y.parenv_file = NULL;
 strcpy(y.in_file,t.tool);
 strcat(y.in_file,".in");
 y.in_dir=".";
 y.out_dir=".";
 y.com_dir=".";
 y.job="";
 strcpy(y.string,"");
 /* */
 n_active=0;
 for(i_opt=0;i_opt<n_options;i_opt++) {
  if (opts[i_opt].long_opt== NULL ) {break;};
  n_active++;
 }
 /* */
 struct option long_options[n_active+1];
 /* */
 n_active=0;
 for(i_opt=0;i_opt<n_options;i_opt++) {
  /**/
  if (use_me(opts,t,i_opt)==0) continue;
  if (opts[i_opt].short_desc== NULL ) {break;};
  /**/
  long_options[n_active].name=opts[i_opt].long_opt;
  long_options[n_active].flag=0;
  long_options[n_active].val=opts[i_opt].short_opt;
  /* VARS */
  n_vars=opts[i_opt].int_var+opts[i_opt].float_var+opts[i_opt].char_var;
  sprintf(ch,"%c",opts[i_opt].short_opt);
  if (n_vars ==0) {
   long_options[n_active].has_arg=no_argument;
   strcat(opt_string,ch);
  }else if (opts[i_opt].optional_var==1){
   long_options[n_active].has_arg=optional_argument;
   strcat(opt_string,ch);
   sprintf(ch,"%s","::");
   strcat(opt_string,ch);
  }else{
   ch[1]=':';
   long_options[n_active].has_arg=required_argument;
   strcat(opt_string,ch);
  };
  n_active++;
 };
 long_options[n_active].name=0;
 long_options[n_active].has_arg=0;
 long_options[n_active].flag=0;
 long_options[n_active].val=0;
 int long_index =0;
 while ((opt = getopt_long_only(argc, argv,opt_string,long_options, &long_index )) != -1) {
  /* No option valid */
  if (opt == '?') {
    printf ("%s","\n Use -h to list the options\n");
    exit(EXIT_FAILURE);
  }
  for(i_opt=0;i_opt<n_options;i_opt++) {
   if (use_me(opts,t,i_opt)==0) continue;
   if (opts[i_opt].short_opt==opt) {break;};
  }
  /*
  if (optarg != NULL) {printf (" with arg %s", optarg);}
  if (opt > 0) {printf ("GETOPT ouput: %c %s",opts[i_opt].short_opt,opts[i_opt].long_opt);}
  printf ("\n");
  */
  /* help */
  if (strcmp(opts[i_opt].long_opt,"help")==0){
   if (optarg == NULL && argv[optind] != NULL && argv[optind][0] != '-') {            // not an option
    usage(opts,t,argv[optind],n_options);
    ++optind;
   } else {  // handle case of argument immediately after option
    if (optarg == NULL) usage(opts,t,"help",n_options);
    if (optarg != NULL) usage(opts,t,optarg,n_options);
   }
   exit(0);
  }
  /* version */
  if (strcmp(opts[i_opt].long_opt,"version")==0){
    usage(opts,t,"version",n_options);
    exit(0);
  }
  if (strcmp(opts[i_opt].long_opt,"Input")==0){y.in_file=optarg;continue;}
  if (strcmp(opts[i_opt].long_opt,"Job")==0){y.job=optarg;continue;}
  if (strcmp(opts[i_opt].long_opt,"Idir")==0){y.in_dir=optarg;continue;}
  if (strcmp(opts[i_opt].long_opt,"Cdir")==0){y.com_dir=optarg;continue;}
  if (strcmp(opts[i_opt].long_opt,"parenv")==0){y.parenv_file=optarg;continue;}
  if (strcmp(opts[i_opt].long_opt,"Odir")==0){y.out_dir=optarg;continue;}
  if (strcmp(opts[i_opt].long_opt,"nompi")==0){*use_mpi=-1;continue;}
  if (strcmp(opts[i_opt].long_opt,"Quiet")==0){*use_editor=-2;continue;}
  if (opt > 0) {
   strcat(y.string," ");
   strcat(y.string,opts[i_opt].yambo_string);
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



