/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <kind.h>
#include <driver.h>

void usage(options_struct *options, struct tool_struct t, char *what, int n_options)
{
 int i_opt,i,i_o,n_blanks,n_strings;
 char *pj   = running_project();
 char *tool = running_tool();
 char *libs = running_libraries();
 int max_long_desc=20;
 /*
   Order
 */
 int n_orders=19,n_order_elements;
 char *order[]={
 "Help & version", /* 0 */
 "Input file & Directories", /* 1 */
 "Parallel Control", /* 2 */
 "Initializations", /* 3 */
 "Response Functions", /* 4 */
 "Self-Energy", /* 5 */
 "Bethe-Salpeter Equation", /* 6 */
 "Hamiltonians & Potentials", /* 7 */
 "Real-Time", /* 8 */
 "Surface Spectroscopy", /* 9 */ 
 "Total Energy", /* 10 */
 "Interface", /* 11 */
 "Brillouin Zone", /* 12 */
 "Convertions", /* 13 */
 "Plots", /* 14 */
 "SOC", /* 15 */
 "Utilites", /* 16 */
 "Wannier", /* 17 */
 "Model Hamiltonians", /* 18 */
 "undef", /* 19 */
 };

 if (strcmp(what,"help")==0) {

  title(stderr,"",t);

  n_strings=0;
  for(i_opt=0;i_opt<n_options;i_opt++) {
   if (use_me(options,t,i_opt)==0) continue;
   n_blanks=options[i_opt].int_var*6+options[i_opt].float_var*7+options[i_opt].char_var*9;
   if (n_blanks>n_strings) n_strings=n_blanks;
  };

  for(i_o=0;i_o<n_orders;i_o++) {
   if (order[i_o]==NULL) continue;
   n_order_elements=0;
   for(i_opt=0;i_opt<n_options;i_opt++) {
    if (use_me(options,t,i_opt)==0) continue;
    if (strcmp(options[i_opt].section,order[i_o])==0) n_order_elements++;
   };
   if (n_order_elements==0) continue;
   fprintf(stderr,"\n %s:\n",order[i_o]);
   for(i_opt=0;i_opt<n_options;i_opt++) {
    if (strcmp(options[i_opt].section,order[i_o])!=0) continue;
    if (use_me(options,t,i_opt)==0) continue;
    fprintf(stderr," -%s",options[i_opt].long_opt);
    n_blanks=15-strlen(options[i_opt].long_opt);
    for(i=1;i<=n_blanks;i++) fprintf(stderr," "); 
    if (options[i_opt].short_opt>57) 
     {fprintf(stderr," (-%c)",options[i_opt].short_opt);}
    else
     {for(i=1;i<=5;i++) fprintf(stderr," ");};
    for(i=1;i<=options[i_opt].int_var;i++)   {fprintf(stderr," %s","<int>");};
    for(i=1;i<=options[i_opt].float_var;i++) {fprintf(stderr," %s","<real>");};
    for(i=1;i<=options[i_opt].char_var;i++)  {fprintf(stderr," %s","<string>");};
    n_blanks=n_strings+2-options[i_opt].int_var*6-options[i_opt].float_var*7-options[i_opt].char_var*9;
    for(i=1;i<=n_blanks;i++) fprintf(stderr," "); 
    fprintf(stderr," :%s",options[i_opt].short_desc);
    if (strcmp(options[i_opt].long_desc[0],"undef")!=0) fprintf(stderr," %s%s%s","(more with -h ",options[i_opt].long_opt,")");
    fprintf(stderr,"\n");
   }
  }

  fprintf(stderr,"\n");
  fprintf(stderr,"%s\n\n"," YAMBO developers group (http://www.yambo-code.eu)");

 }else if (strcmp(what,"version")==0) {
  if (strlen(pj)>0) {
   fprintf(stderr,"\nThis is %s(%s) - %s - Ver. %s \n\n",tool,pj,libs,t.version_string);
  }else{
   fprintf(stderr,"\nThis is %s - %s - Ver. %s \n\n",tool,libs,t.version_string);
  }
 }else{
  for(i_opt=0;i_opt<n_options;i_opt++) {
   if (use_me(options,t,i_opt)==0) continue;
   if (strcmp(options[i_opt].long_opt,what)==0) break;
  }
  if (i_opt==n_options) {
   fprintf(stderr,"unrecognized option '%s'\n",what);
   exit(0);
  }
  title(stderr,"",t);
  fprintf(stderr,"\n");
  fprintf(stderr," Long  option: %s\n",options[i_opt].long_opt);
  if (options[i_opt].short_opt>57) fprintf(stderr," Short option: %c\n",options[i_opt].short_opt);
  if (options[i_opt].int_var+options[i_opt].float_var+options[i_opt].char_var>0) 
  {
   fprintf(stderr," Variables   :");
   for(i=1;i<=options[i_opt].int_var;i++)   {fprintf(stderr," %s","<int>");};
   for(i=1;i<=options[i_opt].float_var;i++) {fprintf(stderr," %s","<real>");};
   for(i=1;i<=options[i_opt].char_var;i++)  {fprintf(stderr," %s","<string>");};
   fprintf(stderr,"\n");
  }
  if (strcmp(options[i_opt].long_desc[0],"undef")!=0)
  {
   fprintf(stderr," Description :%s",options[i_opt].short_desc);
   for(i=0;i<max_long_desc;i++) {
    if (strcmp(options[i_opt].long_desc[i],"undef")==0) continue;
    fprintf(stderr,"\n              %s",options[i_opt].long_desc[i]);
   }
  }
  fprintf(stderr,"\n\n");
  fprintf(stderr,"%s\n\n"," YAMBO developers group (http://www.yambo-code.eu)");
 }
};
