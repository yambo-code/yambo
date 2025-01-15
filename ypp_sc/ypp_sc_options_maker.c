/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM, AC

*/
#include <stdio.h>
#include <kind.h>
#include <ypp_driver.h>
#include <string.h>
#include <driver.h>
#include <stdlib.h>

void options_maker(struct options_struct options[], int n_options)
{
 int i_opt,i,j;
 int max_long_desc=20,non_used_short_opt[200],found;

 for(i_opt=0;i_opt<n_options;i_opt++) {
  options[i_opt].long_opt=NULL;
  options[i_opt].short_opt=0;
  options[i_opt].short_desc=NULL;
  for(i=0;i<max_long_desc;i++) strcpy(options[i_opt].long_desc[i],"undef");
  options[i_opt].yambo_string="undef";
  options[i_opt].bin="all";
  options[i_opt].no_bin="none";
  options[i_opt].int_var=0;
  options[i_opt].float_var=0;
  options[i_opt].char_var=0;
  options[i_opt].serial_var=0;
  options[i_opt].optional_var=0;
  options[i_opt].section="undef";
 }
 i_opt=-1;
 /* 
  Help(s) 
 */
 options_help(options,&i_opt);
 /* 
  Control(s)
 */
 options_control(options,&i_opt);

 /* 
  Ypp
 */
 options_ypp_sc(options,&i_opt);

 /* 
  Find all short_opt alphanumerical variables not used
 */
 j=-1;
 for(i=1;i<=127;i++){
  non_used_short_opt[i]=0;
  if (i<=47) {continue;};
  if (i>=58 && i<=64) {continue;};
  if (i>=91 && i<=96) {continue;};
  if (i>=123) {continue;};
  found=-1;
  for(i_opt=0;i_opt<n_options;i_opt++){
   if (options[i_opt].short_opt == i ) { 
    found=1; 
    break; 
   }
  }
  if (found < 0) { 
   j++;
   non_used_short_opt[j]=i;
  } 
 }
/*
 Assign those unused variables to the options without short descriptions
*/
 j=-1;
 for(i_opt=0;i_opt<n_options;i_opt++){
  if (options[i_opt].short_opt > 0 ) continue;
  if (options[i_opt].short_desc==NULL) break;
  j++;
  options[i_opt].short_opt=non_used_short_opt[j];
  /* DEBUG 
  printf("Short descripton %c assigned to long var %s\n",options[i_opt].short_opt,options[i_opt].long_opt);
  */
 } 
}
