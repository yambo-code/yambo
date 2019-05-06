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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <kind.h>
#include <driver.h>

void usage(n_options_struct *opts,  struct tool_struct t, char *what)
{
 int i_opt,i,n_blanks,n_options_chars;
 /*
 */
 char *tool = running_tool();
 char *libs = tool_libraries();
 /*
 */
 if (strcmp(what,"help")==0) {
  title(stderr,"",t);
  fprintf(stderr,"\n");
  n_options_chars=0;
  for(i_opt=0;i_opt<100;i_opt++) {
   n_blanks=opts[i_opt].n_int*6+opts[i_opt].n_float*7+opts[i_opt].n_char*9;
   if (n_blanks>n_options_chars) n_options_chars=n_blanks;
  };
  for(i_opt=0;i_opt<100;i_opt++) {
   if (opts[i_opt].long_opt== NULL) continue;
   fprintf(stderr," -%s",opts[i_opt].long_opt);
   n_blanks=10-strlen(opts[i_opt].long_opt);
   for(i=1;i<=n_blanks;i++) fprintf(stderr," "); 
   if (opts[i_opt].short_opt>57) 
    {fprintf(stderr," (-%c)",opts[i_opt].short_opt);}
   else
    {for(i=1;i<=5;i++) fprintf(stderr," ");};
   for(i=1;i<=opts[i_opt].n_int;i++)   {fprintf(stderr," %s","<int>");};
   for(i=1;i<=opts[i_opt].n_float;i++) {fprintf(stderr," %s","<real>");};
   for(i=1;i<=opts[i_opt].n_char;i++)  {fprintf(stderr," %s","<string>");};
   n_blanks=n_options_chars+2-opts[i_opt].n_int*6-opts[i_opt].n_float*7-opts[i_opt].n_char*9;
   for(i=1;i<=n_blanks;i++) fprintf(stderr," "); 
   fprintf(stderr," :%s",opts[i_opt].short_desc);
   fprintf(stderr,"\n");
  }
  fprintf(stderr,"\n");
  fprintf(stderr,"%s\n\n"," YAMBO developers group (http://www.yambo-code.org)");
 }else if (strcmp(what,"version")==0) {
  fprintf(stderr,"\nThis is %s - %s - Ver. %s \n\n",tool,libs,t.version_string);
 }else{
  title(stderr,"",t);
  fprintf(stderr,"\n");
  for(i_opt=0;i_opt<100;i_opt++) {
   if (opts[i_opt].long_opt== NULL) continue;
   if (strcmp(opts[i_opt].long_opt,what)!=0) continue;
   fprintf(stderr," Long  option: %s\n",opts[i_opt].long_opt);
   if (opts[i_opt].short_opt>57) fprintf(stderr," Short option: %c\n",opts[i_opt].short_opt);
   if (opts[i_opt].n_int+opts[i_opt].n_float+opts[i_opt].n_char>0) 
   {
    fprintf(stderr," Variables   :");
    for(i=1;i<=opts[i_opt].n_int;i++)   {fprintf(stderr," %s","<int>");};
    for(i=1;i<=opts[i_opt].n_float;i++) {fprintf(stderr," %s","<real>");};
    for(i=1;i<=opts[i_opt].n_char;i++)  {fprintf(stderr," %s","<string>");};
    fprintf(stderr,"\n");
   }
   if (opts[i_opt].long_desc[0]!= NULL)
   {
    fprintf(stderr," Description : \t%s",opts[i_opt].long_desc[0]);
    for(i=1;i<10;i++) {
     if (opts[i_opt].long_desc[i]== NULL) continue;
     fprintf(stderr,"\n\t\t%s",opts[i_opt].long_desc[i]);
    }
   }
  }
  fprintf(stderr,"\n\n");
  fprintf(stderr,"%s\n\n"," YAMBO developers group (http://www.yambo-code.org)");
 }
};
