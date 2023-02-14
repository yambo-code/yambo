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

*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <kind.h>

void input_file(struct yambo_seed_struct y,struct tool_struct t,int *use_editor)
{
 int ttd;
 char edit_line[100]={'\0'},file_name[100]={'\0'};
 /*
  External functions
 */
 extern int guess_winsize();
 /*
  stdlog?
 */
 ttd=guess_winsize();
 /* */
 strcpy(edit_line,t.editor);
 strcpy(file_name,y.in_file);
 if (y.parenv_file !=NULL) {strcpy(file_name,y.parenv_file);};
 strncat(edit_line," ",1);
 strncat(edit_line,file_name,strlen(file_name));
#if defined _yambo || defined _ypp 
 if (*use_editor == 1 && ttd>0 && strstr(t.editor,"none ")==0)
 {
  system(edit_line);
 }else if (*use_editor == -2){ 
  fprintf(stderr," \n%s%s %s %s\n\n",t.tool,": input file",file_name,"created");
  exit (0);
 };
#endif
};
