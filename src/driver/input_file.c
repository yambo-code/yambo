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
#include <string.h>
#include <kind.h>
#include <macros.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void input_file(struct yambo_seed_struct y,struct tool_struct t,int use_editor)
{
 int ttd;
 char edit_line[100]={'\0'};
 /*
  External functions
 */
#if !defined _TEST_MAIN
 extern int guess_winsize();
#endif
 /*
  stdlog?
 */
#if !defined _TEST_MAIN
 ttd=guess_winsize();
#endif
 strcpy(edit_line,t.editor);
 strncat(edit_line,y.in_file,strlen(y.in_file));
#if defined _YAMBO_MAIN || defined _YPP_MAIN 
 if (y.in_file_N == 1 && ttd>0)
 {
  if(strstr(t.editor,"none ")==0 && use_editor==1) { 
    system(edit_line);
  }
  else { 
   fprintf(stderr," \n%s %s %s\n\n","yambo: input file",y.in_file,"created");
   exit (0);
  }
 };
#endif
};
