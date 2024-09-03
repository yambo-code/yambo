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

#include <stdlib.h>
#include <stdio.h>
#include <wrapper.h>
#include <kind.h>
#include <driver.h>
#include <string.h>

char *running_project()
{
 tool_struct tool;
 tool=tool_init();
 char *c;
 if (tool.pj!=NULL) {
  c = malloc(sizeof(tool.pj)+1);
  strcpy(c,tool.pj);
 }else{
  c = malloc(2);
  strcpy(c,"");
 }
 return c;
}
void C_FUNC(get_running_project, GET_RUNNING_TOOL)(char *code_project)
{
 char *c = running_project();
 int len = strlen(c);
 strcpy(code_project, c);
 code_project[len] = code_project[len + 1];
}

