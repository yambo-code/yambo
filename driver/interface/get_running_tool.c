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
#include <stdio.h>
#include <wrapper.h>
#include <kind.h>
#include <driver.h>
#include <string.h>

char *running_tool()
{
 tool_struct tool;
 tool=tool_init();
 char tmp[100], *c;
 if (strlen(tool.pj)>0)
 {
  sprintf(tmp,"%s_%s",tool.tool,tool.pj);
 }else{
  sprintf(tmp,"%s",tool.tool);
 }
 c = malloc(sizeof(tmp)+1);
 strcpy(c,tmp);
 return c;
}
void C_FUNC(get_running_tool, GET_RUNNING_TOOL)(char *what_is_running)
{
 char *c = running_tool();
 int len = strlen(c);
 strcpy(what_is_running, c);
 what_is_running[len] = what_is_running[len + 1];
}

