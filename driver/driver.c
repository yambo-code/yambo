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
#include <stdio.h>
#include <string.h>
/*
 ...definitions
*/
#include <editor.h>
/* 
 ...Command line options structure
*/
#include <kind.h>
/* 
 ...Library
*/
#include <wrapper.h>
#include <driver.h>
/* 
  MAIN
*/
int main(int argc, char *argv[])
{
 /*
  Work Space
 */
 int np=1,pid=0,use_mpi=1,use_editor=0;
 /*
  Yambo and Tool structures
 */
 yambo_seed_struct y;
 tool_struct tool;
 struct n_options_struct n_options[100];
 /* 
  TOOL initialization
  strcpy(t.tool,tool);
  strcpy(t.desc,tool_desc);
 */
 strcpy(tool.editor,editor);
 tool=tool_init();
 /* 
  VERSION initialization
 */
 F90_FUNC(get_the_version)(&tool.version,&tool.subversion,&tool.patchlevel,&tool.revision,tool.hash);
 sprintf(tool.version_string,"%i.%i.%i Revision %i Hash %s",tool.version,tool.subversion,
                                                            tool.patchlevel,tool.revision,tool.hash);
 printf("TOOL version: %s\n",tool.version_string);
 /*
  Options "maker"
 */ 
 options_maker(n_options);
 /*
  Command line parsing
 */ 
 y=command_line(argc,argv,n_options,tool,&use_editor,&use_mpi);
 /*
   Launcher
 */
 launcher(np,pid,y,&use_editor,&use_mpi);
 /* 
   Input File
 */
 input_file(y,tool,use_editor);
}

