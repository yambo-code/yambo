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
#include <string.h>
#include <kind.h>
#include <driver.h>

void title(FILE *file_name,char *cmnt,  struct tool_struct t)
{
 char *tool = running_tool();
 char *pj   = running_project();
 char *libs = running_libraries();
 fprintf(file_name,"%s%s\n",cmnt,  " ___ __  _____  __ __  _____   _____ ");
 fprintf(file_name,"%s%s\n",cmnt,  "|   Y  ||  _  ||  Y  ||  _  \\ |  _  |");
 fprintf(file_name,"%s%s\n",cmnt,  "|   |  ||. |  ||.    ||. |  / |. |  |");
 fprintf(file_name,"%s%s\n",cmnt,  " \\   _/ |. _  ||.\\ / ||. _  \\ |. |  |");
 fprintf(file_name,"%s%s\n",cmnt,  "  |: |  |: |  ||: |  ||: |   \\|: |  |");
 fprintf(file_name,"%s%s\n",cmnt,  "  |::|  |:.|:.||:.|:.||::.   /|::.  |");
 fprintf(file_name,"%s%s\n",cmnt,  "  `--\"  `-- --\"`-- --\"`-----\" `-----\"");
 fprintf(file_name,"%s '%s' \n",cmnt,t.desc);
 if (strlen(pj)>0) {
  fprintf(file_name,"%s\n%s This is      : %s(%s)",cmnt,cmnt,tool,pj);
 }else{
  fprintf(file_name,"%s\n%s This is      : %s",cmnt,cmnt,tool);
 }
 fprintf(file_name,"%s\n%s Version      : %s ",cmnt,cmnt,t.version_string);
 fprintf(file_name,"%s\n%s Configuration: %s \n",cmnt,cmnt,libs);
};
