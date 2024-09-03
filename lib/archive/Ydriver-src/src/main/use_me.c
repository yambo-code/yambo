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
#include <string.h>
#include <stdio.h>
#include <kind.h>
#include <driver.h>

int use_me(struct options_struct options[], struct tool_struct t, int i_opt)
{
 char *pch,str[100];
 if (options[i_opt].short_desc==NULL) return 0;
 strcpy(str,options[i_opt].bin);
 pch = strtok(str," ");
 while (pch != NULL)
 {
   if (strcmp(pch,t.tool)==0) return 1;
   if (strcmp(pch,t.bin)==0) return 1;
   if (strcmp(pch,"all")==0) return 1;
   pch = strtok (NULL, " ");
 }
};
