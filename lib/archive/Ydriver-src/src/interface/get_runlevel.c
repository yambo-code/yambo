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
#include <wrapper.h>
#include <kind.h>
#include <driver.h>
#include <string.h>

char *runlevel(int *runid, int *id)
{
 int n_options=200;
 struct options_struct options[n_options];
 options_maker(options,n_options);
 if (strcmp(options[*runid].yambo_string,"unused")==0) return "EMPTY";
 if (*id == 1) {return options[*runid].yambo_string;}
 if (*id == 2) {return options[*runid].short_desc;}
 if (*id == 3) {return options[*runid].bin;}
}
void C_FUNC(get_runlevel, GET_RUNLEVEL)(char *component, int *component_id, int *runlevel_id)
{
 char *c = runlevel(runlevel_id,component_id);
 int len = strlen(c);
 strcpy(component, c);
 component[len] = component[len + 1];
}

