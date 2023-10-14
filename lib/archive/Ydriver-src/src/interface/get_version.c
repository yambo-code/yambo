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

#include <string.h>
#include <stdio.h>
#include <wrapper.h>
#include <kind.h>
#include <driver.h>
#include <version.h>

struct tool_struct versions( )
{
 tool_struct t;
 t.version=YAMBO_VERSION;
 t.subversion=YAMBO_SUBVERSION;
 t.patchlevel=YAMBO_PATCHLEVEL;
 t.revision=YAMBO_REVISION;
 sprintf(t.hash,"%s",YAMBO_HASH);
 return(t);
}
void C_FUNC(get_version, GET_VERSION)(int *version,int *subversion, int *patchlevel, int *revision, char *hash)
{
 tool_struct t;
 t=versions();
 *version=t.version;
 *subversion=t.subversion;
 *patchlevel=t.patchlevel;
 *revision=t.revision;
 strcpy(hash, t.hash);
 int len = strlen(t.hash);
 hash[len] = hash[len + 1];
}

