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
#include <kind.h>
#include <string.h>
#include <stdio.h>
#include <wrapper.h>
#include <tool.h>
#include <driver.h>
#include <editor.h>

struct tool_struct tool_init( )
{
 tool_struct t;
 t=versions();
 t.editor=editor;
 t.tool=tool;
 t.desc=tool_desc;
 /*
   Projects
 */ 
 char *pj=NULL;
#if defined _YPP_ELPH || defined _ELPH
 pj="ph";
#endif
#if defined _YPP_RT 
 pj="rt";
#endif
#if defined _RT && defined _ELPH
 pj="rt";
#endif
#if defined _YPP_SC || _SC
 pj="sc";
#endif
#if defined _YPP_NL 
 pj="nl";
#endif
#if defined _NL && defined _RT && defined _ELPH
 pj="nl";
#endif
#if defined _MAGNETIC && defined _SC
 pj="magnetic";
#endif
#if defined _ELECTRIC && defined _SC
 pj="electric";
#endif
#if defined _KERR && defined _RT && defined _ELPH
 pj="kerr";
#endif
#if defined _PL && defined _RT && defined _SC && defined _ELPH
 pj="pl";
#endif
#if defined _QED && defined _RT && defined _ELPH
 pj="qed";
#endif
 if (pj==NULL) pj=" ";
 t.pj=pj;

 t.bin = malloc(strlen(tool)+strlen(pj)+1);
 strcat(t.bin,t.tool);
 strcat(t.bin,"_");
 strcat(t.bin,t.pj);
 sprintf(t.version_string,"%i.%i.%i Revision %i Hash %s",t.version,t.subversion,
                                                         t.patchlevel,t.revision,t.hash);
 /*
 printf("TOOL        : %s - %s - %s\n",t.tool,t.desc,t.pj);
 printf("TOOL bin    : %s \n",t.bin);
 printf("TOOL version: %s\n",t.version_string);
 */
 return(t);
};

