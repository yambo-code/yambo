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
#include <kind.h>
#include <string.h>
#include <stdio.h>
#include <wrapper.h>
#include <tool.h>
#include <driver.h>
#if defined _yambo || defined _ypp
 #include <editor.h>
#endif

/*
 AM 29/7/2021

 This source is project dependent via tool.h. In order to properly compile it, then,
 I need to add fictiuous pre-compiler options

*/
#if defined _p2y || defined _a2y || defined _c2y || defined _2y
#endif

struct tool_struct tool_init( )
{
 tool_struct t;
 t=versions();
#if defined _yambo || defined _ypp
 t.editor=editor;
#else
 t.editor="vim";
#endif
 t.tool=tool;
 t.desc=tool_desc;
 /*
   Projects
 */ 
 char *pj=NULL;
#if defined _YPP_ELPH || defined _ELPH
 pj="ph";
#endif
#if defined _YPP_RT || defined _RT
 pj="rt";
#endif
#if defined _YPP_SC || defined _SC
 pj="sc";
#endif
#if defined _YPP_NL || defined _NL
 pj="nl";
#endif
#if defined _YPP_DF || defined _DF
 pj="df";
#endif
#if defined _QED
 pj="qed";
#endif
#if defined _SURF
 pj="surf";
#endif

 if (pj!=NULL) {
  t.bin = malloc(strlen(tool)+strlen(pj)+1);
  strcpy(t.bin,t.tool);
  t.pj=pj;
  strcat(t.bin,"_");
  strcat(t.bin,t.pj);
 }else{
  t.bin = malloc(strlen(tool));
  strcpy(t.bin,t.tool);
  pj="";
  t.pj=pj;
 }
 if (pj==NULL) pj=" ";
 sprintf(t.version_string,"%i.%i.%i Revision %i Hash %s",t.version,t.subversion,
                                                         t.patchlevel,t.revision,t.hash);
 return(t);
};

