/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM

*/

#include <stdlib.h>
#include <kind.h>
#include <string.h>
#include <stdio.h>
#include <wrapper.h>
#include <driver.h>
#include <editor.h>

char *tool="ypp";
char *tool_desc="Y(ambo) P(ost)/(re) P(rocessor)";

struct tool_struct tool_init( )
{
 tool_struct t;
 t=versions();
 t.editor=editor;
 t.tool=tool;
 t.desc=tool_desc;
 /*
   Projects for naming the tool removed for now. Part to be fixed
 */ 
 char *pj=NULL;

 t.bin = malloc(strlen(tool) + 1);
 strcpy(t.bin,t.tool);
 pj="";
 t.pj=pj;
 
 sprintf(t.version_string,"%i.%i.%i Revision %i Hash %s",t.version,t.subversion,t.patchlevel,t.revision,t.hash);
 return(t);
};

