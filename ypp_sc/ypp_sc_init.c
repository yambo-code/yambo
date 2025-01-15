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

 char *pj=NULL;

 pj="sc";

 t.bin = malloc(strlen(tool) + strlen(pj) + 2);
 strcpy(t.bin,t.tool);
 t.pj=pj;
 strcat(t.bin,"_");
 strcat(t.bin,t.pj);

 sprintf(t.version_string,"%i.%i.%i Revision %i Hash %s",t.version,t.subversion,t.patchlevel,t.revision,t.hash);
 return(t);
};

