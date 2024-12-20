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
#include <tool.h>
#include <driver.h>
#include <editor.h>

/*
 AM 29/7/2021

 This source is project dependent via tool.h. In order to properly compile it, then,
 I need to add fictiuous pre-compiler options

*/

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

