/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM
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

