/*
  License-Identifier: GPL

  Copyright (C) 2020 The Yambo Team

  Authors (see AUTHORS file for details): AM
*/

#include <driver.h>
#include <kind.h>
#include <stdio.h>
#include <string.h>
#include <version.h>
#include <wrapper.h>

struct tool_struct versions()
{
    tool_struct t;
    t.version = YAMBO_VERSION;
    t.subversion = YAMBO_SUBVERSION;
    t.patchlevel = YAMBO_PATCHLEVEL;
    t.revision = YAMBO_REVISION;
    sprintf(t.hash, "%s", YAMBO_HASH);
    sprintf(t.lumenver, "%s", LUMEN_VERSION);
    return (t);
}
void C_FUNC(get_version, GET_VERSION)(char *lumenver, int *version, int *subversion,
                                      int *patchlevel, int *revision,
                                      char *hash)
{
    tool_struct t;
    t = versions();
    *version = t.version;
    *subversion = t.subversion;
    *patchlevel = t.patchlevel;
    *revision = t.revision;

    strcpy(hash, t.hash);
    int ylen = strlen(t.hash);
    hash[ylen] = hash[ylen + 1];

    strcpy(lumenver, t.lumenver);
    int llen = strlen(t.lumenver);
    lumenver[llen] = lumenver[llen + 1];
}
