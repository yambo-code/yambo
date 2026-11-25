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
#include <build_provenance.h>

struct tool_struct versions( )
{
 tool_struct t;
 t.version=YAMBO_VERSION;
 t.subversion=YAMBO_SUBVERSION;
 t.patchlevel=YAMBO_PATCHLEVEL;
 t.revision=YAMBO_REVISION;
 snprintf(t.hash,sizeof(t.hash),"%s",YAMBO_BUILD_COMMIT);
 snprintf(t.commit_short,sizeof(t.commit_short),"%s",YAMBO_BUILD_COMMIT_SHORT);
 snprintf(t.ref,sizeof(t.ref),"%s",YAMBO_BUILD_REF);
 snprintf(t.tag,sizeof(t.tag),"%s",YAMBO_BUILD_TAG);
 snprintf(t.state,sizeof(t.state),"%s",YAMBO_BUILD_STATE);
 snprintf(t.provenance_source,sizeof(t.provenance_source),"%s",YAMBO_BUILD_SOURCE);
 return(t);
}
static void copy_to_fortran(char *dest, const int *dest_len, const char *src)
{
 size_t n=strlen(src);
 if (n > (size_t)*dest_len) n=(size_t)*dest_len;
 memset(dest,' ',(size_t)*dest_len);
 memcpy(dest,src,n);
}
void C_FUNC(get_version, GET_VERSION)(int *version,int *subversion, int *patchlevel,
  int *revision, char *hash, int *hash_len, char *commit_short,
  int *commit_short_len, char *ref, int *ref_len, char *tag, int *tag_len,
  char *state, int *state_len, char *provenance_source, int *source_len)
{
 tool_struct t;
 t=versions();
 *version=t.version;
 *subversion=t.subversion;
 *patchlevel=t.patchlevel;
 *revision=t.revision;
 copy_to_fortran(hash,hash_len,t.hash);
 copy_to_fortran(commit_short,commit_short_len,t.commit_short);
 copy_to_fortran(ref,ref_len,t.ref);
 copy_to_fortran(tag,tag_len,t.tag);
 copy_to_fortran(state,state_len,t.state);
 copy_to_fortran(provenance_source,source_len,t.provenance_source);
}

