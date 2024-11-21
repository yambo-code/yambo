/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM
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
 if (strcmp(options[*runid].yambo_string,"undef")==0) return "EMPTY";
 if (*id == 1) {return options[*runid].yambo_string;}
 if (*id == 2) {return options[*runid].short_desc;}
 if (*id == 3) {return options[*runid].bin;}
 if (*id == 4) {return options[*runid].no_bin;}
}
void C_FUNC(get_runlevel, GET_RUNLEVEL)(char *component, int *component_id, int *runlevel_id)
{
 char *c = runlevel(runlevel_id,component_id);
 int len = strlen(c);
 strcpy(component, c);
 component[len] = component[len + 1];
}

