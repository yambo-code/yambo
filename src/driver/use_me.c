/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <kind.h>
#include <driver.h>

int use_me(struct options_struct options[], struct tool_struct t, int i_opt)
{
 char *pch,str[100];
 if (options[i_opt].short_desc==NULL) return 0;
 /* NOT allowed bin */
 strcpy(str,options[i_opt].no_bin);
 pch = strtok(str," ");
 while (pch != NULL)
 {
   if (strcmp(pch,t.tool)==0) return 0;
   if (strcmp(pch,t.bin)==0) return 0;
   pch = strtok (NULL, " ");
 }
 /* allowed bin */
 strcpy(str,options[i_opt].bin);
 pch = strtok(str," ");
 while (pch != NULL)
 {
   if (strcmp(pch,t.tool)==0) return 1;
   if (strcmp(pch,t.bin)==0) return 1;
   if (strcmp(pch,"all")==0) return 1;
   pch = strtok (NULL, " ");
 }
};
