/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM
*/

#include <stdlib.h>
#include <stdio.h>
#include <wrapper.h>
#include <kind.h>
#include <driver.h>
#include <string.h>

char *running_project()
{
 tool_struct tool;
 tool=tool_init();
 char *c;
 if (tool.pj!=NULL) {
  c = malloc(sizeof(tool.pj)+1);
  strcpy(c,tool.pj);
 }else{
  c = malloc(2);
  strcpy(c,"");
 }
 return c;
}
void C_FUNC(get_running_project, GET_RUNNING_TOOL)(char *code_project)
{
 char *c = running_project();
 int len = strlen(c);
 strcpy(code_project, c);
 code_project[len] = code_project[len + 1];
}

