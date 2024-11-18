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

char *running_tool()
{
 tool_struct tool;
 tool=tool_init();
 char *c;
 c = malloc(sizeof(tool.tool)+1);
 strcpy(c,tool.tool);
 return c;
}
void C_FUNC(get_running_tool, GET_RUNNING_TOOL)(char *code_tool)
{
 char *c = running_tool();
 int len = strlen(c);
 strcpy(code_tool, c);
 code_tool[len] = code_tool[len + 1];
}

