/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM
*/
#include <stdio.h>
#include <string.h>
#include <kind.h>
#include <driver.h>

void title(FILE *file_name,char *cmnt,  struct tool_struct t)
{
 char *tool = running_tool();
 char *pj   = running_project();
 char *libs = running_libraries();
 fprintf(file_name,"%s%s\n",cmnt,  " ___ __  _____  __ __  _____   _____ ");
 fprintf(file_name,"%s%s\n",cmnt,  "|   Y  ||  _  ||  Y  ||  _  \\ |  _  |");
 fprintf(file_name,"%s%s\n",cmnt,  "|   |  ||. |  ||.    ||. |  / |. |  |");
 fprintf(file_name,"%s%s\n",cmnt,  " \\   _/ |. _  ||.\\ / ||. _  \\ |. |  |");
 fprintf(file_name,"%s%s\n",cmnt,  "  |: |  |: |  ||: |  ||: |   \\|: |  |");
 fprintf(file_name,"%s%s\n",cmnt,  "  |::|  |:.|:.||:.|:.||::.   /|::.  |");
 fprintf(file_name,"%s%s\n",cmnt,  "  `--\"  `-- --\"`-- --\"`-----\" `-----\"");
 fprintf(file_name,"%s '%s' \n",cmnt,t.desc);
 if (strlen(pj)>0) {
  fprintf(file_name,"%s\n%s This is      : %s(%s)",cmnt,cmnt,tool,pj);
 }else{
  fprintf(file_name,"%s\n%s This is      : %s",cmnt,cmnt,tool);
 }
 fprintf(file_name,"%s\n%s Version      : %s ",cmnt,cmnt,t.version_string);
 fprintf(file_name,"%s\n%s Configuration: %s \n",cmnt,cmnt,libs);
};
