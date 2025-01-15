/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM

*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <kind.h>

void input_file(struct yambo_seed_struct y,struct tool_struct t,int *use_editor)
{
 int ttd;
 char edit_line[100]={'\0'},file_name[100]={'\0'};
 /*
  External functions
 */
 extern int guess_winsize();
 /*
  stdlog?
 */
 ttd=guess_winsize();
 /* */
 strcpy(edit_line,t.editor);
 strcpy(file_name,y.in_file);
 if (y.parenv_file !=NULL) {strcpy(file_name,y.parenv_file);};
 strncat(edit_line," ",1);
 strncat(edit_line,file_name,strlen(file_name));
 if (*use_editor == 1 && ttd>0 && strstr(t.editor,"none ")==0)
 {
  system(edit_line);
 }else if (*use_editor == -2){ 
  fprintf(stderr," \n%s%s %s %s\n\n",t.tool,": input file",file_name,"created");
  exit (0);
 };
};
