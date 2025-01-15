/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM
*/
struct yambo_seed_struct command_line(int argc, char *argv[],struct options_struct options[], struct tool_struct t, int *use_editor, int *use_mpi, int n_options);
void input_file(struct yambo_seed_struct y,struct tool_struct t, int *use_editor);
void launcher(int argc, char *argv[],int np, int pid, struct yambo_seed_struct y,int *use_editor , int *use_mpi );
void options_maker(struct options_struct options[], int n_options);
struct tool_struct tool_init( );
void usage(struct options_struct options[], struct tool_struct t, char* what, int n_options);
struct tool_struct versions( );
void title(FILE *file_name,char *cmnt, struct tool_struct t);
int use_me(struct options_struct options[], struct tool_struct t, int i_opt);
char *running_tool();
char *running_project();
char *running_libraries();
char *runlevel(int *runid, int *id);
void options_help(struct options_struct options[],int *i_opt);
void options_control(struct options_struct options[],int *i_opt);
void options_ypp(struct options_struct options[],int *i_opt);
void options_yambo(struct options_struct options[],int *i_opt);
void options_projects(struct options_struct options[],int *i_opt);
void options_interfaces(struct options_struct options[],int *i_opt);
