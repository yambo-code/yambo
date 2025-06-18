/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM

*/

typedef struct tool_struct
{
 char *editor;
 char *tool;
 char *bin;
 char *desc;
 char version_string[500];
 char hash[500];
 char *pj;
 int  version;
 int  subversion;
 int  patchlevel;
 int  revision;
} tool_struct;

typedef struct yambo_seed_struct
{
 char string[500];
 char *in_file;
 char *in_dir;
 char *out_dir;
 char *com_dir;
 char *job;
 char *parenv_file;
 int  string_N;
 int  in_file_N;
 int  in_dir_N;
 int  out_dir_N;
 int  com_dir_N;
 int  job_N;
} yambo_seed_struct;

typedef struct options_struct
{
 int  short_opt;
 char *long_opt;
 char *short_desc;
 char long_desc[20][100];
 char *bin;
 char *no_bin;
 char *yambo_string;
 char *section;
 int   int_var;
 int   float_var;
 int   char_var;
 int   optional_var;
 int   serial_var;
} options_struct;

