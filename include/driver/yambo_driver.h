/*
  License-Identifier: GPL
 
  Copyright (C) 2019 The Yambo Team
 
  Authors (see AUTHORS file for details): DS
*/
/* YAMBO  */
int load_environments(char *file_name);
void options_control(struct options_struct options[],int *i_opt);
void options_yambo(struct options_struct options[],int *i_opt);
#if defined _FORTRAN_US
 int yambo_
#else
 int yambo
#endif
 (int *, int *,int *,int *,int *,int *,int *,int *,
  char *string, char *in_file, char *in_dir, char *out_dir, char *com_dir, char *job,
  int string_N, int in_file_N, int in_dir_N, int out_dir_N, int com_dir_N, int job_N);
