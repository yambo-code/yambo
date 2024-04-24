/*
  License-Identifier: GPL
 
  Copyright (C) 2019 The Yambo Team
 
  Authors (see AUTHORS file for details): AM
*/
int load_environments(char *file_name);
void options_control(struct options_struct options[],int *i_opt);
void options_yambo(struct options_struct options[],int *i_opt);
void options_projects(struct options_struct options[],int *i_opt);
void options_interfaces(struct options_struct options[],int *i_opt);
void options_ypp(struct options_struct options[],int *i_opt);
