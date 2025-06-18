/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM

*/
#include <stdio.h>
#include <kind.h>
#include <string.h>

void options_help(struct options_struct options[],int *i_opt)
{
 char *desc="Help & version";
 /*
  Help(s) 
 */
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="<string> can be an option (e.g. -h optics)";
 options[*i_opt].short_opt='h';
 options[*i_opt].long_opt="help";
 options[*i_opt].serial_var=1;
 options[*i_opt].optional_var=1;
 options[*i_opt].char_var=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Code version & libraries";
 options[*i_opt].long_opt="version"; 
 options[*i_opt].serial_var=1;
 options[*i_opt].section=desc;

};
