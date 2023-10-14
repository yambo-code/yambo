/*
         Copyright (C) 2000-2022 the YAMBO team
               http://www.yambo-code.org
 
  Authors (see AUTHORS file for details): AM
  
  This file is distributed under the terms of the GNU 
  General Public License. You can redistribute it and/or 
  modify it under the terms of the GNU General Public 
  License as published by the Free Software Foundation; 
  either version 2, or (at your option) any later version.
 
  This program is distributed in the hope that it will 
  be useful, but WITHOUT ANY WARRANTY; without even the 
  implied warranty of MERCHANTABILITY or FITNESS FOR A 
  PARTICULAR PURPOSE.  See the GNU General Public License 
  for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
  MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.

*/
#include <stdio.h>
#include <kind.h>

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
