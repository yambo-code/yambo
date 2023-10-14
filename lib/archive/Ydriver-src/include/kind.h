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
 char  short_opt;
 char *long_opt;
 char *short_desc;
 char *long_desc[20]; /* max size equal to max_long_desc */
 char *bin;
 char *yambo_string;
 char *section;
 int   int_var;
 int   float_var;
 int   char_var;
 int   optional_var;
 int   serial_var;
} options_struct;

