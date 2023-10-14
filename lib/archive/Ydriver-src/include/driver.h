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
