/*
         Copyright (C) 2000-2019 the YAMBO team
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
#include <stdlib.h>
#include <getopt.h>
#include <string.h>
#include <kind.h>
#include <usage.h>
#include <load_environments.h>

struct n_options_struct options_maker( )
{
 int i_opt;
 n_options_struct n_options[100];
 for(i_opt=0;i_opt<100;i_opt++) {
  n_options[i_opt].long_opt=NULL;
  n_options[i_opt].short_opt=0;
  n_options[i_opt].short_desc=NULL;
  n_options[i_opt].long_desc=NULL;
  n_options[i_opt].project="all";
  n_options[i_opt].n_int=0;
  n_options[i_opt].n_float=0;
  n_options[i_opt].n_char=0;
  n_options[i_opt].serial_var=0;
  n_options[i_opt].no_GPL=0;
 }
 i_opt=-1;
 /* */
 i_opt++;
 n_options[i_opt].short_desc="Short Help";
 n_options[i_opt].short_opt='h';
 n_options[i_opt].long_opt="help";
 n_options[i_opt].serial_var=1;
 i_opt++;
 n_options[i_opt].short_desc="Long Help";
 n_options[i_opt].short_opt='H';
 n_options[i_opt].long_opt="Help";
 n_options[i_opt].serial_var=1;
 i_opt++;
 n_options[i_opt].short_desc="Job strinh";
 n_options[i_opt].short_opt='J';
 n_options[i_opt].long_opt="job";
 n_options[i_opt].n_char=1;


/*
  {"Help","lhelp",  'H',"Long Help",0,0,0,0},               
  {"job","jobstr", 'J',"Job string identifier",0,0,1,1},   
  {"verbosity","infver", 'V',"Input file verbosity",0,0,1,0},    
  {"NULL","DESC",   0,"[opt=RL,kpt,sc,qp,io,gen,resp,all,par]",0,0,0,0},
  {"input","ifile",  'F',"Input file",0,0,1,1},              
  {"dir","idir",   'I',"Core I/O directory",0,0,1,1},         
  {"io","odir",   'O',"Additional I/O directory",0,0,1,1},   
  {"com","cdir",   'C',"Communications I/O directory",0,0,1,1},
  {"db","dbpr",   'D',"DataBases properties",0,0,0,0},    
  {"walltime","wallt",  'W',"Wall Time limitation (1d2h30m format)",0,0,1,1}, 
  {"quiet","quiet",  'Q',"Don't launch the text editor",0,0,0,0}, 
#if defined _MPI
  {"parenv","parenv", 'E',"Environment Parallel Variables file",0,0,1,1},               
  {"nompi","nompi",  'M',"Switch-off MPI support (serial run)",0,0,0,0}, 
#endif
#if defined _OPENMP
  {"noopenmp","noopenmp",'N',"Switch-off OpenMP support (single thread run)",0,0,0,1}, 
#endif
*/
 /* */
 return(n_options[100]);
};
