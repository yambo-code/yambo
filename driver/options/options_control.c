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
#include <kind.h>

void options_control(struct options_struct options[],int *i_opt)
{ 
 char *desc;
 /* 
  Input file 
 */
 desc="Input file & Directories";
 *i_opt=*i_opt+1;
#if defined _yambo || defined _ypp || defined _c2y
 options[*i_opt].short_desc="Input file";
#endif
#if defined _a2y || defined _e2y
 options[*i_opt].short_desc="KSS or WFK file";
#endif
#if defined _p2y 
 options[*i_opt].short_desc="Index file";
#endif
 options[*i_opt].short_opt='F';
 options[*i_opt].long_opt="Input"; 
 options[*i_opt].n_char=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Input file variables verbosity";
 options[*i_opt].long_desc[0]="<string> can be RL,kpt,sc,qp,io,gen,resp,all,par";
 options[*i_opt].short_opt='V';
 options[*i_opt].long_opt="Verbosity"; 
 options[*i_opt].n_char=1;
 options[*i_opt].serial_var=1;
 options[*i_opt].yambo_string="infver";
 options[*i_opt].bin="yambo ypp";
 options[*i_opt].section=desc;
 /* 
  Utils
 */
 desc="Utilites";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Quiet input file creation";
 options[*i_opt].short_opt='Q';
 options[*i_opt].long_opt="Quiet"; 
 options[*i_opt].serial_var=1;
 options[*i_opt].bin="yambo ypp";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Databases properties";
 options[*i_opt].short_opt='D';
 options[*i_opt].long_opt="DBlist";
 options[*i_opt].serial_var=1;
 options[*i_opt].yambo_string="dbpr";
 options[*i_opt].bin="yambo";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Walltime";
 options[*i_opt].long_desc[0]="Format is DdHhMm with D=days, H=hours and M=minutes";
 options[*i_opt].long_opt="walltime";
 options[*i_opt].n_char=1;
 options[*i_opt].yambo_string="wallt";
 options[*i_opt].bin="yambo";
 options[*i_opt].section=desc;
#if defined _SCALAPACK
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="ScaLapacK test";
 options[*i_opt].long_opt="slktest";
 options[*i_opt].short_opt='s';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="slktest";
 options[*i_opt].section=desc;
#endif
 /* 
  Job control 
 */
 desc="Input file & Directories";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Job string";
 options[*i_opt].short_opt='J';
 options[*i_opt].long_opt="Job"; 
 options[*i_opt].n_char=1;
 options[*i_opt].yambo_string="jobstr";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Input directory";
 options[*i_opt].short_opt='I';
 options[*i_opt].long_opt="Idir"; 
 options[*i_opt].n_char=1;
 options[*i_opt].bin="yambo ypp";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="I/O directory";
 options[*i_opt].short_opt='O';
 options[*i_opt].long_opt="Odir";
 options[*i_opt].n_char=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Communication directory";
 options[*i_opt].short_opt='C';
 options[*i_opt].long_opt="Cdir";
 options[*i_opt].n_char=1;
 options[*i_opt].section=desc;
 /* 
  Parallel
 */
 desc="Parallel Control";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Environment Parallel Variables file";
 options[*i_opt].long_opt="parenv"; 
 options[*i_opt].n_char=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Switch off MPI support";
 options[*i_opt].long_opt="nompi";
 options[*i_opt].serial_var=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Switch off OPENMP support";
 options[*i_opt].long_opt="noopenmp";
 options[*i_opt].section=desc;
};
