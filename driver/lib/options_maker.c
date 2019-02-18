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

void options_maker(struct n_options_struct n_options[])
{
 int i_opt,dummy;
 for(i_opt=0;i_opt<100;i_opt++) {
  n_options[i_opt].long_opt=NULL;
  n_options[i_opt].short_opt=0;
  n_options[i_opt].short_desc=NULL;
  n_options[i_opt].long_desc=NULL;
  n_options[i_opt].yambo_string=NULL;
  n_options[i_opt].project="all";
  n_options[i_opt].n_int=0;
  n_options[i_opt].n_float=0;
  n_options[i_opt].n_char=0;
  n_options[i_opt].serial_var=0;
  n_options[i_opt].no_GPL=0;
 }
 i_opt=-1;
 /* 
  Help(s) 
 */
 i_opt++;
 n_options[i_opt].short_desc="Short Help";
 n_options[i_opt].short_opt='h';
 n_options[i_opt].long_opt="help"; /*help*/
 n_options[i_opt].serial_var=1;
 i_opt++;
 n_options[i_opt].short_desc="Long Help";
 n_options[i_opt].short_opt='H';
 n_options[i_opt].long_opt="Help"; /*lhelp*/
 n_options[i_opt].serial_var=1;
 /* 
  Input file 
 */
 i_opt++;
 n_options[i_opt].short_desc="Input file variables verbosity";
 n_options[i_opt].long_desc="<string> can be RL,kpt,sc,qp,io,gen,resp,all,par";
 n_options[i_opt].short_opt='V';
 n_options[i_opt].long_opt="Verbosity"; 
 n_options[i_opt].n_char=1;
 n_options[i_opt].serial_var=1;
 n_options[i_opt].yambo_string="infver";
 i_opt++;
 n_options[i_opt].short_desc="Input file";
 n_options[i_opt].short_opt='F';
 n_options[i_opt].long_opt="Input"; 
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="Quiet input file creation";
 n_options[i_opt].short_opt='Q';
 n_options[i_opt].long_opt="Quiet"; 
 n_options[i_opt].serial_var=1;
 /* 
  Job control 
 */
 i_opt++;
 n_options[i_opt].short_desc="Job string";
 n_options[i_opt].short_opt='J';
 n_options[i_opt].long_opt="Job"; 
 n_options[i_opt].n_char=1;
 n_options[i_opt].yambo_string="jobstr";
 i_opt++;
 n_options[i_opt].short_desc="Databases properties";
 n_options[i_opt].short_opt='D';
 n_options[i_opt].long_opt="DBlist";
 n_options[i_opt].serial_var=1;
 n_options[i_opt].yambo_string="dbpr";
 i_opt++;
 n_options[i_opt].short_desc="Input directory";
 n_options[i_opt].short_opt='I';
 n_options[i_opt].long_opt="dir"; 
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="I/O directory";
 n_options[i_opt].short_opt='O';
 n_options[i_opt].long_opt="output";
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="Communication directory";
 n_options[i_opt].short_opt='C';
 n_options[i_opt].long_opt="com";
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="Walltime";
 n_options[i_opt].long_desc="Format is DdHhMm with D=days, H=hours and M=minutes";
 n_options[i_opt].long_opt="walltime";
 n_options[i_opt].n_char=1;
 n_options[i_opt].yambo_string="wallt";
 i_opt++;
 n_options[i_opt].short_desc="Environment Parallel Variables file";
 n_options[i_opt].long_opt="parenv"; 
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="Switch off MPI support";
 n_options[i_opt].long_opt="nompi";
 n_options[i_opt].serial_var=1;
 i_opt++;
 n_options[i_opt].short_desc="Switch off OPENMP support";
 n_options[i_opt].long_opt="noopenmp";
 /* 
  Runlevels 
 */
 i_opt++;
 n_options[i_opt].short_desc="Hartree-Fock";
 n_options[i_opt].long_opt="hf";
 n_options[i_opt].short_opt='x';
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="HF_and_locXC";
 i_opt++;
 n_options[i_opt].short_desc="Initialization";
 n_options[i_opt].short_opt='i';
 n_options[i_opt].long_opt="setup";
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="setup";
 i_opt++;
 n_options[i_opt].short_desc="Linear Response optical properties";
 n_options[i_opt].long_desc="Response function solution\nstring=c (G)-space\nstring=b for Bethe-Salpter";
 n_options[i_opt].long_opt="optics";
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="optics";
 n_options[i_opt].n_char=1;
 /* 
  Assign dummy short options to variables with only long options
 */
 dummy=57;
 for(i_opt=0;i_opt<100;i_opt++) {
  if (n_options[i_opt].short_opt > 0 ) continue;
  n_options[i_opt].short_opt=dummy;
  dummy--;
 }
}
