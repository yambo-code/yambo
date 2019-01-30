/*
         Copyright (C) 2000-2019 the YAMBO team
               http://www.yambo-code.org
 
  Authors (see AUTHORS file for details): DS
  
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
  {"help","help",   'h',"Short Help",0,0,0,0},              
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
  {"noopemp","noopenmp",'N',"Switch-off OpenMP support (single thread run)",0,0,0,1}, 
#endif
