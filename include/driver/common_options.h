/*
         Copyright (C) 2000-2018 the YAMBO team
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
/*
 Driver declaration
*/
  {"help",   "h","Short Help",0,0,0,0,0},              
  {"lhelp",  "H","Long Help",0,0,0,0,0},               
  {"jobstr", "J","Job string identifier",0,0,1,0,1},   
  {"infver", "V","Input file verbosity",0,0,1,0,0},    
  {"DESC",   " ","[opt=RL,kpt,sc,qp,io,gen,resp,all,par]",0,0,0,0,0},
  {"ifile",  "F","Input file",0,0,1,0,1},              
  {"idir",   "I","Core I/O directory",0,0,1,0,1},         
  {"odir",   "O","Additional I/O directory",0,0,1,0,1},   
  {"cdir",   "C","Communications I/O directory",0,0,1,0,1},
  {"dbpr",   "D","DataBases properties",0,0,0,0,0},    
  {"wallt",  "W","Wall Time limitation (1d2h30m format)",0,0,1,0,1}, 
  {"quiet",  "Q","Don't launch the text editor",0,0,0,0,0}, 
#if defined _MPI
  {"parenv", "E","Environment Parallel Variables file",0,0,1,0,1},               
  {"nompi",  "M","Switch-off MPI support (serial run)",0,0,0,0,0}, 
#endif
#if defined _OPENMP
  {"noopenmp","N","Switch-off OpenMP support (single thread run)",0,0,0,0,1}, 
#endif
