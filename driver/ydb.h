/*
  Copyright (C) 2000-2013 A. Marini and the YAMBO team 
               http://www.yambo-code.org
  
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
  declaration
*/
/*
 "e" and "s" commmand line structure
*/
#if defined _FORTRAN_US
 int ydb_i
#else
 int ydb_i_
#endif
(int *, char *rnstr2, int *, char *desc, char *vers, char *runlevels, char *file_name, int * );
/*
 Command line structure
*/
 static Ldes opts[] = { /* Int Real Ch (dummy) Parallel_option*/
  {"help",  "h","Short Help",0,0,0,0,0}, 
  {"lhelp", "H","Long Help",0,0,0,0,0}, 
  {"del",   "d","Remove",1,0,0,0,0},   
  {"add",   "a","Add",0,0,0,0,0},   
  {"in",    "i","Input",0,0,1,0,0},    
  {"out",   "o","Output",0,0,1,0,0},    
  {"list",  "l","List",0,0,0,0,0},              
  {"view",  "v","View",0,0,0,0,0},              
  {"ref",   "r","Reference",1,0,0,0,0},        
  {"msg",   "m","Short description",0,0,1,0,0},
  {"copy",  "c","Copy the whole ID folder (in/out) locally",1,0,0,0,0},
  {"change","v","Change a give VAR I/O directory",0,0,2,0,0},
  {NULL,NULL,NULL,0,0,0,0,0}
 };
 char *tool="ydb";
 char *tdesc="Y(ambo) D(ata) B(ase) manager";
