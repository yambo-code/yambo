/*
  Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
 static Ldes opts[] = { /* Int Real Ch (Dummy)*/
  {"ifile",  "F","Input file",0,0,1,0},              
  {"help","h","Short Help",0,0,0,0}, 
  {"lhelp","H","Long Help",0,0,0,0}, 
  {"jobstr","J","Job string identifier",0,0,1,0},   
  {"ifile",  "F","Input file",0,0,1,0},              
  {"idir",   "I","Core I/O directory",0,0,1,0},         
  {"odir",   "O","Additional I/O directory",0,0,1,0},        
  {"nompi", "N","Skip MPI initialization",0,0,0,0}, 
  {"dbfrag","S","DataBases fragmentation",0,0,0,0}, 
  {"kpt","k","K-grid generator",0,0,0,0}, 
  {"excp","e","Excitons [(s)ort,(sp)in,(a)mplitude]",0,0,1,0}, 
  {"plot","p","Plot [(e)xciton,(m)agnetization,(em),(d)ensity,(w)ave]",0,0,1,0}, 
  {"freehole","f","Free hole position [excitonic plot]",0,0,0,0}, 
  {"bzrim","r","BZ energy RIM analyzer",0,0,0,0}, 
#if defined _SPP_ELPH
  {"elph", "p","Electron-Phonon DBs preprocessing",0,0,0,0}, 
#endif
#if defined _SPP_RAS
  {"surf", "a","Surface spectroscopy postprocessor",0,0,1,0}, 
#endif
  {NULL,NULL,NULL,0,0,0,0}
 };
 char *tool="ypp";
 char *tdesc="Y(ambo) P(ost) P(rocessor)";
