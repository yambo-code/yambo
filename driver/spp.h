/*
  Copyright (C) 2000-2005 A. Marini and the SELF team 
          http://www.fisica.uniroma2.it/~self
  
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
  {"ifile",  "F","Input file",0,0,1,0},              /* 4 */
  {"help","h","Short Help",0,0,0,0}, /* 0 */
  {"lhelp","H","Long Help",0,0,0,0}, /* 1 */
  {"jobstr","J","Job string identifier",0,0,1,0},   /* 2 */
  {"ifile",  "F","Input file",0,0,1,0},              /* 3 */
  {"idir",   "I","Input directory",0,0,1,0},         /* 4 */
  {"odir",   "O","Output directory",0,0,1,0},        /* 5 */
  {"nompi", "N","Skip MPI initialization",0,0,0,0}, /* 6 */
  {"dbfrag","S","DataBases fragmentation",0,0,0,0}, /* 7 */
  {"s2s","s","SELF ver. 2 databases translator",0,0,0,0}, /* 8 */
  {"kpt","k","K-grid generator",0,0,0,0}, /* 9,1 */
  {"excp","e","Excitonic properties [(s)ort,sp(in),(p)lot]",0,0,1,0}, /* 10,2 */
  {"bzrim","r","BZ energy RIM analyzer",0,0,0,0}, /* 11,3 */
#if defined _SPP_ELPH
  {"elph", "p","Electron-Phonon DBs preprocessing",0,0,0,0}, /* 12,4 */
#endif
#if defined _SPP_RAS
  {"surf", "a","Surface spectroscopy postprocessor",0,0,1,0}, /* 13,4 */
#endif
  {NULL,NULL,NULL,0,0,0,0}
 };
 char *tool="spp";
 char *tdesc="S(elf) P(ost) P(rocessor)";
