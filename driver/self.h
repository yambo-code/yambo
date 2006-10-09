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
  "quiet", "q","Quiet mode",0,0,0,0
*/
 static Ldes opts[] = { /* Int Real Ch (Dummy)*/
  {"help",   "h","Short Help",0,0,0,0},              /* 0 */
  {"lhelp",  "H","Long Help",0,0,0,0},               /* 1 */
  {"jobstr", "J","Job string identifier",0,0,1,0},   /* 2 */
  {"infver", "V","Input Verbosity [1(more),2(qp),3(io),4(debug)]",1,0,0,0},    /* 3 */
  {"ifile",  "F","Input file",0,0,1,0},              /* 4 */
  {"idir",   "I","Input directory",0,0,1,0},         /* 5 */
  {"odir",   "O","Output directory",0,0,1,0},        /* 6 */
  {"nompi",  "N","Skip MPI initialization",0,0,0,0}, /* 7 */
  {"dbpr",   "D","DataBases properties",0,0,0,0},    /* 8 */
  {"dbfrag", "S","DataBases fragmentation",0,0,0,0}, /* 9 */
  {"setup",  "i","Initialization",0,0,0,0},          /* 10 */
  {"optics", "o","Optics [opt=l(inear response)/b(se)]",0,0,1,0},    /* 11 */
  {"tddft",  "t","The TDDFT way (a)ALDA (b)SE (l)RC",0,0,1,0},     /* 12 */
  {"rim_cut","c","Coulomb interaction",0,0,0,0},  /* 13 */
  {"xxvxc",  "x","Exact Exchange Self-energy and Vxc",0,0,0,0},      /* 14 */
  {"em1d",   "d","Dynamical Inverse Dielectric Matrix",0,0,0,0},     /* 15 */
  {"em1s",   "b","Static Inverse Dielectric Matrix",0,0,0,0},        /* 15 */
  {"ppa",    "p","Plasmon Pole Approximation",0,0,0,0},              /* 17 */
  {"gowo",   "g","Dyson Equation solver [n(ewton)/s(ecant)/g(reen)]",0,0,1,0}, /* 18 */
  {"life",   "l","GoWo Quasiparticle lifetimes",0,0,0,0},                  /* 19 */
  {"bss",    "y","BSE solver [opt=h/d/i/t]",0,0,1,0},                      /* 20 */
  {"acfdt",  "a","ACFDT Total Energy",0,0,0,0},                            /* 21 */
#if defined PJ_RAS 
  {"sursp",  "s","Surface Spectroscopy [opt=r(as)/e(el)/b(oth)]",0,0,1,0}, /* 22 */
#endif
#if defined PJ_REELS
  {"reels",  "r","Surface Spectroscopy (REELS)",0,0,0,0},                  /* 22 */
#endif
#if defined PJ_PH
  {"corrtp", "s","Correlation [e(lectrons)/p(honons)/b(oth)]",0,0,1,0},    /* 22 */
#endif
  {NULL,NULL,NULL,0,0,0,0}
 };
 char *tool="self";
 char *tdesc="A shiny pot of fun and happiness [C.D.Hogan]";
