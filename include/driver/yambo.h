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
/*
 Driver declaration
*/
#if defined _FORTRAN_US
 int yambo_driver
#else
 int yambo_driver_
#endif
 (int *, int *,int *,int *,int *,int *,int *,int *,
  char *rnstr2, char *inf, char *id, char *od, char *com_dir, char *js,
  int lni,int iif,int iid,int iod,int icd,int ijs);
/*
 Command line structure
*/
 static Ldes opts[] = { /* Int Real Ch (dummy) Parallel_option*/
#include "common_options.h"
  {"setup",  "i","Initialization",0,0,0,0,0},          
  {"optics", "o","Optics [opt=(c)hi is (G)-space / (b)se is (eh)-space ]",0,0,1,0,0},
#if defined _SC | defined _MAGNETIC | defined _RT | defined _ELECTRIC
  {"potential",  "v","Self-Consistent Potential",0,0,1,0,0}, 
  {"DESC",       " ","opt=(h)artree,(f)ock,(coh),(sex),(cohsex),(exx),(exxc),(srpa),(d)ef,(ip)",0,0,0,0,0},
  {"DESC",       " ","(h)artree can be combined with others, i.e. hf, hsex, ... ",0,0,0,0,0},
#endif
  {"kernel", "k","Kernel [opt=hartree/alda/lrc/hf/sex/bsfxc]",0,0,1,0,0},
  {"DESC",   " ","       (hf/sex only eh-space; lrc only G-space)",0,0,0,0,0},
  {"bss",    "y","BSE solver [opt=h/d/s/(p/f)i/t]",0,0,1,0,0},                      
  {"DESC",   " ","       (h)aydock/(d)iagonalization/(i)nversion",0,0,0,0,0},
  {"rim_cut","r","Coulomb potential",0,0,0,0,0},  
  {"HF_and_locXC",  "x","Hartree-Fock Self-energy and local XC",0,0,0,0,0},      
  {"em1d",   "d","Dynamical Inverse Dielectric Matrix",0,0,0,0,0},     
  {"em1s",   "b","Static Inverse Dielectric Matrix",0,0,0,0,0},        
  {"dipoles","q","Compute oscillator strenghts (or dipoles)",0,0,0,0,0},        
  {"gwapprx","p","GW approximations [opt=(p)PA/(c)HOSEX]",0,0,1,0,0},              
  {"gw0",    "g","Dyson Equation solver",0,0,1,0,0}, 
  {"DESC",   " ","[opt=(n)ewton/(s)ecant/(g)reen]",0,0,0,0,0},
  {"life",   "l","GoWo Quasiparticle lifetimes",0,0,0,0,0},                  
  {"acfdt",  "a","ACFDT Total Energy",0,0,0,0,0},                            
#if defined _PL
  {"photolum", "u","Photo-Luminescence",0,0,0,0,0}, 
#endif
#if defined _NL
  {"nloptics","u","Non-linear spectroscopy",0,0,0,0,0}, 
#endif
#if defined _RT
  {"negf",   "n","NEQ Real-time dynamics [opt=(p)ump or probe,(pp)ump & probe, (pn) n external fields]",0,0,1,0,0}, 
  {"scattp", "s","Scattering  [opt=(e)lectrons/(p)honons/p(h)otons/(a)ll]",0,0,1,0,0},
#endif
#if defined _QED && !defined _ELPH
  {"corrtp", "c","Correlation [opt=(e)lectrons/p(h)otons/(a)ll]",0,0,1,0,0},
#endif
#if defined _ELPH && !defined _QED
  {"corrtp", "c","Correlation [opt=(e)lectrons/(p)honons]",0,0,1,0,0},
#endif
#if defined _QED && defined _ELPH
  {"corrtp", "c","Correlation [opt=(e)lectrons/(p)honons/p(h)otons/(a)ll]",0,0,1,0,0},
#endif
#if defined _ELPH && !defined _RT
  {"ElPhHam","f","Electron-Phonon Hamiltonian",0,0,0,0,0},    
#endif
#if defined _SC | defined _RT
  {"collisions", "e","Evaluate Collisions",0,0,0,0,0}, 
#endif
#if defined _SC
  {"scrun",      "s","Self-Consistent Single-Particle Calculations",0,0,0,0,0}, 
#endif
#if defined _MAGNETIC 
  {"magnetic", "m","Magnetic [opt=(p)auli,(l)andau,(a)ll]",0,0,1,0,0}, 
#endif
#if defined _ELECTRIC 
  {"electric", "m","Static Electric Field",0,0,0,0,0}, 
#endif
#if defined _SURF
  {"sursp",  "s","Surface Spectroscopy [opt=(r)as/r(e)els/(b)oth]",0,0,1,0,0},
#endif
#if defined _SCALAPACK
  {"slktest",  "s","ScaLapacK test",0,0,0,0,0},
#endif
  {NULL,NULL,NULL,0,0,0,0,0}
 };
 char *tool="yambo";
 char *tool_desc="A shiny pot of fun and happiness [C.D.Hogan]";
