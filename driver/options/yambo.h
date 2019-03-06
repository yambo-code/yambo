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

  Command line structure

*/
 static options_struct options[] = { 
#include "common_options.h"
  {"setup","setup",  'i',"Initialization",0,0,0,0},          
  {"optics","optics", 'o',"Optics [opt=(c)hi is (G)-space / (b)se is (eh)-space ]",0,0,1,0},
#if defined _SC | defined _MAGNETIC | defined _RT | defined _ELECTRIC
  {"potential","potential",  'v',"Self-Consistent Potential",0,0,1,0}, 
  {"NULL","DESC",       0,"opt=(h)artree (f)ock (coh) (sex) (cohsex) (exx) (exxc) (srpa) (d)ef (ip)",0,0,0,0},
  {"NULL","DESC",       0,"(h)artree can be combined with others  i.e. hf  hsex  ... ",0,0,0,0},
#endif
#if defined _TEST_MAIN
  {"test","test", 0,"test",0,0,0,0},
#endif
  {"kernel","kernel", 'k',"Kernel [opt=hartree/alda/lrc/hf/sex/bsfxc]",0,0,1,0},
  {"NULL","DESC",0 ,"       (hf/sex only eh-space; lrc only G-space)",0,0,0,0},
  {"solver","bss",    'y',"BSE solver [opt=h/d/s/(p/f)i/t]",0,0,1,0},
                   
  {"NULL","DESC",  0,"       (h)aydock/(d)iagonalization/(i)nversion",0,0,0,0},
  {"coulomb","rim_cut",'r',"Coulomb potential",0,0,0,0},  
  {"hf","HF_and_locXC",  'x',"Hartree-Fock Self-energy and local XC",0,0,0,0},      
  {"screen_dyn","em1d",   'd',"Dynamical Inverse Dielectric Matrix",0,0,0,0},     
  {"screen","em1s",   'b',"Static Inverse Dielectric Matrix",0,0,0,0},        
  {"dipoles","dipoles",'q',"Compute oscillator strenghts (or dipoles)",0,0,0,0},        
  {"se","gwapprx",'p',"GW approximations [opt=(p)PA/(c)HOSEX]",0,0,1,0},              
  {"dyson","gw0",    'g',"Dyson Equation solver",0,0,1,0}, 
  {"NULL","DESC",   0,"[opt=(n)ewton/(s)ecant/(g)reen]",0,0,0,0},
  {"life","life",   'l',"GoWo Quasiparticle lifetimes",0,0,0,0},                  
  {"acdft","acfdt",  'a',"ACFDT Total Energy",0,0,0,0},                            
#if defined _PL
  {"pl","photolum", 'u',"Photo-Luminescence",0,0,0,0}, 
#endif
#if defined _NL
  {"nl","nloptics",'u',"Non-linear spectroscopy",0,0,0,0}, 
#endif
#if defined _RT
  {"negf","negf",   'n',"NEQ Real-time dynamics [opt=(p)ump or probe (pp)ump & probe (pn) n external fields]",0,0,1,0}, 
  {"scatt","scattp", 's',"Scattering  [opt=(e)lectrons/(p)honons/p(h)otons/(a)ll]",0,0,1,0},
#endif
#if defined _QED && !defined _ELPH
  {"corr","corrtp", 'c',"Correlation [opt=(e)lectrons/p(h)otons/(a)ll]",0,0,1,0},
#endif
#if defined _ELPH && !defined _QED
  {"corr","corrtp", 'c',"Correlation [opt=(e)lectrons/(p)honons]",0,0,1,0},
#endif
#if defined _QED && defined _ELPH
  {"corr","corrtp", 'c',"Correlation [opt=(e)lectrons/(p)honons/p(h)otons/(a)ll]",0,0,1,0},
#endif
#if defined _ELPH && !defined _RT
  {"elphham","ElPhHam",'f',"Electron-Phonon Hamiltonian",0,0,0,0},    
#endif
#if defined _SC | defined _RT
  {"coll","collisions", 'e',"Evaluate Collisions",0,0,0,0}, 
#endif
#if defined _SC
  {"sc","scrun",      's',"Self-Consistent Single-Particle Calculations",0,0,0,0}, 
#endif
#if defined _MAGNETIC 
  {"magnetic","magnetic", 'm',"Magnetic [opt=(p)auli (l)andau (a)ll]",0,0,1,0}, 
#endif
#if defined _ELECTRIC 
  {"electric","electric", 'm',"Static Electric Field",0,0,0,0}, 
#endif
#if defined _SCALAPACK
  {"slktest","slktest",  's',"ScaLapacK test",0,0,0,0},
#endif
 };

