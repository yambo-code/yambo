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
 int i_opt,dummy,i;
 for(i_opt=0;i_opt<100;i_opt++) {
  n_options[i_opt].long_opt=NULL;
  n_options[i_opt].short_opt=0;
  n_options[i_opt].short_desc=NULL;
  for(i=0;i<=10;i++) n_options[i_opt].long_desc[i]=NULL;
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
 n_options[i_opt].short_desc="Help";
 n_options[i_opt].short_opt='h';
 n_options[i_opt].long_opt="help"; /*Help*/
 n_options[i_opt].serial_var=1;
 i_opt++;
 n_options[i_opt].short_desc="<string> can be an option (e.g. -info optics) or 'version' to get the code configuration";
 n_options[i_opt].long_opt="info"; /*info*/
 n_options[i_opt].serial_var=1;
 n_options[i_opt].n_char=1;
 /* 
  Input file 
 */
 i_opt++;
 n_options[i_opt].short_desc="Input file variables verbosity";
 n_options[i_opt].long_desc[0]="<string> can be RL,kpt,sc,qp,io,gen,resp,all,par";
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
 n_options[i_opt].long_opt="Idir"; 
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="I/O directory";
 n_options[i_opt].short_opt='O';
 n_options[i_opt].long_opt="Odir";
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="Communication directory";
 n_options[i_opt].short_opt='C';
 n_options[i_opt].long_opt="Cdir";
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="Walltime";
 n_options[i_opt].long_desc[0]="Format is DdHhMm with D=days, H=hours and M=minutes";
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
  
  Yambo
  =====

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
 n_options[i_opt].long_desc[0]="<string>=c Reciprocal-Space";
 n_options[i_opt].long_desc[1]="<string>=b for Transition-Space Bethe-Salpeter";
 n_options[i_opt].long_opt="optics";
 n_options[i_opt].short_opt='o';
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="optics";
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="Kernel";
 n_options[i_opt].long_desc[0]="<string>=hartree/alda/lrc/hf/sex/bsfxc";
 n_options[i_opt].long_desc[1]="hf/sex only eh-space; lrc only G-space";
 n_options[i_opt].long_opt="kernel";
 n_options[i_opt].short_opt='k';
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="kernel";
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="BSE solver";
 n_options[i_opt].long_desc[0]="<string>=h/d/s/(p/f)i/t";
 n_options[i_opt].long_desc[1]="(h)aydock/(d)iagonalization/(i)nversion";
 n_options[i_opt].long_opt="Ksolver";
 n_options[i_opt].short_opt='y';
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="bss";
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="Coulomb potential";
 n_options[i_opt].long_opt="coulomb";
 n_options[i_opt].short_opt='r';
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="rim_cut";
 i_opt++;
 n_options[i_opt].short_desc="Dynamical Inverse Dielectric Matrix";
 n_options[i_opt].long_opt="Xd";
 n_options[i_opt].short_opt='d';
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="em1d";
 i_opt++;
 n_options[i_opt].short_desc="Statical Inverse Dielectric Matrix";
 n_options[i_opt].long_opt="Xs";
 n_options[i_opt].short_opt='b';
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="em1s";
 i_opt++;
 n_options[i_opt].short_desc="Oscillator strenghts (or dipoles)";
 n_options[i_opt].long_opt="dipoles";
 n_options[i_opt].short_opt='q';
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="dipoles";
 i_opt++;
 n_options[i_opt].short_desc="GW approximation";
 n_options[i_opt].long_desc[0]="<string>=(p)PA/(c)HOSEX";
 n_options[i_opt].long_opt="selfenergy";
 n_options[i_opt].short_opt='p';
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="gwapprox";
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="Dyson Equation solver";
 n_options[i_opt].long_desc[0]="<string>=(n)ewton/(s)ecant/(g)reen";
 n_options[i_opt].long_opt="Dsolver";
 n_options[i_opt].short_opt='g';
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="gw0";
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="GoWo Quasiparticle lifetimes";
 n_options[i_opt].long_opt="lifetimes";
 n_options[i_opt].short_opt='l';
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="life";
 i_opt++;
 n_options[i_opt].short_desc="ACFDT Total Energy";
 n_options[i_opt].long_opt="acfdt";
 n_options[i_opt].project="yambo";
 n_options[i_opt].yambo_string="acfdt";
 /* 

  Projects
  ========

 */
 i_opt++;
 n_options[i_opt].short_desc="Self-Consistent Potential";
 n_options[i_opt].long_desc[0]="<string>=(h)artree,(f)ock,(coh),(sex),(cohsex),(exx),(exxc),(srpa),(d)ef,(ip)";
 n_options[i_opt].long_desc[1]="(h)artree can be combined with others, i.e. hf, hsex, ... ";
 n_options[i_opt].long_opt="potential";
 n_options[i_opt].short_opt='v';
 n_options[i_opt].project="yambo_sc yambo_magnetic yambo_rt yambo_electric";
 n_options[i_opt].yambo_string="potential";
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="Photo-Luminescence";
 n_options[i_opt].long_opt="pl";
 n_options[i_opt].short_opt='u';
 n_options[i_opt].project="yambo_pl";
 n_options[i_opt].yambo_string="photolum";
 i_opt++;
 n_options[i_opt].short_desc="Non-linear spectroscopy";
 n_options[i_opt].long_opt="nl";
 n_options[i_opt].short_opt='u';
 n_options[i_opt].project="yambo_nl";
 n_options[i_opt].yambo_string="nloptics";
 i_opt++;
 n_options[i_opt].short_desc="NEQ Real-time dynamics";
 n_options[i_opt].long_desc[0]="<string>=(p)ump or probe,(pp)ump & probe, (pn) n external fields";
 n_options[i_opt].long_opt="rt";
 n_options[i_opt].short_opt='n';
 n_options[i_opt].project="yambo_rt";
 n_options[i_opt].yambo_string="negf";
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="NEQ scattering kind";
 n_options[i_opt].long_desc[0]="<string>=(e)lectrons/(p)honons/p(h)otons/(a)ll";
 n_options[i_opt].long_opt="scattering";
 n_options[i_opt].short_opt='s';
 n_options[i_opt].project="yambo_rt";
 n_options[i_opt].yambo_string="scattp";
 n_options[i_opt].n_char=1;
 i_opt++;
 n_options[i_opt].short_desc="Self-Consistent Single-Particle Calculations";
 n_options[i_opt].long_opt="sc";
 n_options[i_opt].short_opt='s';
 n_options[i_opt].project="yambo_sc";
 n_options[i_opt].yambo_string="scrun";
/*
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
*/
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
