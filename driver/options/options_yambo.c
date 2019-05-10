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

void options_yambo(struct options_struct options[],int *i_opt)
{ 
 char *desc;
 desc="Self-Energy";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Hartree-Fock";
 options[*i_opt].long_opt="hf";
 options[*i_opt].short_opt='x';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="HF_and_locXC";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="GW approximation";
 options[*i_opt].long_desc[0]="<string>=(p)PA/(c)HOSEX";
 options[*i_opt].long_opt="gw";
 options[*i_opt].short_opt='p';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="gwapprx";
 options[*i_opt].n_char=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Dyson Equation solver";
 options[*i_opt].long_desc[0]="<string>=(n)ewton/(s)ecant/(g)reen";
 options[*i_opt].long_opt="dyson";
 options[*i_opt].short_opt='g';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="gw0";
 options[*i_opt].n_char=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="GoWo Quasiparticle lifetimes";
 options[*i_opt].long_opt="lifetimes";
 options[*i_opt].short_opt='l';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="life";
 options[*i_opt].section=desc;

 desc="Initializations";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Initialization";
 options[*i_opt].short_opt='i';
 options[*i_opt].long_opt="setup";
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="setup";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Coulomb potential";
 options[*i_opt].long_opt="coulomb";
 options[*i_opt].short_opt='r';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="rim_cut";
 options[*i_opt].section=desc;

 desc="Response Functions";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Linear Response optical properties";
 options[*i_opt].long_desc[0]="<string>=c Reciprocal-Space";
 options[*i_opt].long_desc[1]="<string>=b for Transition-Space Bethe-Salpeter";
 options[*i_opt].long_opt="optics";
 options[*i_opt].short_opt='o';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="optics";
 options[*i_opt].n_char=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Dynamical Inverse Dielectric Matrix";
 options[*i_opt].long_opt="Xd";
 options[*i_opt].short_opt='d';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="em1d";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Statical Inverse Dielectric Matrix";
 options[*i_opt].long_opt="Xs";
 options[*i_opt].short_opt='b';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="em1s";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Oscillator strenghts (or dipoles)";
 options[*i_opt].long_opt="dipoles";
 options[*i_opt].short_opt='q';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="dipoles";
 options[*i_opt].section=desc;

 desc="Bethe-Salpeter Equation";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Kernel";
 options[*i_opt].long_desc[0]="<string>=hartree/alda/lrc/hf/sex/bsfxc";
 options[*i_opt].long_desc[1]="hf/sex only eh-space; lrc only G-space";
 options[*i_opt].long_opt="kernel";
 options[*i_opt].short_opt='k';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="kernel";
 options[*i_opt].n_char=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="BSE solver";
 options[*i_opt].long_desc[0]="<string>=h/d/s/(p/f)i/t";
 options[*i_opt].long_desc[1]="(h)aydock/(d)iagonalization/(i)nversion";
 options[*i_opt].long_opt="Ksolver";
 options[*i_opt].short_opt='y';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="bss";
 options[*i_opt].n_char=1;
 options[*i_opt].section=desc;

 desc="Total Energy";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="ACFDT Total Energy";
 options[*i_opt].long_opt="acfdt";
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="acfdt";
 options[*i_opt].section=desc;

};
