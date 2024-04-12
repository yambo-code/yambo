/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM

*/
#include <string.h>
#include <stdio.h>
#include <kind.h>

void options_yambo(struct options_struct options[],int *i_opt)
{ 
 char *desc;
 int i_desc=0;
 int s_size;

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
 s_size=sizeof("<string>=(p)PA/(m)PA/(c)HOSEX/(r)eal-axis");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s","<string>=(p)PA/(m)PA/(c)HOSEX/(r)eal-axis");
#if defined _ELPH
 i_desc=i_desc+1;
 s_size=sizeof("<string>=fan");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s","<string>=fan");
#endif
#if defined _ELPH
 i_desc=i_desc+1;
 s_size=sizeof("<string>=X");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s","<string>=X");
#endif
 options[*i_opt].long_opt="gw0";
 options[*i_opt].short_opt='p';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="gw0";
 options[*i_opt].char_var=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Dyson Equation solver";
 s_size=sizeof("<string>=(g)reen [any scattering]");
 snprintf(options[*i_opt].long_desc[0],s_size,"%s","<string>=(g)reen [any scattering]");
 s_size=sizeof("<string>=(n)ewton [order 1]/(s)ecant [e-e scattering]");
 snprintf(options[*i_opt].long_desc[1],s_size,"%s","<string>=(n)ewton [order 1]/(s)ecant [e-e scattering]");
#if defined _PHEL
 s_size=sizeof("<string>=(n)ewton [order 2] [p-e scattering]");
 snprintf(options[*i_opt].long_desc[2],s_size,"%s","<string>=(n)ewton [order 2] [p-e scattering]");
#endif
 options[*i_opt].long_opt="dyson";
 options[*i_opt].short_opt='g';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="dyson";
 options[*i_opt].char_var=1;
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
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Screened coulomb potential";
 options[*i_opt].long_opt="rw";
 options[*i_opt].short_opt='w';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="rim_w";
 options[*i_opt].section=desc;


 desc="Response Functions";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Linear Response optical properties";
 s_size=sizeof("<string>=c Reciprocal-Space");
 snprintf(options[*i_opt].long_desc[0],s_size,"%s","<string>=c Reciprocal-Space");
 s_size=sizeof("<string>=b for Transition-Space Bethe-Salpeter");
 snprintf(options[*i_opt].long_desc[1],s_size,"%s","<string>=b for Transition-Space Bethe-Salpeter");
 options[*i_opt].long_opt="optics";
 options[*i_opt].short_opt='o';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="optics";
 options[*i_opt].char_var=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Inverse Dielectric/Response Matrix";
 options[*i_opt].long_opt="X";
 options[*i_opt].short_opt='d';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="screen";
 options[*i_opt].section=desc;
 s_size=sizeof("<string>=(s)static/(p)PA/m(PA)/(d)ynamical dielectric matrix");
 snprintf(options[*i_opt].long_desc[0],s_size,"%s","<string>=(s)static/(p)PA/m(PA)/(d)ynamical dielectric matrix");
 s_size=sizeof("<string>=(X) dynamical response matrix");
 snprintf(options[*i_opt].long_desc[1],s_size,"%s","<string>=(X) dynamical response matrix");
 options[*i_opt].char_var=1;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Oscillator strenghts (or dipoles)";
 options[*i_opt].long_opt="dipoles";
 options[*i_opt].short_opt='q';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="dipoles";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Kernel";
 s_size=sizeof("<string>=hartree/alda/lrc/hf/sex/bsfxc");
 snprintf(options[*i_opt].long_desc[0],s_size,"%s","<string>=hartree/alda/lrc/hf/sex/bsfxc");
 s_size=sizeof("hf/sex only eh-space; lrc only G-space");
 snprintf(options[*i_opt].long_desc[1],s_size,"%s","hf/sex only eh-space; lrc only G-space");
 options[*i_opt].long_opt="kernel";
 options[*i_opt].short_opt='k';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="kernel";
 options[*i_opt].char_var=1;
 options[*i_opt].section=desc;

 desc="Bethe-Salpeter Equation";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="BSE solver";
#if defined _SLEPC && !defined _NL
 s_size=sizeof("<string>=h/d/s/(p/f)i");
 snprintf(options[*i_opt].long_desc[0],s_size,"%s","<string>=h/d/s/(p/f)i");
#else
 s_size=sizeof("<string>=h/d/(p/f)i");
 snprintf(options[*i_opt].long_desc[0],s_size,"%s","<string>=h/d/(p/f)i");
#endif
 s_size=sizeof("(h)aydock/(d)iagonalization");
 snprintf(options[*i_opt].long_desc[1],s_size,"%s","(h)aydock/(d)iagonalization");
 s_size=sizeof("(pi) perturbative inversion/ (fi) full inversion");
 snprintf(options[*i_opt].long_desc[2],s_size,"%s","(pi) perturbative inversion/ (fi) full inversion");
#if defined _SLEPC && !defined _NL
 s_size=sizeof("(s)lepc partial diagonalization");
 snprintf(options[*i_opt].long_desc[2],s_size,"%s","(s)lepc partial diagonalization");
#endif
 options[*i_opt].long_opt="Ksolver";
 options[*i_opt].short_opt='y';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="bss";
 options[*i_opt].char_var=1;
 options[*i_opt].section=desc;

 desc="Total Energy";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="ACFDT Total Energy";
 options[*i_opt].long_opt="acfdt";
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="acfdt";
 options[*i_opt].section=desc;

};
