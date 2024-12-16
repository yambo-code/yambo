/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM

*/
#include <string.h>
#include <stdio.h>
#include <kind.h>

void options_yambo_sc(struct options_struct options[],int *i_opt)
{ 
 char *desc;
 int i_desc=0;
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
 strcpy(options[*i_opt].long_desc[i_desc],"<string>=(p)PA/(m)PA/(c)HOSEX/(r)eal-axis");
 i_desc=i_desc+1;
 strcpy(options[*i_opt].long_desc[i_desc],"<string>=fan");
 i_desc=i_desc+1;
 strcpy(options[*i_opt].long_desc[i_desc],"<string>=X");
 options[*i_opt].long_opt="gw0";
 options[*i_opt].short_opt='p';
 options[*i_opt].bin="yambo";
 options[*i_opt].yambo_string="gw0";
 options[*i_opt].char_var=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Dyson Equation solver";
 strcpy(options[*i_opt].long_desc[0],"<string>=(g)reen [any scattering]");
 strcpy(options[*i_opt].long_desc[1],"<string>=(n)ewton [order 1]/(s)ecant [e-e scattering]");
#if defined _PHEL
 strcpy(options[*i_opt].long_desc[2],"<string>=(n)ewton [order 2] [p-e scattering]");
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
 strcpy(options[*i_opt].long_desc[0],"<string>=c Reciprocal-Space");
 strcpy(options[*i_opt].long_desc[1],"<string>=b for Transition-Space Bethe-Salpeter");
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
 strcpy(options[*i_opt].long_desc[0],"<string>=(s)static/(p)PA/m(PA)/(d)ynamical dielectric matrix");
 strcpy(options[*i_opt].long_desc[1],"<string>=(X) dynamical response matrix");
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
 strcpy(options[*i_opt].long_desc[0],"<string>=hartree/alda/lrc/hf/sex/bsfxc");
 strcpy(options[*i_opt].long_desc[1],"hf/sex only eh-space; lrc only G-space");
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
 strcpy(options[*i_opt].long_desc[0],"<string>=h/d/s/(p/f)i");
#else
 strcpy(options[*i_opt].long_desc[0],"<string>=h/d/(p/f)i");
#endif
 strcpy(options[*i_opt].long_desc[1],"(h)aydock/(d)iagonalization");
 strcpy(options[*i_opt].long_desc[2],"(pi) perturbative inversion/ (fi) full inversion");
#if defined _SLEPC && !defined _NL
 strcpy(options[*i_opt].long_desc[2],"(s)lepc partial diagonalization");
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

void options_projects_sc(struct options_struct options[],int *i_opt)
{
 char *desc;
 int i_desc=0;
 desc="Hamiltonians & Potentials";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Self-Consistent Single-Particle Calculations";
 options[*i_opt].long_opt="sc";
 options[*i_opt].short_opt='s';
 options[*i_opt].bin="yambo_sc";
 options[*i_opt].yambo_string="scrun";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Self-Consistent Potential";
 strcpy(options[*i_opt].long_desc[0],"Hartree => <string>=h");
 strcpy(options[*i_opt].long_desc[1],"Fock    => <string>=f");
 strcpy(options[*i_opt].long_desc[2],"Coh     => <string>=coh");
 strcpy(options[*i_opt].long_desc[3],"Sex     => <string>=sex");
 strcpy(options[*i_opt].long_desc[4],"exx     => <string>=exx");
 strcpy(options[*i_opt].long_desc[5],"exxc    => <string>=exxc");
 strcpy(options[*i_opt].long_desc[6],"srpa    => <string>=srpa");
 strcpy(options[*i_opt].long_desc[7],"default => <string>=d");
 strcpy(options[*i_opt].long_desc[8],"IP      => <string>=ip");
 strcpy(options[*i_opt].long_desc[9],"LDA_X   => <string>=ldax");
 strcpy(options[*i_opt].long_desc[10],"PZ      => <string>=pz");
 strcpy(options[*i_opt].long_desc[11],"GS      => <string>=gs");
 strcpy(options[*i_opt].long_desc[12],"CVONLY  => <string>=cvonly (compute only cv collisions)");
 strcpy(options[*i_opt].long_desc[13]," ");
 strcpy(options[*i_opt].long_desc[14],"Potentials can be combined. Example: use hf for Hartree-Fock");
 options[*i_opt].long_opt="potential";
 options[*i_opt].short_opt='v';
 options[*i_opt].bin="yambo_sc yambo_rt yambo_nl";
 options[*i_opt].yambo_string="potential";
 options[*i_opt].char_var=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Self-Consistent Magnetic Calculations";
 options[*i_opt].long_opt="magnetic";
 options[*i_opt].bin="yambo_sc";
 options[*i_opt].yambo_string="magnetic";
 strcpy(options[*i_opt].long_desc[0],"<string>=(p)auli/(l)andau");
 options[*i_opt].char_var=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Self-Consistent Static Electric Field Calculations";
 options[*i_opt].long_opt="electric";
 options[*i_opt].bin="yambo_sc";
 options[*i_opt].yambo_string="electric";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Collisions";
 options[*i_opt].long_opt="collisions";
 options[*i_opt].short_opt='e';
 options[*i_opt].bin="yambo_rt yambo_sc yambo_nl";
 options[*i_opt].yambo_string="collisions";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Electron-Phonon Hamiltonian";
 options[*i_opt].long_opt="epham";
 options[*i_opt].bin="yambo_sc";
 options[*i_opt].yambo_string="ElPhHam";
 options[*i_opt].section=desc;

 desc="Real-Time";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="NEQ Real-time dynamics";
 strcpy(options[*i_opt].long_desc[0],"<string>=(p)ump or probe,(pp)ump & probe, (pn) n external fields");
 options[*i_opt].char_var=1;
 options[*i_opt].long_opt="rt";
 options[*i_opt].short_opt='n';
 options[*i_opt].bin="yambo_rt";
 options[*i_opt].yambo_string="negf";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Non-linear spectroscopy";
 strcpy(options[*i_opt].long_desc[0],"<string>=(p)ump or probe,(n) non-linear optics");
 options[*i_opt].long_opt="nl";
 options[*i_opt].char_var=1;
 options[*i_opt].short_opt='u';
 options[*i_opt].bin="yambo_nl";
 options[*i_opt].yambo_string="nloptics";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="NEQ scattering kind";
 i_desc=0;
 strcpy(options[*i_opt].long_desc[i_desc],"<string>=(ee):electron-electron interaction");
#if defined _QED 
 i_desc=i_desc+1;
 strcpy(options[*i_opt].long_desc[i_desc],"<string>=(eh):electron-photon interaction");
#endif
 i_desc=i_desc+1;
 strcpy(options[*i_opt].long_desc[i_desc],"<string>=(ep):electron-phonon interaction");
 i_desc=i_desc+1;
 strcpy(options[*i_opt].long_desc[i_desc],"<string>=(pe):phonon-electron interaction");
#if defined _PHEL || defined _PHEL
 i_desc=i_desc+1;
 strcpy(options[*i_opt].long_desc[i_desc]," ");
 i_desc=i_desc+1;
 strcpy(options[*i_opt].long_desc[i_desc],"Use -scattering ee+ep/ee+pe to activate more than one kind simultaneously");
#endif
 options[*i_opt].long_opt="scattering";
 options[*i_opt].short_opt='s';
 options[*i_opt].bin="yambo_rt";
 options[*i_opt].yambo_string="scattp";
 options[*i_opt].char_var=1;
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Correlation kind";
 i_desc=0;
 strcpy(options[*i_opt].long_desc[i_desc],"<string>=(ee):electron-electron interaction");
 i_desc=i_desc+1;
 strcpy(options[*i_opt].long_desc[i_desc],"<string>=(ep):electron-phonon interaction");
 i_desc=i_desc+1;
 strcpy(options[*i_opt].long_desc[i_desc],"<string>=(pe):phonon-electron interaction");
 options[*i_opt].long_opt="correlation";
 options[*i_opt].short_opt='c';
 options[*i_opt].bin="yambo_sc yambo_qed";
 options[*i_opt].yambo_string="corrtp";
 options[*i_opt].section="Self-Energy";
 options[*i_opt].char_var=1;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Photo-Luminescence";
 options[*i_opt].long_opt="pl";
 options[*i_opt].short_opt='u';
 options[*i_opt].bin="yambo_pl";
 options[*i_opt].yambo_string="photolum";
 options[*i_opt].section=desc;
};
