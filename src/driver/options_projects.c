/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM

*/
#include <stdio.h>
#include <kind.h>

void options_projects(struct options_struct options[],int *i_opt)
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
 options[*i_opt].long_desc[0]="Hartree => <string>=h";
 options[*i_opt].long_desc[1]="Fock    => <string>=f";
 options[*i_opt].long_desc[2]="Coh     => <string>=coh";
 options[*i_opt].long_desc[3]="Sex     => <string>=sex";
 options[*i_opt].long_desc[4]="exx     => <string>=exx";
 options[*i_opt].long_desc[5]="exxc    => <string>=exxc";
 options[*i_opt].long_desc[6]="srpa    => <string>=srpa";
 options[*i_opt].long_desc[7]="default => <string>=d";
 options[*i_opt].long_desc[8]="IP      => <string>=ip";
 options[*i_opt].long_desc[9]="LDA_X   => <string>=ldax";
 options[*i_opt].long_desc[10]="PZ      => <string>=pz";
 options[*i_opt].long_desc[11]="GS      => <string>=gs";
 options[*i_opt].long_desc[12]="CVONLY  => <string>=cvonly (compute only cv collisions)";
 options[*i_opt].long_desc[13]=" ";
 options[*i_opt].long_desc[14]="Potentials can be combined. Example: use hf for Hartree-Fock";
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
 options[*i_opt].long_desc[0]="<string>=(p)auli/(l)andau";
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
 options[*i_opt].bin="yambo_ph";
 options[*i_opt].yambo_string="ElPhHam";
 options[*i_opt].section=desc;

 desc="Real-Time";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="NEQ Real-time dynamics";
 options[*i_opt].long_desc[0]="<string>=(p)ump or probe,(pp)ump & probe, (pn) n external fields";
 options[*i_opt].char_var=1;
 options[*i_opt].long_opt="rt";
 options[*i_opt].short_opt='n';
 options[*i_opt].bin="yambo_rt";
 options[*i_opt].yambo_string="negf";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Non-linear spectroscopy";
 options[*i_opt].long_desc[0]="<string>=(p)ump or probe,(n) non-linear optics";
 options[*i_opt].long_opt="nl";
 options[*i_opt].char_var=1;
 options[*i_opt].short_opt='u';
 options[*i_opt].bin="yambo_nl";
 options[*i_opt].yambo_string="nloptics";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="NEQ scattering kind";
 i_desc=0;
 options[*i_opt].long_desc[i_desc]="<string>=(ee):electron-electron interaction";
#if defined _QED 
 i_desc=i_desc+1;
 options[*i_opt].long_desc[i_desc]="<string>=(eh):electron-photon interaction";
#endif
#if defined _ELPH 
 i_desc=i_desc+1;
 options[*i_opt].long_desc[i_desc]="<string>=(ep):electron-phonon interaction";
#endif
#if defined _ELPH 
 i_desc=i_desc+1;
 options[*i_opt].long_desc[i_desc]="<string>=(pe):phonon-electron interaction";
#endif
#if defined _PHEL || defined _PHEL
 i_desc=i_desc+1;
 options[*i_opt].long_desc[i_desc]=" ";
 i_desc=i_desc+1;
 options[*i_opt].long_desc[i_desc]="Use -scattering ee+ep/ee+pe to activate more than one kind simultaneously";
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
 options[*i_opt].long_desc[i_desc]="<string>=(ee):electron-electron interaction";
#if defined _QED 
 i_desc=i_desc+1;
 options[*i_opt].long_desc[i_desc]="<string>=(eh):electron-photon interaction";
#endif
#if defined _ELPH 
 i_desc=i_desc+1;
 options[*i_opt].long_desc[i_desc]="<string>=(ep):electron-phonon interaction";
#endif
#if defined _ELPH 
 i_desc=i_desc+1;
 options[*i_opt].long_desc[i_desc]="<string>=(pe):phonon-electron interaction";
#endif
 options[*i_opt].long_opt="correlation";
 options[*i_opt].short_opt='c';
 options[*i_opt].bin="yambo_ph yambo_qed";
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
