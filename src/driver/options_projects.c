/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM

*/
#include <string.h>
#include <stdio.h>
#include <kind.h>

void options_projects(struct options_struct options[],int *i_opt)
{
 char *desc;
 int i_desc=0;
 int s_size;
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
 s_size=sizeof("Hartree => <string>=h");
 snprintf(options[*i_opt].long_desc[0],s_size,"%s","Hartree => <string>=h");

 s_size=sizeof("Fock    => <string>=f");
 snprintf(options[*i_opt].long_desc[1],s_size,"%s","Fock    => <string>=f");
 
 s_size=sizeof("Coh     => <string>=coh");
 snprintf(options[*i_opt].long_desc[2],s_size,"%s","Coh     => <string>=coh");

 s_size=sizeof("Sex     => <string>=sex");
 snprintf(options[*i_opt].long_desc[3],s_size,"%s","Sex     => <string>=sex");

 s_size=sizeof("exx     => <string>=exx");
 snprintf(options[*i_opt].long_desc[4],s_size,"%s","exx     => <string>=exx");

 s_size=sizeof("exxc    => <string>=exxc");
 snprintf(options[*i_opt].long_desc[5],s_size,"%s","exxc    => <string>=exxc");

 s_size=sizeof("srpa    => <string>=srpa");
 snprintf(options[*i_opt].long_desc[6],s_size,"%s","srpa    => <string>=srpa");

 s_size=sizeof("default => <string>=d");
 snprintf(options[*i_opt].long_desc[7],s_size,"%s","default => <string>=d");

 s_size=sizeof("IP      => <string>=ip");
 snprintf(options[*i_opt].long_desc[8],s_size,"%s","IP      => <string>=ip");

 s_size=sizeof("LDA_X   => <string>=ldax");
 snprintf(options[*i_opt].long_desc[9],s_size,"%s","LDA_X   => <string>=ldax");

 s_size=sizeof("PZ      => <string>=pz");
 snprintf(options[*i_opt].long_desc[10],s_size,"%s","PZ      => <string>=pz");

 s_size=sizeof("GS      => <string>=gs");
 snprintf(options[*i_opt].long_desc[11],s_size,"%s","GS      => <string>=gs");

 s_size=sizeof("CVONLY  => <string>=cvonly (compute only cv collisions)");
 snprintf(options[*i_opt].long_desc[12],s_size,"%s","CVONLY  => <string>=cvonly (compute only cv collisions)");

 s_size=sizeof(" ");
 snprintf(options[*i_opt].long_desc[13],s_size,"%s"," ");

 s_size=sizeof("Potentials can be combined. Example: use hf for Hartree-Fock");
 snprintf(options[*i_opt].long_desc[14],s_size,"%s","Potentials can be combined. Example: use hf for Hartree-Fock");

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
 s_size=sizeof("<string>=(p)auli/(l)andau");
 snprintf(options[*i_opt].long_desc[0],s_size,"%s","<string>=(p)auli/(l)andau");
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
 s_size=sizeof("<string>=(p)ump or probe,(pp)ump & probe, (pn) n external fields");
 snprintf(options[*i_opt].long_desc[0],s_size,"%s","<string>=(p)ump or probe,(pp)ump & probe, (pn) n external fields");
 options[*i_opt].char_var=1;
 options[*i_opt].long_opt="rt";
 options[*i_opt].short_opt='n';
 options[*i_opt].bin="yambo_rt";
 options[*i_opt].yambo_string="negf";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Non-linear spectroscopy";
 s_size=sizeof("<string>=(p)ump or probe,(n) non-linear optics");
 snprintf(options[*i_opt].long_desc[0],s_size,"%s","<string>=(p)ump or probe,(n) non-linear optics");
 options[*i_opt].long_opt="nl";
 options[*i_opt].char_var=1;
 options[*i_opt].short_opt='u';
 options[*i_opt].bin="yambo_nl";
 options[*i_opt].yambo_string="nloptics";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="NEQ scattering kind";
 i_desc=0;
 s_size=sizeof("<string>=(ee):electron-electron interaction");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s","<string>=(ee):electron-electron interaction");
#if defined _QED 
 i_desc=i_desc+1;
 s_size=sizeof("<string>=(eh):electron-photon interaction");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s","<string>=(eh):electron-photon interaction");
#endif
#if defined _ELPH 
 i_desc=i_desc+1;
 s_size=sizeof("<string>=(ep):electron-phonon interaction");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s","<string>=(ep):electron-phonon interaction");
#endif
#if defined _ELPH 
 i_desc=i_desc+1;
 s_size=sizeof("<string>=(pe):phonon-electron interaction");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s","<string>=(pe):phonon-electron interaction");
#endif
#if defined _PHEL || defined _PHEL
 i_desc=i_desc+1;
 s_size=sizeof(" ");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s"," ");
 i_desc=i_desc+1;
 s_size=sizeof("Use -scattering ee+ep/ee+pe to activate more than one kind simultaneously");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s","Use -scattering ee+ep/ee+pe to activate more than one kind simultaneously");
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
 s_size=sizeof("<string>=(ee):electron-electron interaction");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s","<string>=(ee):electron-electron interaction");
#if defined _QED 
 i_desc=i_desc+1;
 s_size=sizeof("<string>=(eh):electron-photon interaction");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s","<string>=(eh):electron-photon interaction");
#endif
#if defined _ELPH 
 i_desc=i_desc+1;
 s_size=sizeof("<string>=(ep):electron-phonon interaction");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s","<string>=(ep):electron-phonon interaction");
#endif
#if defined _ELPH 
 i_desc=i_desc+1;
 s_size=sizeof("<string>=(pe):phonon-electron interaction");
 snprintf(options[*i_opt].long_desc[i_desc],s_size,"%s","<string>=(pe):phonon-electron interaction");
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
