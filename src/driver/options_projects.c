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
 strlcpy(options[*i_opt].long_desc[0],"Hartree => <string>=h",s_size);

 s_size=sizeof("Fock    => <string>=f");
 strlcpy(options[*i_opt].long_desc[1],"Fock    => <string>=f",s_size);
 
 s_size=sizeof("Coh     => <string>=coh");
 strlcpy(options[*i_opt].long_desc[2],"Coh     => <string>=coh",s_size);

 s_size=sizeof("Sex     => <string>=sex");
 strlcpy(options[*i_opt].long_desc[3],"Sex     => <string>=sex",s_size);

 s_size=sizeof("exx     => <string>=exx");
 strlcpy(options[*i_opt].long_desc[4],"exx     => <string>=exx",s_size);

 s_size=sizeof("exxc    => <string>=exxc");
 strlcpy(options[*i_opt].long_desc[5],"exxc    => <string>=exxc",s_size);

 s_size=sizeof("srpa    => <string>=srpa");
 strlcpy(options[*i_opt].long_desc[6],"srpa    => <string>=srpa",s_size);

 s_size=sizeof("default => <string>=d");
 strlcpy(options[*i_opt].long_desc[7],"default => <string>=d",s_size);

 s_size=sizeof("IP      => <string>=ip");
 strlcpy(options[*i_opt].long_desc[8],"IP      => <string>=ip",s_size);

 s_size=sizeof("LDA_X   => <string>=ldax");
 strlcpy(options[*i_opt].long_desc[9],"LDA_X   => <string>=ldax",s_size);

 s_size=sizeof("PZ      => <string>=pz");
 strlcpy(options[*i_opt].long_desc[10],"PZ      => <string>=pz",s_size);

 s_size=sizeof("GS      => <string>=gs");
 strlcpy(options[*i_opt].long_desc[11],"GS      => <string>=gs",s_size);

 s_size=sizeof("CVONLY  => <string>=cvonly (compute only cv collisions)");
 strlcpy(options[*i_opt].long_desc[12],"CVONLY  => <string>=cvonly (compute only cv collisions)",s_size);

 s_size=sizeof(" ");
 strlcpy(options[*i_opt].long_desc[13]," ",s_size);

 s_size=sizeof("Potentials can be combined. Example: use hf for Hartree-Fock");
 strlcpy(options[*i_opt].long_desc[14],"Potentials can be combined. Example: use hf for Hartree-Fock",s_size);

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
 strlcpy(options[*i_opt].long_desc[0],"<string>=(p)auli/(l)andau",s_size);
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
 strlcpy(options[*i_opt].long_desc[0],"<string>=(p)ump or probe,(pp)ump & probe, (pn) n external fields",s_size);
 options[*i_opt].char_var=1;
 options[*i_opt].long_opt="rt";
 options[*i_opt].short_opt='n';
 options[*i_opt].bin="yambo_rt";
 options[*i_opt].yambo_string="negf";
 options[*i_opt].section=desc;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Non-linear spectroscopy";
 s_size=sizeof("<string>=(p)ump or probe,(n) non-linear optics");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(p)ump or probe,(n) non-linear optics",s_size);
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
 strlcpy(options[*i_opt].long_desc[i_desc],"<string>=(ee):electron-electron interaction",s_size);
#if defined _QED 
 i_desc=i_desc+1;
 s_size=sizeof("<string>=(eh):electron-photon interaction");
 strlcpy(options[*i_opt].long_desc[i_desc],"<string>=(eh):electron-photon interaction",s_size);
#endif
#if defined _ELPH 
 i_desc=i_desc+1;
 s_size=sizeof("<string>=(ep):electron-phonon interaction");
 strlcpy(options[*i_opt].long_desc[i_desc],"<string>=(ep):electron-phonon interaction",s_size);
#endif
#if defined _ELPH 
 i_desc=i_desc+1;
 s_size=sizeof("<string>=(pe):phonon-electron interaction");
 strlcpy(options[*i_opt].long_desc[i_desc],"<string>=(pe):phonon-electron interaction",s_size);
#endif
#if defined _PHEL || defined _PHEL
 i_desc=i_desc+1;
 s_size=sizeof(" ");
 strlcpy(options[*i_opt].long_desc[i_desc]," ",s_size);
 i_desc=i_desc+1;
 s_size=sizeof("Use -scattering ee+ep/ee+pe to activate more than one kind simultaneously");
 strlcpy(options[*i_opt].long_desc[i_desc],"Use -scattering ee+ep/ee+pe to activate more than one kind simultaneously",s_size);
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
 strlcpy(options[*i_opt].long_desc[i_desc],"<string>=(ee):electron-electron interaction",s_size);
#if defined _QED 
 i_desc=i_desc+1;
 s_size=sizeof("<string>=(eh):electron-photon interaction");
 strlcpy(options[*i_opt].long_desc[i_desc],"<string>=(eh):electron-photon interaction",s_size);
#endif
#if defined _ELPH 
 i_desc=i_desc+1;
 s_size=sizeof("<string>=(ep):electron-phonon interaction");
 strlcpy(options[*i_opt].long_desc[i_desc],"<string>=(ep):electron-phonon interaction",s_size);
#endif
#if defined _ELPH 
 i_desc=i_desc+1;
 s_size=sizeof("<string>=(pe):phonon-electron interaction");
 strlcpy(options[*i_opt].long_desc[i_desc],"<string>=(pe):phonon-electron interaction",s_size);
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
