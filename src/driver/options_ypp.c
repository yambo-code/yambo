/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM

*/
#include <string.h>
#include <stdio.h>
#include <kind.h>

void options_ypp(struct options_struct options[],int *i_opt)
{
 int s_size;
 /* 
  Brillouin Zone 
 */
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="BZ Grid generator";
 s_size=sizeof("<string>=(k)pt,(q)pt,(s)hifted,(h)igh symmetry,(r)andom,r(e)gular");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(k)pt,(q)pt,(s)hifted,(h)igh symmetry,(r)andom,r(e)gular",s_size);
 options[*i_opt].long_opt="grid";
 options[*i_opt].short_opt='k';
 options[*i_opt].bin="ypp";
 options[*i_opt].char_var=1;
 options[*i_opt].yambo_string="bzgrids";
 options[*i_opt].section="Brillouin Zone";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Fine to coarse grid Map";
 options[*i_opt].long_opt="map";
 options[*i_opt].short_opt='m';
 options[*i_opt].bin="ypp";
 options[*i_opt].yambo_string="kpts_map";
 options[*i_opt].section="Brillouin Zone";
 /* 
  Wannier
 */
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Wannier 90 interface";
 options[*i_opt].long_opt=  "wannier";
 options[*i_opt].bin="ypp";
 options[*i_opt].yambo_string="wannier";
 options[*i_opt].section="Wannier";
 /* 
  SOC
 */
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Perturbative SOC mapping";
 options[*i_opt].long_opt="soc";
 options[*i_opt].short_opt='w';
 options[*i_opt].bin="ypp";
 options[*i_opt].yambo_string="WFs_SOC_map";
 options[*i_opt].section="SOC";
 /* 
  Convertions
 */
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Remove symmetries not consistent with an external perturbation";
 options[*i_opt].long_opt=  "fixsym";
 options[*i_opt].short_opt='y';
 options[*i_opt].bin="ypp";
 options[*i_opt].yambo_string="fixsyms";
 options[*i_opt].section="Convertions";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Quasiparticle Databases";
 s_size=sizeof("<string>=(g)enerate-modify/(m)erge/(e)xpand");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(g)enerate-modify/(m)erge/(e)xpand",s_size);
 s_size=sizeof("         (e)xpand uses the symmetries to generate a BZ-expanded QP database");
 strlcpy(options[*i_opt].long_desc[1],"         (e)xpand uses the symmetries to generate a BZ-expanded QP database",s_size);
 options[*i_opt].long_opt="qpdb";
 options[*i_opt].bin="ypp";
 options[*i_opt].yambo_string="QPDBs";
 options[*i_opt].section="Convertions";
 options[*i_opt].char_var=1;
#if !defined _YPP_RT  
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="gkkp databases";
 options[*i_opt].long_opt= "gkkp";
 options[*i_opt].short_opt='g';
 options[*i_opt].bin="ypp_ph";
 options[*i_opt].yambo_string="gkkp"  ;
 options[*i_opt].section="Convertions";
 s_size=sizeof("<string>=(g)kkp,(d)ouble grid,(p)lot gkkp");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(g)kkp,(d)ouble grid,(p)lot gkkp",s_size);
 options[*i_opt].char_var=1;
#endif
 /* 
  Plots
 */
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Excitonic State Momentum";
 options[*i_opt].long_opt= "BSiq";
 options[*i_opt].short_opt='b';
 options[*i_opt].bin="ypp";
 options[*i_opt].yambo_string="BSiq";
 options[*i_opt].section="Plots";
 options[*i_opt].int_var=1;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Mean Potential";
 options[*i_opt].long_opt= "potential";
 options[*i_opt].short_opt='v';
 options[*i_opt].bin="ypp_sc";
 options[*i_opt].yambo_string="MeanPot";
 options[*i_opt].section="Plots";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Electronic properties";
 s_size=sizeof("<string>=(h)artree,(f)ock,(coh),(sex),(cohsex),(exx),(exxc),(srpa),(d)ef,(ip)");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(h)artree,(f)ock,(coh),(sex),(cohsex),(exx),(exxc),(srpa),(d)ef,(ip)",s_size);
#if defined _ELPH  
 s_size=sizeof("<string>=(w)ave,(d)ensity,(m)ag,do(s),(b)ands,(c)urrent,(e)lias");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(w)ave,(d)ensity,(m)ag,do(s),(b)ands,(c)urrent,(e)lias",s_size);
#elif defined _YPP_MAGNETIC 
 s_size=sizeof("<string>=(w)ave,(d)ensity,(m)ag,do(s),(b)ands,(c)urrent,angu(l)ar,(p)osition");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(w)ave,(d)ensity,(m)ag,do(s),(b)ands,(c)urrent,angu(l)ar,(p)osition",s_size);
#else
 s_size=sizeof("<string>=(w)ave,(d)ensity,(m)ag,do(s),(b)ands,(c)urrent");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(w)ave,(d)ensity,(m)ag,do(s),(b)ands,(c)urrent",s_size);
#endif
 options[*i_opt].long_opt="electron";
 options[*i_opt].short_opt='s';
 options[*i_opt].bin="ypp"; 
 options[*i_opt].yambo_string="electrons";
 options[*i_opt].section="Plots";
 options[*i_opt].char_var=1;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Excitonic properties";
 options[*i_opt].long_opt="exciton";
 options[*i_opt].short_opt='e';
 options[*i_opt].bin="ypp ypp_ph";
#if defined _ELPH  
 s_size=sizeof("<string>=(s)ort,(sp)in,(a)mplitude,(w)ave,(i)nterpolate,");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(s)ort,(sp)in,(a)mplitude,(w)ave,(i)nterpolate,",s_size);
 s_size=sizeof("         (e)lias,(g)kkp,(p)h-assisted dos");
 strlcpy(options[*i_opt].long_desc[1],"         (e)lias,(g)kkp,(p)h-assisted dos",s_size);
#else
 s_size=sizeof("<string>=(s)ort,(sp)in,(a)mplitude,(w)ave,(i)nterpolate");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(s)ort,(sp)in,(a)mplitude,(w)ave,(i)nterpolate",s_size);
#endif
 options[*i_opt].yambo_string="excitons";
 options[*i_opt].section="Plots";
 options[*i_opt].char_var=1;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Dipole properties";
 options[*i_opt].long_opt="dipoles";
 options[*i_opt].bin="ypp";
 s_size=sizeof("<string>=(exc)itonic,(ip)independent-particle");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(exc)itonic,(ip)independent-particle",s_size);
#if defined _YPP_RT  
 s_size=sizeof("<string>=(exc)itonic,(ip)independent-particle,(m)ask");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(exc)itonic,(ip)independent-particle,(m)ask",s_size);
#endif
 options[*i_opt].yambo_string="dipoles";
 options[*i_opt].section="Plots";
 options[*i_opt].char_var=1;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Free hole position in the excitonic plot";
 options[*i_opt].long_opt="freehole";
 options[*i_opt].bin="ypp";
 options[*i_opt].yambo_string="freehole";
 options[*i_opt].section="Plots";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Average hole/electron wavefunction";
 options[*i_opt].long_opt="avehole";
 options[*i_opt].bin="ypp";
 options[*i_opt].yambo_string="avehole";
 options[*i_opt].section="Plots";
#if !defined _YPP_RT  
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Phononic properties";
 s_size=sizeof("<string>=(d)os,(e)lias,(a)mplitude");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(d)os,(e)lias,(a)mplitude",s_size);
 options[*i_opt].long_opt=  "phonon";
 options[*i_opt].short_opt='p';
 options[*i_opt].bin="ypp_ph";
 options[*i_opt].yambo_string="phonons";
 options[*i_opt].section="Plots";
 options[*i_opt].char_var=1;
#endif
/*
  Real-Time
*/
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Non-linear response analysis";
 options[*i_opt].long_opt="nl";
 options[*i_opt].short_opt='u';
 options[*i_opt].bin="ypp_nl";
 options[*i_opt].yambo_string="nonlinear";
 options[*i_opt].section="Real-Time";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Carriers database generation";
 s_size=sizeof("<string>=(e)nergy,(f)ermi");
 strlcpy(options[*i_opt].long_desc[0],"<string>=(e)nergy,(f)ermi",s_size);
 options[*i_opt].long_opt=  "rtdb";
 options[*i_opt].short_opt='c';
 options[*i_opt].bin="ypp_rt";
 options[*i_opt].yambo_string="RTDBs";
 options[*i_opt].section="Real-Time";
 options[*i_opt].char_var=1;
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="TD observables plot";
 options[*i_opt].long_opt="rtplot";
 options[*i_opt].short_opt='n';
 options[*i_opt].char_var=1;
 options[*i_opt].bin="ypp_rt";
 options[*i_opt].yambo_string="TDplots"; /* TDplots */
 options[*i_opt].section="Real-Time";
 s_size=sizeof("<string>=(X)response,(a)bsorption,(o)ccupations,(l)ifetimes,(d)ensity,(p)olariazion,(g)reen-function",s_size);
 strlcpy(options[*i_opt].long_desc[0],"<string>=(X)response,(a)bsorption,(o)ccupations,(l)ifetimes,(d)ensity,(p)olariazion,(g)reen-function",s_size);
 s_size=sizeof(" ");
 strlcpy(options[*i_opt].long_desc[1]," ",s_size);
 s_size=sizeof("(X) response calculates the response via the time-resolved polarization",s_size);
 strlcpy(options[*i_opt].long_desc[2],"(X) response calculates the response via the time-resolved polarization",s_size);
 s_size=sizeof("(a) absorption amends the Kubo expression with the time-dependent occupations",s_size);
 strlcpy(options[*i_opt].long_desc[3],"(a) absorption amends the Kubo expression with the time-dependent occupations",s_size);
 s_size=sizeof("(p) polarization evaluates the k-resolved components of the time-dependent polarization",s_size);
 strlcpy(options[*i_opt].long_desc[4],"(p) polarization evaluates the k-resolved components of the time-dependent polarization",s_size);
 s_size=sizeof("(g) evaluates the two-times Green`s function",s_size);
 strlcpy(options[*i_opt].long_desc[5],"(g) evaluates the two-times Green`s function",s_size);
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="TD plot control";
 options[*i_opt].char_var=1;
 options[*i_opt].long_opt= "rtmode";
 options[*i_opt].short_opt='t';
 options[*i_opt].bin="ypp_rt";
 options[*i_opt].yambo_string="TDplotmode"; /* TDpol */
 options[*i_opt].section="Real-Time";
 s_size=sizeof("rtplot=X/a => <string>=(t)ime",s_size);
 strlcpy(options[*i_opt].long_desc[0],"rtplot=X/a => <string>=(t)ime",s_size);
 s_size=sizeof("rtplot=o   => <string>=(b)ands,(t)ime,(e)nergy,(d)os",s_size);
 strlcpy(options[*i_opt].long_desc[1],"rtplot=o   => <string>=(b)ands,(t)ime,(e)nergy,(d)os",s_size);
 s_size=sizeof("rtplot=l   => <string>=(b)ands,(t)ime,(e)nergy",s_size);
 strlcpy(options[*i_opt].long_desc[2],"rtplot=l   => <string>=(b)ands,(t)ime,(e)nergy",s_size);
 s_size=sizeof("rtplot=d   => <string>=(t)ime",s_size);
 strlcpy(options[*i_opt].long_desc[3],"rtplot=d   => <string>=(t)ime",s_size);
 s_size=sizeof("rtplot=p   => <string>=(t)ime",s_size);
 strlcpy(options[*i_opt].long_desc[4],"rtplot=p   => <string>=(t)ime",s_size);
};
