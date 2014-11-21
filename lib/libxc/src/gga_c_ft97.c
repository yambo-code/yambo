/*
 Copyright (C) 2006-2007 M.A.L. Marques

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published by
 the Free Software Foundation; either version 3 of the License, or
 (at your option) any later version.
  
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU Lesser General Public License for more details.
  
 You should have received a copy of the GNU Lesser General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "util.h"

#define XC_GGA_C_FT97          88 /* Filatov & Thiel correlation */

static const FLOAT
  C0 = 0.01554534543483, /* (1-ln2)/(2*Pi**2), see eq.(9)            */
  C1 = 0.02072712724644, /* 2*C0/3           , see eq.(13),(28),(33) */
  C2 = 0.6203504908994,  /* (3/(4*Pi))**1/3  , see eq.(8)            */
  C3 = 0.2067834969665;  /* C2/3                                     */

static const FLOAT big = 1e4;


void func_kssp0(FLOAT rs, int order, FLOAT *kssp0, FLOAT *dkssp0)
{
  /* calculate kssp0(rs) Eq. (39) */
  static FLOAT k0 = 1.291551074, k1 = 0.349064173, r1 = 0.083275880;
  static const FLOAT KSSPBIG = 1.291551074 - 0.349064173;

  FLOAT aux1, aux2;

  if(rs > big)
    *kssp0 = KSSPBIG;
  else{
    aux1   = POW(rs, 4.0/5.0);
    aux2   = EXP(-r1*aux1);

    *kssp0 = k0 - k1*(1.0 - aux2);
  }  

  if(order < 1) return;

  if(rs > big)
    *dkssp0 = 0.0;
  else
    *dkssp0 = -4.0*r1*k1/5.0 * aux1/rs * aux2;
}


void func_fssp(FLOAT rs, FLOAT gr, int order, FLOAT *fssp, FLOAT *dfsspdrs, FLOAT *dfsspdgr)
{
  /* calculate fssp(rs, gr) Eq. (45) */
  static const FLOAT A1=1.622118767, A2=0.489958076, A3=1.379021941;  

  FLOAT aux1, aux2, aux3;

  aux1 = A2*A2*gr*gr;
  if(aux1 > big)
    *fssp = 0.0;
  else{
    aux2  = EXP(-aux1);
    aux3  = SQRT(1.0 + A3*gr/rs);

    *fssp = (1.0 + A1*gr + aux1)*aux2/aux3;
  }

  if(order < 1) return;

  if(aux1 > big)
    *dfsspdrs = *dfsspdgr = 0.0;
  else{
    *dfsspdrs = (*fssp)*A3*gr/(2.0*rs*(rs + A3*gr));
    *dfsspdgr = aux2/aux3*(-(2.0*A2*A2*gr + A3/(2.0*rs*aux3*aux3))*(1.0 + A1*gr + aux1) + (A1 + 2.0*A2*A2*gr));
  }
}


void func_factor(FLOAT rs, int order, FLOAT *fa, FLOAT *dfa)
{
  /* factor: see Eq. (34) */
  static const FLOAT fa_a1 = 0.939016, fa_a2 = 1.733170;

  FLOAT fa_den, fa_arg, dfa_den, dfa_arg;

  fa_den = fa_a1*SQRT(rs) + fa_a2*rs;
  fa_arg = rs/fa_den;

  *fa = EXP(-fa_arg*fa_arg);

  if(order < 1) return;

  dfa_den = fa_a1/(2.0*SQRT(rs)) + fa_a2;
  dfa_arg = (1.0 - rs*dfa_den/fa_den)/fa_den;
  
  *dfa    = -2.0*fa_arg*dfa_arg*(*fa);
}


void func_kss0(FLOAT rs, int order, FLOAT *kss0, FLOAT *dkss0)
{
  /* calculate kss0(rs) Eq. (40) */
  static const FLOAT k0 = 1.200801774, k1 = 0.859614445, k2 = -0.812904345, r1 = 1.089338848, r2 = 0.655638823;
  static const FLOAT KSS0BIG = 1.200801774 + 0.859614445 - 0.812904345;

  FLOAT aux1, aux2, aux3;

  if(rs > big){
    *kss0 = KSS0BIG;
  }else{
    aux1  = POW(rs, 2.0/5.0);
    aux2  = EXP(-r1*SQRT(rs));
    aux3  = EXP(-r2*aux1);

    *kss0 = k0 + k1*(1.0 - aux2) + k2*(1.0 - aux3);
  }

  if(order < 1) return;

  if(rs > big)
    *dkss0 = 0.0;
  else
    *dkss0 = k1*r1/(2.0*SQRT(rs))*aux2 + 2.0*k2*r2*aux1*aux3/(5.0*rs);
}


void func_fss(FLOAT rs, FLOAT gr, int order, FLOAT *fss, FLOAT *dfssdrs, FLOAT *dfssdgr)
{
  static const FLOAT A4 = 4.946281353, A5 = 3.600612059;

  FLOAT aux1, aux2, aux3;

  aux1 = A4*A4*gr*gr;
  if(aux1 > big)
    *fss = 0.0;
  else{
    aux2 = EXP(-aux1);
    aux3 = SQRT(1.0 + A5*gr/rs);

    *fss = (1.0 + aux1)*aux2/aux3;
  }

  if(order < 1) return;

  if(aux1 > big)
    *dfssdrs = *dfssdgr = 0.0;
  else{
    *dfssdrs = (*fss)*A5*gr/(2.0*rs*(rs + A5*gr));
    *dfssdgr = aux2/aux3*(-(2.0*A4*A4*gr + A5/(2.0*rs*aux3*aux3))*(1.0 + aux1) + 2.0*A4*A4*gr);
  }
}


void func_eab(FLOAT mu, int order, FLOAT *eab, FLOAT *deabdmu)
{
  FLOAT mu12, eei, eei1, ff_n, ff_d, ff, deeidmu, deei1dmu, dffdmu;

  mu12 = SQRT(mu);

  eei  = expint_Ei_scaled(-mu);
  eei1 = mu*eei + 1.0;

  /* calculate approximate normalization, Eq. (15) */
  ff_n = 3.0 + 2.0*(mu12 + mu);
  ff_d = 3.0 + 6.0*(mu12 + mu);
  ff   = ff_n/ff_d;
 
  /* eba : Correlation energy density, Eq. (19) */
  *eab = C0*(eei + 2.0*ff*eei1);    

  if(order < 1) return;

  deeidmu  = eei + 1.0/mu;
  deei1dmu = eei*(1.0 + mu) + 1.0;

  dffdmu   = (1.0 + 2.0*mu12)/(2.0*mu12*ff_d) * (2.0 - 6.0*ff_n/ff_d);

  *deabdmu = C0*(deeidmu + 2.0*(dffdmu*eei1 + ff*deei1dmu));
}


static void 
func(const XC(func_type) *p, XC(gga_work_c_t) *r)
{
  /* numerical curoff for mu_aa, mu_ab, mu_ba, mu_bb, see Eqs.(13) and (33) */
  static const FLOAT CUTOFF  = 1.0e7;
  static const FLOAT sign[2] = {1.0, -1.0};

  int ispin;
  FLOAT rsa, ga2, opz, opz13;
  FLOAT kssp0, fssp, den_ba, mu_ba, e_ba; 
  FLOAT kss0, fss, den_aa, mu_aa, e_aa, fa;

  FLOAT drsadrs, drsadzeta, dga2dxs;
  FLOAT dkssp0, dfsspdrsa, dfsspdga2, dden_badrsa, dden_badga2, dmu_badrsa, dmu_badga2, de_badmu_ba;
  FLOAT dkss0,  dfssdrsa,  dfssdga2,  dden_aadrsa, dden_aadga2, dmu_aadrsa, dmu_aadga2, de_aadmu_aa, dfa;

  r->f = 0.0;
  if(r->order >= 1) r->dfdrs = r->dfdz = r->dfdxs[0] = r->dfdxs[1] = 0.0;

  for(ispin=0; ispin<2; ispin++){
    opz   = 1.0 + sign[ispin]*r->zeta;
    opz13 = CBRT(opz);

    /* rs_alpha */
    rsa = r->rs*M_CBRT2/opz13;

    /* | grad rs_alpha |^2 */
    ga2 = C3*C3 * r->xs[ispin]*r->xs[ispin];

    /* mu(beta,alpha), Eq.(13) */

    /* calculate kssp0(rsa) Eq. (39) */
    func_kssp0(rsa, r->order, &kssp0, &dkssp0);

    /* calculate fssp(rsa, ga2) Eq. (45) */
    func_fssp(rsa, ga2, r->order, &fssp, &dfsspdrsa, &dfsspdga2);

    /* calculate denominator */
    den_ba = kssp0*kssp0*fssp*fssp;

    if(C1*rsa <= den_ba*CUTOFF){
      mu_ba = C1*rsa/den_ba;

      func_eab(mu_ba, r->order, &e_ba, &de_badmu_ba);

      r->f += e_ba*(1.0 - sign[ispin]*r->zeta)/2.0;
    }

    /* mu(alpha,alpha), Eq. (33) */

    /* factor: see Eq. (34) */
    func_factor(rsa, r->order, &fa, &dfa);

    /* calculate kss0(rsa) Eq. (40) */
    func_kss0(rsa, r->order, &kss0, &dkss0);

    /* calculate fss(rsa, ga2) Eq. (45) */
    func_fss(rsa, ga2, r->order, &fss, &dfssdrsa, &dfssdga2);

    /* calculate denominator */
    den_aa = kss0*kss0*fss*fss;

    if(C1*rsa <= den_aa*CUTOFF){
      mu_aa = C1*rsa/den_aa;

      func_eab(mu_aa, r->order, &e_aa, &de_aadmu_aa);

      r->f += e_aa*fa*(1.0 + sign[ispin]*r->zeta)/2.0;
    }
      
    if(r->order < 1) continue;

    drsadrs   = M_CBRT2/opz13;
    drsadzeta = -r->rs*M_CBRT2*sign[ispin]/(3.0*opz*opz13);

    dga2dxs  = 2.0*C3*C3 * r->xs[ispin];
    
    /* mu(beta,alpha), Eq.(13) */

    /* calculate denominator */
    dden_badrsa = 2.0*kssp0*fssp*(fssp*dkssp0 + dfsspdrsa*kssp0);
    dden_badga2 = 2.0*kssp0*kssp0*fssp*dfsspdga2;
    
    if(C1*rsa <= den_ba*CUTOFF){
      dmu_badrsa  = C1/den_ba *(1.0 - rsa*dden_badrsa/den_ba);
      dmu_badga2  = -mu_ba*dden_badga2/den_ba;

      r->dfdrs    += de_badmu_ba*dmu_badrsa*drsadrs*(1.0 - sign[ispin]*r->zeta)/2.0;
      r->dfdz     += (de_badmu_ba*dmu_badrsa*drsadzeta*(1.0 - sign[ispin]*r->zeta) - e_ba*sign[ispin])/2.0;

      r->dfdxs[ispin] += de_badmu_ba*dmu_badga2*dga2dxs*(1.0 - sign[ispin]*r->zeta)/2.0;
    }
    
    /* mu(alpha,alpha), Eq. (33) */

    /* calculate denominator */
    dden_aadrsa = 2.0*kss0*fss*(fss*dkss0 + dfssdrsa*kss0);
    dden_aadga2 = 2.0*kss0*kss0*fss*dfssdga2;
    
    if(C1*rsa <= den_aa*CUTOFF){
      dmu_aadrsa  = C1/den_aa *(1.0 - rsa*dden_aadrsa/den_aa);
      dmu_aadga2  = -mu_aa*dden_aadga2/den_aa;

      r->dfdrs   += (de_aadmu_aa*dmu_aadrsa*fa + e_aa*dfa)*  drsadrs*(1.0 + sign[ispin]*r->zeta)/2.0;
      r->dfdz    += (de_aadmu_aa*dmu_aadrsa*fa + e_aa*dfa)*drsadzeta*(1.0 + sign[ispin]*r->zeta)/2.0 + e_aa*fa*sign[ispin]/2.0;

      r->dfdxs[ispin] += de_aadmu_aa*dmu_aadga2*dga2dxs*fa*(1.0 + sign[ispin]*r->zeta)/2.0;
    }

  } /* for ispin */
}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_ft97) = {
  XC_GGA_C_FT97,
  XC_CORRELATION,
  "Filatov & Thiel correlation",
  XC_FAMILY_GGA,
  "M Filatov & W Thiel, Int. J. Quant. Chem. 62, 603-616 (1997)\n"
  "M Filatov & W Thiel, Mol Phys 91, 847 (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 0.0, 1e-32,
  NULL,
  NULL,
  NULL,
  work_gga_c
};
