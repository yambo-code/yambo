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

/************************************************************************
 Implements Perdew 86 Generalized Gradient Approximation
 correlation functional.
************************************************************************/

#define XC_GGA_C_P86          132 /* Perdew 86 */

static void 
gga_c_p86_init(XC(func_type) *p)
{
  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_LDA_C_PZ, p->nspin);
}


static inline void 
func(const XC(func_type) *p, XC(gga_work_c_t) *r)
{
  static const FLOAT alpha = 0.023266, beta = 7.389e-6, gamma = 8.723, delta = 0.472;
  static const FLOAT aa = 0.001667, bb = 0.002568;
  static const FLOAT ftilde = 1.745*0.11;

  FLOAT rsconv, x1, dx1drs, dx1dxt, d2x1drs2, d2x1drsxt;
  FLOAT f1, f2, H, df1, df2, dHdx1, dHdrs, d2f1, d2f2, d2Hdrs2, d2Hdx12, d2Hdrsx1;
  FLOAT DD, dDDdzeta, d2DDdzeta2, CC, CCinf, dCCdrs, d2CCdrs2;
  FLOAT Phi, dPhidx1, dPhidrs, d2Phidrs2, d2Phidrsx1;

  XC(lda_work_t) pw;

  rsconv = POW(4.0*M_PI/3.0, 1.0/6.0);

  pw.order = r->order;
  pw.rs[0] = SQRT(r->rs);
  pw.rs[1] = r->rs;
  pw.rs[2] = r->rs*r->rs;
  pw.zeta  = r->zeta;

  XC(lda_c_pz_func)(p->func_aux[0], &pw);

  /* Equation [1].(4) */ 
  DD = SQRT(POW(1.0 + r->zeta, 5.0/3.0) + POW(1.0 - r->zeta, 5.0/3.0))/M_SQRT2;
  
  /* Equation [1].(6) */
  f1    = bb + alpha*r->rs + beta*pw.rs[2];
  f2    = 1.0 + gamma*r->rs + delta*pw.rs[2] + 1.0e4*beta*r->rs*pw.rs[2];
  CC    = aa + f1/f2;
  CCinf = aa + bb;

  /* Equation [1].(9) */
  x1  = r->xt/(rsconv*pw.rs[0]);
  Phi  = ftilde*(CCinf/CC)*x1;

  /* Equation [1].(8) */
  H = x1*x1*exp(-Phi)*CC/DD;
  r->f = pw.zk + H;

  if(r->order < 1) return;

  dDDdzeta = 5.0/(3.0*4.0*DD)*(POW(1.0 + r->zeta, 2.0/3.0) - POW(1.0 - r->zeta, 2.0/3.0));

  df1    = alpha + 2.0*beta*r->rs;
  df2    = gamma + 2.0*delta*r->rs + 3.0e4*beta*pw.rs[2];
  dCCdrs = (df1*f2 - f1*df2)/(f2*f2);

  dx1drs = -r->xt/(2.0*rsconv*r->rs*pw.rs[0]);
  dx1dxt = 1.0/(rsconv*pw.rs[0]);

  dPhidx1 =  ftilde*(CCinf/CC);
  dPhidrs = -dCCdrs*Phi/CC;

  dHdx1   =  x1*exp(-Phi)*CC/DD*(2.0 - x1*dPhidx1);
  dHdrs   =  x1*x1*exp(-Phi)/DD*(dCCdrs - dPhidrs*CC);

  r->dfdrs    = pw.dedrs + dHdrs + dHdx1*dx1drs;
  r->dfdz     = pw.dedz - H*dDDdzeta/DD;
  r->dfdxt    = dHdx1*dx1dxt;
  r->dfdxs[0] = 0.0;
  r->dfdxs[1] = 0.0;

  if(r->order < 2) return;

  d2DDdzeta2 = 0.0;
  if(r->zeta < 1.0)
    d2DDdzeta2 += POW(1.0 - r->zeta, -1.0/3.0);
  if(r->zeta > -1.0)
    d2DDdzeta2 += POW(1.0 + r->zeta, -1.0/3.0);

  d2DDdzeta2 = -dDDdzeta*dDDdzeta/DD + 10.0/(36.0*DD)*d2DDdzeta2;

  d2f1      = 2.0*beta;
  d2f2      = 2.0*delta + 6.0e4*beta*r->rs;
  d2CCdrs2  = (f2*(d2f1*f2 - f1*d2f2) - 2.0*df2*(df1*f2 - f1*df2))/(f2*f2*f2);
  
  d2Phidrs2  = -(d2CCdrs2*Phi + dCCdrs*dPhidrs - dCCdrs*dCCdrs*Phi/CC)/CC;
  d2Phidrsx1 = -dCCdrs*dPhidx1/CC; 

  d2x1drs2  = 3.0*r->xt/(4.0*rsconv*pw.rs[2]*pw.rs[0]);
  d2x1drsxt = -1.0/(2.0*rsconv*r->rs*pw.rs[0]);
  
  d2Hdx12   = exp(-Phi)*CC/DD*(2.0 + x1*dPhidx1*(x1*dPhidx1 - 4.0));
  d2Hdrs2   = x1*x1*exp(-Phi)/DD*(d2CCdrs2 - d2Phidrs2*CC - dPhidrs*(2.0*dCCdrs - dPhidrs*CC));
  d2Hdrsx1  =    x1*exp(-Phi)/DD*((dCCdrs - CC*dPhidrs)*(2.0 - x1*dPhidx1) - CC*x1*d2Phidrsx1);

  r->d2fdrs2     = pw.d2edrs2 + d2Hdrs2 + 2.0*d2Hdrsx1*dx1drs + d2Hdx12*dx1drs*dx1drs + dHdx1*d2x1drs2;
  r->d2fdrsz     = pw.d2edrsz - (dHdrs + dHdx1*dx1drs)*dDDdzeta/DD;
  r->d2fdrsxt    = d2Hdrsx1*dx1dxt + d2Hdx12*dx1drs*dx1dxt + dHdx1*d2x1drsxt;
  r->d2fdrsxs[0] = 0.0;
  r->d2fdrsxs[1] = 0.0;
  r->d2fdz2      = pw.d2edz2 - H*(d2DDdzeta2*DD - 2.0*dDDdzeta*dDDdzeta)/(DD*DD);
  r->d2fdzxt     = -dHdx1*dx1dxt*dDDdzeta/DD;
  r->d2fdzxs[0]  = 0.0;
  r->d2fdzxs[1]  = 0.0;
  r->d2fdxt2     = d2Hdx12*dx1dxt*dx1dxt;
  r->d2fdxtxs[0] = 0.0;
  r->d2fdxtxs[1] = 0.0;
  r->d2fdxs2[0]  = 0.0;
  r->d2fdxs2[1]  = 0.0;
  r->d2fdxs2[2]  = 0.0;
}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_p86) = {
  XC_GGA_C_P86,
  XC_CORRELATION,
  "Perdew 86",
  XC_FAMILY_GGA,
  "JP Perdew, Phys. Rev. B 33, 8822 (1986)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_c_p86_init,
  NULL,
  NULL,
  work_gga_c
};
