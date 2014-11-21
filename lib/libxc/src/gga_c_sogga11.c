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

#define XC_GGA_C_SOGGA11       152 /* Second-order generalized gradient approximation 2011 */
#define XC_GGA_C_SOGGA11_X     159 /* To be used with hyb_gga_x_SOGGA11-X  */

static void 
gga_c_sogga11_init(XC(func_type) *p)
{
  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_LDA_C_PW_MOD, p->nspin);

  switch(p->info->number){
  case XC_GGA_C_SOGGA11:   p->func = 0; break;
  case XC_GGA_C_SOGGA11_X: p->func = 1; break;
  default:
    fprintf(stderr, "Internal error in gga_c_sogga11\n");
    exit(1);
  } 
}


static inline void 
func(const XC(func_type) *p, XC(gga_work_c_t) *r)
{
  static FLOAT beta = 15.75592*0.004235; /* the usual value of 0.066726 */
  const FLOAT aa[][6] = {
    {0.50000, -4.62334,  8.00410, -130.226,  38.2685,   69.5599},
    {0.50000, 78.2439,  25.7211,   -13.8830, -9.87375, -14.1357}
  };
  const FLOAT bb[][6] = {
    {0.50000,   3.62334, 9.36393, 34.5114, -18.5684,   -0.16519},
    {0.50000, -79.2439, 16.3725,   2.08129,  7.50769, -10.1861}
  };

  FLOAT phi, dphidz, d2phidz2;
  FLOAT y, dydrs, dydxt, dydz, d2ydrs2, d2ydrsxt, d2ydrsz, d2ydxt2, d2ydxtz, d2ydz2;
  FLOAT pyprs, pypzk, pypxt, pypphi;
  FLOAT p2yprs2, p2yprszk, p2yprsxt, p2yprsphi, p2ypzk2, p2ypzkxt, p2ypzkphi, p2ypxt2, p2ypxtphi;
  FLOAT dfdy, d2fdy2;
  FLOAT den0, den1, t0, dt0, d2t0, t1, dt1, d2t1, f0, df0, d2f0, f1, df1, d2f1;

  XC(lda_work_t) pw;
  FLOAT alpha, auxp, auxm;

  pw.order = r->order;
  pw.rs[0] = SQRT(r->rs);
  pw.rs[1] = r->rs;
  pw.rs[2] = r->rs*r->rs;
  pw.zeta  = r->zeta;

  XC(lda_c_pw_func)(p->func_aux[0], &pw);

  alpha = beta/(16.0*M_CBRT2*M_CBRT2);

  auxp = CBRT(1.0 + r->zeta);
  auxm = CBRT(1.0 - r->zeta);

  phi  = 0.5*(auxp*auxp + auxm*auxm);
  y    = -alpha*phi*r->xt*r->xt/(r->rs*pw.zk);

  den0 = -1.0/(1.0 + y);
  f0   = 1.0 + den0;
  den1 = -exp(-y);
  f1   = 1.0 + den1;

  t0  = aa[p->func][0] + f0*(aa[p->func][1] + f0*(aa[p->func][2] + f0*(aa[p->func][3] + f0*(aa[p->func][4] + f0*aa[p->func][5]))));
  t1  = bb[p->func][0] + f1*(bb[p->func][1] + f1*(bb[p->func][2] + f1*(bb[p->func][3] + f1*(bb[p->func][4] + f1*bb[p->func][5]))));
  r->f = pw.zk*(t0 + t1);

  if(r->order < 1) return;

  dphidz = 0.0;
  if(auxp > p->info->min_zeta) dphidz += 1/auxp;
  if(auxm > p->info->min_zeta) dphidz -= 1/auxm;
  dphidz *= 1.0/3.0;

  /* partial derivatives */
  pyprs  = -y/r->rs;
  pypzk  = -y/pw.zk;
  pypxt  = -2.0*alpha*phi*r->xt/(r->rs*pw.zk);
  pypphi =  y/phi;
  
  /* full derivatives */
  dydrs = pyprs + pypzk*pw.dedrs;
  dydxt = pypxt;
  dydz  = pypphi*dphidz + pypzk*pw.dedz;

  df0 =  den0*den0;
  df1 = -den1;

  dt0 = aa[p->func][1] + f0*(2.0*aa[p->func][2] + f0*(3.0*aa[p->func][3] + f0*(4.0*aa[p->func][4] + f0*5.0*aa[p->func][5])));
  dt1 = bb[p->func][1] + f1*(2.0*bb[p->func][2] + f1*(3.0*bb[p->func][3] + f1*(4.0*bb[p->func][4] + f1*5.0*bb[p->func][5])));

  dfdy = dt0*df0 + dt1*df1;

  r->dfdrs    = pw.dedrs*(t0 + t1) + pw.zk*dfdy*dydrs;
  r->dfdz     = pw.dedz *(t0 + t1) + pw.zk*dfdy*dydz;
  r->dfdxt    = pw.zk*dfdy*dydxt;
  r->dfdxs[0] = 0.0;
  r->dfdxs[1] = 0.0;

  if(r->order < 2) return;
  
  d2phidz2 = 0.0;
  if(auxp > p->info->min_zeta) d2phidz2 += 1.0/((1.0 + r->zeta)*auxp);
  if(auxm > p->info->min_zeta) d2phidz2 += 1.0/((1.0 - r->zeta)*auxm);
  d2phidz2 *= -1.0/9.0;

  p2yprs2   = -2.0*pyprs/r->rs;
  p2yprszk  = -pypzk/r->rs;
  p2yprsxt  = -pypxt/r->rs;
  p2yprsphi = -pypphi/r->rs;
  p2ypzk2   = -2.0*pypzk/pw.zk;
  p2ypzkxt  = -pypxt/pw.zk;
  p2ypzkphi = -pypphi/pw.zk;
  p2ypxt2   = -2.0*alpha*phi/(r->rs*pw.zk);
  p2ypxtphi =  pypxt/phi;

  d2ydrs2   = p2yprs2 + 2.0*p2yprszk*pw.dedrs + pypzk*pw.d2edrs2 + p2ypzk2*pw.dedrs*pw.dedrs;
  d2ydrsxt  = p2yprsxt + p2ypzkxt*pw.dedrs;
  d2ydrsz   = pypzk*pw.d2edrsz + dphidz*(p2yprsphi + p2ypzkphi*pw.dedrs) + pw.dedz*(p2yprszk + p2ypzk2*pw.dedrs);
  d2ydxt2   = p2ypxt2;
  d2ydxtz   = p2ypxtphi*dphidz + p2ypzkxt*pw.dedz;
  d2ydz2    = pypphi*d2phidz2 + pypzk*pw.d2edz2 + 2.0*p2ypzkphi*dphidz*pw.dedz + p2ypzk2*pw.dedz*pw.dedz;
  
  d2f0 = 2.0*den0*df0;
  d2f1 = -df1;

  d2t0 = 2.0*aa[p->func][2] + f0*(6.0*aa[p->func][3] + f0*(12.0*aa[p->func][4] + f0*20.0*aa[p->func][5]));
  d2t1 = 2.0*bb[p->func][2] + f1*(6.0*bb[p->func][3] + f1*(12.0*bb[p->func][4] + f1*20.0*bb[p->func][5]));

  d2fdy2 = d2t0*df0*df0 + dt0*d2f0 + d2t1*df1*df1 + dt1*d2f1;

  r->d2fdrs2     = pw.d2edrs2*(t0 + t1) + 2.0*pw.dedrs*dfdy*dydrs + pw.zk*(d2fdy2*dydrs*dydrs + dfdy*d2ydrs2);
  r->d2fdrsz     = pw.d2edrsz*(t0 + t1) + dfdy*(pw.dedrs*dydz + pw.dedz*dydrs)
    + pw.zk*(d2fdy2*dydrs*dydz + dfdy*d2ydrsz);
  r->d2fdrsxt    = pw.dedrs*dfdy*dydxt + pw.zk*(d2fdy2*dydrs*dydxt + dfdy*d2ydrsxt);
  r->d2fdrsxs[0] = 0.0;
  r->d2fdrsxs[1] = 0.0;
  r->d2fdz2      = pw.d2edz2*(t0 + t1) + 2.0*pw.dedz*dfdy*dydz + pw.zk*(d2fdy2*dydz*dydz + dfdy*d2ydz2);
  r->d2fdzxt     = pw.dedz*dfdy*dydxt + pw.zk*(d2fdy2*dydz*dydxt + dfdy*d2ydxtz);
  r->d2fdzxs[0]  = 0.0;
  r->d2fdzxs[1]  = 0.0;
  r->d2fdxt2     = pw.zk*(d2fdy2*dydxt*dydxt + dfdy*d2ydxt2);
  r->d2fdxtxs[0] = 0.0;
  r->d2fdxtxs[1] = 0.0;
  r->d2fdxs2[0]  = 0.0;
  r->d2fdxs2[1]  = 0.0;
  r->d2fdxs2[2]  = 0.0;
}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_sogga11) = {
  XC_GGA_C_SOGGA11,
  XC_CORRELATION,
  "Second-order generalized gradient approximation 2011",
  XC_FAMILY_GGA,
  "R Peverati, Y Zhao, and DG Truhlar, J. Phys. Chem. Lett. 2, 1911-1997 (2011)\n"
  "http://comp.chem.umn.edu/mfm/index.html",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-27, 1e-32, 0.0, 1e-32,
  gga_c_sogga11_init,
  NULL, NULL,
  work_gga_c,
};

const XC(func_info_type) XC(func_info_gga_c_sogga11_x) = {
  XC_GGA_C_SOGGA11_X,
  XC_CORRELATION,
  "To be used with hyb_gga_x_SOGGA11-X",
  XC_FAMILY_GGA,
  "R Peverati and DG Truhlar, J. Chem. Phys. 135, 191102 (2011)\n"
  "http://comp.chem.umn.edu/mfm/index.html",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-26, 1e-32, 0.0, 1e-32,
  gga_c_sogga11_init, 
  NULL, NULL,
  work_gga_c
};
