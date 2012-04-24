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

#define XC_GGA_C_AM05          135 /* Armiento & Mattsson 05 correlation             */

static void
gga_c_am05_init(void *p_)
{
  XC(gga_type) *p = (XC(gga_type) *)p_;

  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_LDA_C_PW_MOD, p->nspin);
}


static inline void 
func(const XC(gga_type) *p, int order, FLOAT rs, FLOAT zeta, FLOAT xt, FLOAT *xs,
     FLOAT *f, FLOAT *dfdrs, FLOAT *dfdz, FLOAT *dfdxt, FLOAT *dfdxs,
     FLOAT *d2fdrs2, FLOAT *d2fdrsz, FLOAT *d2fdrsxt, FLOAT *d2fdrsxs, FLOAT *d2fdz2, 
     FLOAT *d2fdzxt, FLOAT *d2fdzxs, FLOAT *d2fdxt2, FLOAT *d2fdxtxs, FLOAT *d2fdxs2)
{
  const FLOAT am05_alpha = 2.804;
  const FLOAT am05_gamma = 0.8098;

  XC(lda_rs_zeta) pw;
  FLOAT sfact;
  int is;

  pw.order = order;
  pw.rs[0] = SQRT(rs);
  pw.rs[1] = rs;
  pw.rs[2] = rs*rs;
  pw.zeta  = zeta;

  XC(lda_c_pw_func)(p->func_aux[0]->lda, &pw);

  sfact = (p->nspin == XC_POLARIZED) ? 1.0 : 2.0;

  *f = 0.0;
  if(order > 0)
    *dfdrs = *dfdz = *dfdxt = 0.0;
  if(order > 1){
    *d2fdrs2 = *d2fdrsz = *d2fdrsxt = *d2fdz2 = *d2fdzxt = *d2fdxt2 = 0.0;
    d2fdxs2[1] = 0.0;
  }
  
  for(is=0; is<p->nspin; is++){
    FLOAT ss, ff, dff, d2ff, XX, dXX, d2XX;
    FLOAT sign[2] = {1.0, -1.0};
    int js;

    ss = X2S*xs[is];
    XX = 1.0/(1.0 + am05_alpha*ss*ss);
    ff = XX + (1.0 - XX)*am05_gamma;

    *f += sfact*pw.zk*(1.0 + sign[is]*zeta)*ff/2.0;

    if(order < 1) continue;

    dXX = -2.0*am05_alpha*ss * XX*XX*X2S;
    dff = dXX*(1.0 - am05_gamma);

    *dfdrs   += sfact*pw.dedrs*(1.0 + sign[is]*zeta)*ff/2.0;
    dfdxs[is] = pw.zk*(1.0 + sign[is]*zeta)*dff/2.0;
    if(p->nspin == XC_POLARIZED)
      *dfdz += (pw.dedz*(1.0 + sign[is]*zeta) + pw.zk*sign[is])*ff/2.0;

    if(order < 2) continue;

    js = (is == 0) ? 0 : 2;
    
    d2XX = 2.0*am05_alpha*(3.0*am05_alpha*ss*ss - 1.0)*(XX*XX*XX)*(X2S*X2S);
    d2ff = d2XX*(1.0 - am05_gamma);

    *d2fdrs2    += sfact*pw.d2edrs2*(1.0 + sign[is]*zeta)*ff/2.0;
    d2fdrsxs[is] = pw.dedrs*(1.0 + sign[is]*zeta)*dff/2.0;
    d2fdxtxs[is] = 0.0;
    d2fdxs2[js]  = pw.zk*(1.0 + sign[is]*zeta)*d2ff/2.0;

    if(p->nspin == XC_POLARIZED){
      *d2fdrsz   += (pw.d2edrsz*(1.0 + sign[is]*zeta) +     pw.dedrs*sign[is])*ff/2.0;
      *d2fdz2    += (pw.d2edz2 *(1.0 + sign[is]*zeta) + 2.0*pw.dedz *sign[is])*ff/2.0;
      d2fdzxs[is] = (pw.dedz   *(1.0 + sign[is]*zeta) +     pw.zk   *sign[is])*dff/2.0;
    }else
      d2fdzxs[is] = 0.0;
    
  }
}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_am05) = {
  XC_GGA_C_AM05,
  XC_CORRELATION,
  "Armiento & Mattsson 05",
  XC_FAMILY_GGA,
  "R Armiento and AE Mattsson, Phys. Rev. B 72, 085108 (2005)\n"
  "AE Mattsson, R Armiento, J Paier, G Kresse, JM Wills, and TR Mattsson, J. Chem. Phys. 128, 084714 (2008).",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_c_am05_init,
  NULL,
  NULL,            /* this is not an LDA                   */
  work_gga_c,
};
