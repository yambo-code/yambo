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
  This functional is provided for historical reasons.
  It was one of the first GGAs that ever appeared.
************************************************************************/

#define XC_GGA_C_LM          137 /* Langreth and Mehl correlation          */

static void 
gga_c_lm_init(void *p_)
{
  XC(gga_type) *p = (XC(gga_type) *)p_;

  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_LDA_C_vBH, p->nspin);
}


static inline void 
func(const XC(gga_type) *p, int order, FLOAT rs, FLOAT zeta, FLOAT xt, FLOAT *xs,
     FLOAT *f, FLOAT *dfdrs, FLOAT *dfdz, FLOAT *dfdxt, FLOAT *dfdxs,
     FLOAT *d2fdrs2, FLOAT *d2fdrsz, FLOAT *d2fdrsxt, FLOAT *d2fdrsxs, FLOAT *d2fdz2, 
     FLOAT *d2fdzxt, FLOAT *d2fdzxs, FLOAT *d2fdxt2, FLOAT *d2fdxtxs, FLOAT *d2fdxs2)
{
  const FLOAT a2 = -0.262;
  const FLOAT a3 = -7.0/(9.0*2.0*M_CBRT2*M_CBRT2);

  FLOAT a1, alpha;
  FLOAT opz, omz, opz13, omz13, DD, dDDdz, d2DDdz2;
  FLOAT aux1, aux2, daux1drs, daux1dxt, d2aux1drs2, d2aux1dxt2, d2aux1drsxt;
  FLOAT t1, t2, dt1drs, dt1dz, dt1dxt, dt2dz, d2t1drs2, d2t1dxt2, d2t1dz2, d2t2dz2, d2t1drsz, d2t1drsxt, d2t1dzxt;

  XC(lda_rs_zeta) pw;

  alpha = POW(4.0*M_PI/3.0, 1.0/6.0);
  a1    = M_PI/(16.0*POW(3*M_PI*M_PI, 4/3)); /* 4.28e-3/2.0, where the 2 comes from the covertion from Ryd. to Hartree */

  pw.order = order;
  pw.rs[0] = SQRT(rs);
  pw.rs[1] = rs;
  pw.rs[2] = rs*rs;
  pw.zeta  = zeta;

  XC(lda_c_hl_func)(p->func_aux[0]->lda, &pw);

  opz   = 1.0 + zeta;
  omz   = 1.0 - zeta;
  opz13 = CBRT(opz);
  omz13 = CBRT(omz);

  DD = SQRT(opz*opz13*opz13 + omz*omz13*omz13)/M_SQRT2;

  aux1 = exp(a2*xt/(alpha*pw.rs[0]));
  aux2 = a1/(alpha*alpha*rs);

  t1   = xt*xt*aux1/DD;
  t2   = a3*(xs[0]*xs[0]*opz*opz13 + xs[1]*xs[1]*omz*omz13);

  *f = pw.zk + aux2*(t1 + t2);

  if(order < 1) return;

  dDDdz    =  5.0/(3.0*4.0*DD)*(opz13*opz13 - omz13*omz13);
  daux1drs = -a2*xt/(2.0*alpha*rs*pw.rs[0])*aux1;
  daux1dxt =  a2/(alpha*pw.rs[0])*aux1;

  dt1drs  =  xt*xt*daux1drs/DD;
  dt1dz   = -xt*xt*aux1*dDDdz/(DD*DD);
  dt1dxt  =  xt*(2.0*aux1 + xt*daux1dxt)/DD;

  dt2dz   = a3*(4.0/3.0)*(xs[0]*xs[0]*opz13 - xs[1]*xs[1]*omz13);

  *dfdrs   = pw.dedrs + aux2*(-(t1 + t2)/rs + dt1drs);
  *dfdz    = pw.dedz + aux2*(dt1dz + dt2dz);
  *dfdxt   = aux2*dt1dxt;
  dfdxs[0] = aux2*(a3*2.0*xs[0]*opz*opz13);
  dfdxs[1] = aux2*(a3*2.0*xs[1]*omz*omz13);

  if(order < 2) return;

  d2DDdz2 = d2t2dz2 = 0.0;
  if(zeta < 1.0){
    d2DDdz2 += 1.0/omz13;
    d2t2dz2 += xs[1]*xs[1]/(omz13*omz13);
  }
  if(zeta > -1.0){
    d2DDdz2 += 1.0/opz13;
    d2t2dz2 += xs[0]*xs[0]/(opz13*opz13);
  }

  d2DDdz2 = -dDDdz*dDDdz/DD + 10.0/(36.0*DD)*d2DDdz2;
  d2t2dz2 = a3*(4.0/9.0)*d2t2dz2;

  d2aux1drs2  = -a2*xt/(2.0*alpha*rs*pw.rs[0])*(-3.0/2.0*aux1/rs + daux1drs);
  d2aux1drsxt = -a2/(2.0*alpha*rs*pw.rs[0])*(aux1 + xt*daux1dxt);
  d2aux1dxt2  =  a2/(alpha*pw.rs[0])*daux1dxt;

  d2t1drs2   =  xt*xt*d2aux1drs2/DD;
  d2t1drsz   = -xt*xt*daux1drs*dDDdz/(DD*DD);
  d2t1drsxt  =  xt*(2.0*daux1drs + xt*d2aux1drsxt)/DD;
  d2t1dz2    =  xt*xt*aux1*(2.0*dDDdz*dDDdz - DD*d2DDdz2)/(DD*DD*DD);
  d2t1dzxt   = -xt*(2.0*aux1 + xt*daux1dxt)*dDDdz/(DD*DD);

  d2t1dxt2   =  (2.0*aux1 + 4.0*xt*daux1dxt + xt*xt*d2aux1dxt2)/DD;

  *d2fdrs2    =  pw.d2edrs2 + aux2*(d2t1drs2 - 2.0*dt1drs/rs + 2.0*(t1 + t2)/pw.rs[2]);
  *d2fdrsz    =  pw.d2edrsz + aux2*(d2t1drsz - (dt1dz + dt2dz)/rs);
  *d2fdrsxt   =  aux2*(d2t1drsxt - dt1dxt/rs);
  d2fdrsxs[0] = -aux2/rs*(a3*2.0*xs[0]*opz*opz13);
  d2fdrsxs[1] = -aux2/rs*(a3*2.0*xs[1]*omz*omz13);
  *d2fdz2     =  pw.d2edz2 + aux2*(d2t1dz2 + d2t2dz2);
  *d2fdzxt    =  aux2*d2t1dzxt;;
  d2fdzxs[0]  =  aux2*(a3*8.0/3.0*xs[0]*opz13);
  d2fdzxs[1]  = -aux2*(a3*8.0/3.0*xs[1]*omz13);
  *d2fdxt2    =  aux2*d2t1dxt2;
  d2fdxtxs[0] =  0.0;
  d2fdxtxs[1] =  0.0;
  d2fdxs2[0]  =  aux2*(a3*2.0*opz*opz13);
  d2fdxs2[1]  =  0.0;
  d2fdxs2[2]  =  aux2*(a3*2.0*omz*omz13);

}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_lm) = {
  XC_GGA_C_LM,
  XC_CORRELATION,
  "Langreth & Mehl",
  XC_FAMILY_GGA,
  "DC Langreth and MJ Mehl, Phys. Rev. Lett. 47, 446 (1981)\n"
  "CD Hu and DC Langreth, Phys. Scr. 32, 391 (1985)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_c_lm_init,
  NULL,
  NULL,            /* this is not an LDA                   */
  work_gga_c,
};

