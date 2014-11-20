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

#define XC_GGA_C_TCA          100 /* Tognetti, Cortona, Adamo */
#define XC_GGA_C_REVTCA        99 /* Tognetti, Cortona, Adamo (revised) */

static void 
gga_c_tca_init(XC(func_type) *p)
{
  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_LDA_C_RC04, p->nspin);
}


static inline void 
func(const XC(func_type) *p, XC(gga_work_c_t) *r)
{
  const FLOAT sigma=1.43, alpha=2.30;
  const FLOAT xconv = X2S*M_CBRT2;

  FLOAT ss, ssalpha, aux, Bs, dBs, d2Bs;
  FLOAT zeta2, zeta4, aa, arg, dargdrs, dargdxt, sinc, dsinc, DD, dDDdz, dDDdrs, dDDdxt;
  FLOAT d2sinc, d2argdrs2, d2argdrsxt, d2DDdrs2, d2DDdrsz, d2DDdrsxt, d2DDdz2, d2DDdzxt, d2DDdxt2;

  XC(lda_work_t) pw;

  pw.order = r->order;
  pw.rs[0] = SQRT(r->rs);
  pw.rs[1] = r->rs;
  pw.rs[2] = r->rs*r->rs;
  pw.zeta  = r->zeta;

  XC(lda_c_rc04_func)(p->func_aux[0], &pw);

  ss      = xconv*r->xt;

  ssalpha = POW(ss, alpha);
  aux     = 1.0 + sigma*ssalpha;
  Bs      = 1.0/aux;

  if(p->info->number == XC_GGA_C_REVTCA){
    zeta2 = r->zeta*r->zeta;
    zeta4 = zeta2*zeta2;
    aa    = M_PI*CBRT(9.0*M_PI/4.0);
    arg   = aa*ss/r->rs;
    sinc  = (arg == 0.0) ? 1.0 : sin(arg)/arg;

    DD = 1.0 - zeta4*(1.0 - sinc*sinc);
  }else{
    DD = 1.0;
  }

  r->f = pw.zk*Bs*DD;

  if(r->order < 1) return;

  dBs = -sigma*alpha*ssalpha * xconv/(ss*aux*aux);

  if(p->info->number == XC_GGA_C_REVTCA){
    dsinc  = (arg == 0.0) ? 0.0 : cos(arg)/arg - sin(arg)/(arg*arg);

    dargdrs = -arg/r->rs;
    dargdxt = aa*xconv/r->rs;

    dDDdrs =  2.0*zeta4*sinc*dsinc*dargdrs;
    dDDdxt =  2.0*zeta4*sinc*dsinc*dargdxt;
    dDDdz  = -4.0*r->zeta*zeta2*(1.0 - sinc*sinc);
  }else{
    dDDdrs = 0.0;
    dDDdz  = 0.0;
    dDDdxt = 0.0;
  }  

  r->dfdrs    = Bs*(pw.dedrs*DD + pw.zk*dDDdrs);
  r->dfdz     = Bs*(pw.dedz *DD + pw.zk*dDDdz);
  r->dfdxt    = pw.zk*(dBs*DD + Bs*dDDdxt);
  r->dfdxs[0] = 0.0;
  r->dfdxs[1] = 0.0;

  if(r->order < 2) return;

  d2Bs = -dBs/(ss*aux) * xconv * (1.0 - alpha + sigma*(1.0 + alpha)*ssalpha);

  if(p->info->number == XC_GGA_C_REVTCA){
    d2sinc  = (arg == 0.0) ? -1.0/3.0 : -(2.0*arg*cos(arg) + (arg*arg - 2.0)*sin(arg))/(arg*arg*arg);

    d2argdrs2  = -2.0*dargdrs/r->rs;
    d2argdrsxt = -dargdxt/r->rs;

    d2DDdrs2  =   2.0*zeta4*((dsinc*dsinc + sinc*d2sinc)*dargdrs*dargdrs + sinc*dsinc*d2argdrs2);
    d2DDdrsz  =   8.0*r->zeta*zeta2*sinc*dsinc*dargdrs;
    d2DDdrsxt =   2.0*zeta4*((dsinc*dsinc + sinc*d2sinc)*dargdrs*dargdxt + sinc*dsinc*d2argdrsxt);
    d2DDdz2   = -12.0*zeta2*(1.0 - sinc*sinc);
    d2DDdzxt  =   8.0*r->zeta*zeta2*sinc*dsinc*dargdxt;
    d2DDdxt2  =   2.0*zeta4*((dsinc*dsinc + sinc*d2sinc)*dargdxt*dargdxt);
  }else{
    d2DDdrs2  = 0.0;
    d2DDdrsz  = 0.0;
    d2DDdrsxt = 0.0;
    d2DDdz2   = 0.0;
    d2DDdzxt  = 0.0;
    d2DDdxt2  = 0.0;
  }

  r->d2fdrs2     = Bs*(pw.d2edrs2*DD + 2.0*pw.dedrs*dDDdrs + pw.zk*d2DDdrs2);
  r->d2fdrsz     = Bs*(pw.d2edrsz*DD + pw.dedrs*dDDdz + pw.dedz*dDDdrs + pw.zk*d2DDdrsz);
  r->d2fdrsxt    = pw.dedrs*(dBs*DD + Bs*dDDdxt) + pw.zk*(dBs*dDDdrs + Bs*d2DDdrsxt);
  r->d2fdrsxs[0] = 0.0;
  r->d2fdrsxs[1] = 0.0;
  r->d2fdz2      = Bs*(pw.d2edz2*DD + 2.0*pw.dedz*dDDdz + pw.zk*d2DDdz2);
  r->d2fdzxt     = pw.dedz*(dBs*DD + Bs*dDDdxt) + pw.zk*(dBs*dDDdz + Bs*d2DDdzxt);
  r->d2fdzxs[0]  = 0.0;
  r->d2fdzxs[1]  = 0.0;
  r->d2fdxt2     = pw.zk*(d2Bs*DD + 2.0*dBs*dDDdxt + Bs*d2DDdxt2);
  r->d2fdxtxs[0] = 0.0;
  r->d2fdxtxs[1] = 0.0;
  r->d2fdxs2[0]  = 0.0;
  r->d2fdxs2[1]  = 0.0;
  r->d2fdxs2[2]  = 0.0;
}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_tca) = {
  XC_GGA_C_TCA,
  XC_CORRELATION,
  "Tognetti, Cortona, Adamo",
  XC_FAMILY_GGA,
  "V Tognetti, P Cortona, and C Adamo, J. Chem. Phys. 128, 034101 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_c_tca_init,
  NULL,
  NULL,
  work_gga_c
};


const XC(func_info_type) XC(func_info_gga_c_revtca) = {
  XC_GGA_C_REVTCA,
  XC_CORRELATION,
  "Tognetti, Cortona, Adamo (revised)",
  XC_FAMILY_GGA,
  "V Tognetti, P Cortona, and C Adamo, Chem. Phys. Lett. 460, 536-539 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_c_tca_init,
  NULL,
  NULL,
  work_gga_c
};
