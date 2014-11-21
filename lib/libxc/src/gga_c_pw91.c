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
#include <math.h>
#include "util.h"

#define XC_GGA_C_PW91 134 /* Perdew & Wang 91 */

static FLOAT pw91_nu, pw91_beta;
static const FLOAT
  pw91_C_c0  = 4.235e-3, 
  pw91_alpha = 0.09;

static void 
gga_c_pw91_init(XC(func_type) *p)
{
  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_LDA_C_PW, p->nspin);

  pw91_nu   = 16.0/M_PI * CBRT(3.0*M_PI*M_PI);
  pw91_beta = pw91_nu*pw91_C_c0;
}


static void
A_eq14(int order, FLOAT ec, FLOAT g, FLOAT *A, 
       FLOAT *dAec, FLOAT *dAg,
       FLOAT *d2Aec2, FLOAT *d2Ag2, FLOAT *d2Aecg)
{
  FLOAT xx, expxx, g2, g3;
  FLOAT dAdxx, dxxdec, dxxdg;
  FLOAT d2Adxx2, d2xxdecg, d2xxdg2;

  g2 = g*g;
  g3 = g*g2;

  xx    = -2.0*pw91_alpha*ec/(g3*pw91_beta*pw91_beta);
  expxx = exp(xx);

  *A = (2.0*pw91_alpha/pw91_beta)/(expxx - 1.0);

  if(order < 1) return;

  dAdxx  = -(*A)*expxx/(expxx - 1.0);
  dxxdec = xx/ec;
  dxxdg  = -3.0*xx/g;

  *dAec = dAdxx*dxxdec;
  *dAg  = dAdxx*dxxdg;

  if(order < 2) return;

  d2Adxx2  = -dAdxx*(expxx + 1.0) / (expxx - 1.0);
  d2xxdecg =  dxxdg/ec;
  d2xxdg2  = -4.0*dxxdg/g;

  *d2Aec2 = d2Adxx2*dxxdec*dxxdec;
  *d2Aecg = d2Adxx2*dxxdec*dxxdg + dAdxx*d2xxdecg;
  *d2Ag2  = d2Adxx2*dxxdg *dxxdg + dAdxx*d2xxdg2;
}


static void
H0_eq13(int order, FLOAT ec, FLOAT g, FLOAT t, FLOAT *H0,
	FLOAT *dH0dec, FLOAT *dH0dg, FLOAT *dH0dt,
	FLOAT *d2H0dec2, FLOAT *d2H0dg2, FLOAT *d2H0dt2, FLOAT *d2H0dgec, FLOAT *d2H0dtec, FLOAT *d2H0dgt)
{
  FLOAT c1, c2, A, dAdec, dAdg, d2Adec2, d2Adg2, d2Adecg;
  FLOAT g3, t2, t4, n0, d0;
  FLOAT f, dfdA, dfdt, pH0pg, pH0pA, pH0pt;
  FLOAT d2fdA2, d2fdAt, d2fdt2, p2H0pg2, p2H0pgA, p2H0pgt, p2H0pA2, p2H0pAt, p2H0pt2;

  c1 = pw91_beta*pw91_beta/(2.0*pw91_alpha);
  c2 = 2.0*pw91_alpha/pw91_beta;

  A_eq14(order, ec, g, &A, &dAdec, &dAdg, &d2Adec2, &d2Adg2, &d2Adecg);

  g3 = g*g*g;
  t2 = t*t;
  t4 = t2*t2;

  n0 = t2 + A*t4;
  d0 = 1.0 + A*t2 + A*A*t4;
  f  = n0/d0;

  *H0 = c1*g3*log(1.0 + c2*f);

  if(order < 1) return;

  dfdA = -A*t2*t4*(2.0 + A*t2)/(d0*d0);
  dfdt = 2.0*t*(1.0 + 2.0*A*t2)/(d0*d0);

  pH0pg = 3.0*(*H0)/g;
  pH0pA = c1*g3*c2*dfdA/(1.0 + c2*f);
  pH0pt = c1*g3*c2*dfdt/(1.0 + c2*f);

  *dH0dec = pH0pA*dAdec;
  *dH0dg  = pH0pg + pH0pA*dAdg;
  *dH0dt  = pH0pt;

  if(order < 2) return;

  d2fdA2 = 2.0*t4*t2*(A*A*t4*(A*t2 + 3.0) - 1.0)/(d0*d0*d0);
  d2fdAt = -12.0*A*t4*t*(A*t2 + 1.0)/(d0*d0*d0);
  d2fdt2 = (2.0 - 2.0*A*t2*(A*t2*(9.0 + 10.0*A*t2) - 3.0))/(d0*d0*d0);

  p2H0pg2 = 2.0*pH0pg/g;
  p2H0pgA = 3.0*pH0pA/g;
  p2H0pgt = 3.0*pH0pt/g;
  p2H0pA2 = c1*g3*c2*(d2fdA2*(1.0 + c2*f) - c2*dfdA*dfdA)/((1.0 + c2*f)*(1.0 + c2*f));
  p2H0pAt = c1*g3*c2*(d2fdAt*(1.0 + c2*f) - c2*dfdA*dfdt)/((1.0 + c2*f)*(1.0 + c2*f));
  p2H0pt2 = c1*g3*c2*(d2fdt2*(1.0 + c2*f) - c2*dfdt*dfdt)/((1.0 + c2*f)*(1.0 + c2*f));

  *d2H0dec2 = p2H0pA2*dAdec*dAdec + pH0pA*d2Adec2;
  *d2H0dgec = p2H0pgA*dAdec + p2H0pA2*dAdec*dAdg + pH0pA*d2Adecg;
  *d2H0dtec = p2H0pAt*dAdec;
  *d2H0dg2  = p2H0pg2 + 2.0*p2H0pgA*dAdg + p2H0pA2*dAdg*dAdg + pH0pA*d2Adg2;
  *d2H0dgt  = p2H0pgt + p2H0pAt*dAdg;
  *d2H0dt2  = p2H0pt2;
}


/* pade parametrized form of C-xc found in
   M Rasolt & DJW Geldart, Phys. Rev. B 34, 1325 (1986)
*/
static inline void 
Rasold_Geldart_C_xc(int order, FLOAT rs, FLOAT *C_xc, FLOAT *drs, FLOAT *d2rs)
{
  const FLOAT 
    a[3] = {2.568, 23.266, 0.007389},
    b[3] = {1.0, 8.723, 0.472};
  
    FLOAT d0, d1, d2, n0, n1, n2;

  n0 = (a[0] + rs*(a[1] + rs*a[2]));
  d0 =  b[0] + rs*(b[1] + rs*(b[2] + 10.0*rs*a[2]));

  *C_xc = n0/(1000.0*d0);

  if(order < 1) return;

  n1 = a[1] + 2.0*rs*a[2];
  d1 = b[1] + 2.0*rs*b[2] + 10.0*3.0*rs*rs*a[2];

  *drs  = (n1*d0 - n0*d1)/(1000.0*d0*d0);

  if(order < 2) return;

  n2 = 2.0*a[2];
  d2 = 2.0*b[2] + 10.0*3.0*2.0*rs*a[2];

  *d2rs = (2.0*n0*d1*d1 - 2.0*d0*d1*n1 - d0*n0*d2 + d0*d0*n2)/(1000.0*d0*d0*d0);
}


static void 
H1_eq15(int order, FLOAT   rs, FLOAT   g, FLOAT   t, FLOAT *H1,
	FLOAT *dH1drs, FLOAT *dH1dg, FLOAT *dH1dt,
	FLOAT *d2H1drs2, FLOAT *d2H1dg2, FLOAT *d2H1dt2, FLOAT *d2H1dgrs, FLOAT *d2H1dtrs, FLOAT *d2H1dgt)
{
  const FLOAT C_xc0 = 2.568e-3, C_x = -0.001667;
  const FLOAT a1 = -100.0*0.663436439606450070377435073068; /* -100*4/Pi (4/(9 Pi))^(1/3) */

  FLOAT g3, g4, t2, C_xc;
  FLOAT p1, dp1drs, dp1dg, dp1dt, d2p1drs2, d2p1dgrs, d2p1dtrs, d2p1dg2, d2p1dgt, d2p1dt2;
  FLOAT p2, dp2drs, d2p2drs2;

  g3  = g*g*g;
  g4  = g3*g;
  t2  = t*t;

  p1 = a1*rs*g4*t2;
  p1 = exp(p1);

  Rasold_Geldart_C_xc(order, rs, &C_xc, &dp2drs, &d2p2drs2);
  p2 = C_xc - C_xc0 - 3.0*C_x/7.0;

  *H1  = pw91_nu*p2*g3*t2*p1;

  if(order < 1) return;

  dp1drs = a1*g4*t2*p1;
  dp1dg  = a1*rs*4.0*g3*t2*p1;
  dp1dt  = a1*rs*g4*2.0*t*p1;

  *dH1drs = pw91_nu*g3*t2*(dp2drs*p1  + p2*dp1drs);
  *dH1dg  = pw91_nu*p2*t2*(3.0*g*g*p1 + g3*dp1dg);
  *dH1dt  = pw91_nu*p2*g3*(2.0*t*p1   + t2*dp1dt);

  if(order < 2) return;

  d2p1drs2 = a1*g4*t2*dp1drs;
  d2p1dgrs = a1*t2*(4.0*g3*p1 + g4*dp1dg);
  d2p1dtrs = a1*g4*(2.0*t *p1 + t2*dp1dt);
  d2p1dg2  = a1*rs*t2*4.0*(3.0*g*g*p1 + g3*dp1dg);
  d2p1dgt  = a1*rs*4.0*g3*(2.0*t*p1 + t2*dp1dt);
  d2p1dt2  = a1*rs*g4*2.0*(p1 + t*dp1dt);

  *d2H1drs2 = pw91_nu*g3*t2*(d2p2drs2*p1 + 2.0*dp2drs*dp1drs + p2*d2p1drs2);
  *d2H1dgrs = pw91_nu*t2*(3.0*g*g*(dp2drs*p1 + p2*dp1drs) + g3*(dp2drs*dp1dg + p2*d2p1dgrs));
  *d2H1dtrs = pw91_nu*g3*(2.0*t  *(dp2drs*p1 + p2*dp1drs) + t2*(dp2drs*dp1dt + p2*d2p1dtrs));
  *d2H1dg2  = pw91_nu*p2*t2*(6.0*g*p1 + 6.0*g*g*dp1dg + g3*d2p1dg2);
  *d2H1dgt  = pw91_nu*p2*(2.0*t*(3.0*g*g*p1 + g3*dp1dg) + t2*(3.0*g*g*dp1dt + g3*d2p1dgt));
  *d2H1dt2  = pw91_nu*p2*g3*(2.0*p1 + 4.0*t*dp1dt + t2*d2p1dt2);
}


void 
XC(gga_c_pw91_func) (const XC(func_type) *p, XC(gga_work_c_t) *r)
{
  FLOAT g, dgdz, d2gdz2;
  FLOAT t, dtdrs, dtdxt, dtdg, d2tdrs2, d2tdrsxt, d2tdg2, d2tdgrs, d2tdxtg;
  FLOAT H0, dH0dec, dH0dg, dH0dt, d2H0dec2, d2H0dg2, d2H0dt2, d2H0dgec, d2H0dtec, d2H0dgt;
  FLOAT H1, dH1drs, dH1dg, dH1dt, d2H1drs2, d2H1dg2, d2H1dt2, d2H1dgrs, d2H1dtrs, d2H1dgt;

  XC(lda_work_t) pw;
  FLOAT tconv, auxp, auxm;

  pw.order = r->order;
  pw.rs[0] = SQRT(r->rs);
  pw.rs[1] = r->rs;
  pw.rs[2] = r->rs*r->rs;
  pw.zeta  = r->zeta;

  XC(lda_c_pw_func)(p->func_aux[0], &pw);

  tconv = 4.0*M_CBRT2;

  auxp = CBRT(1.0 + r->zeta);
  auxm = CBRT(1.0 - r->zeta);

  g    = 0.5*(auxp*auxp + auxm*auxm); /* g is called phi in PBE */
  t    = r->xt/(tconv*g*pw.rs[0]);

  H0_eq13(r->order, pw.zk, g, t, &H0, &dH0dec, &dH0dg, &dH0dt,
	  &d2H0dec2, &d2H0dg2, &d2H0dt2, &d2H0dgec, &d2H0dtec, &d2H0dgt);
  H1_eq15(r->order, r->rs, g, t, &H1, &dH1drs, &dH1dg, &dH1dt,
	  &d2H1drs2, &d2H1dg2, &d2H1dt2, &d2H1dgrs, &d2H1dtrs, &d2H1dgt);

  r->f = pw.zk + H0 + H1;

  if(r->order < 1) return;

  dgdz = 0.0;
  if(auxp > p->info->min_zeta) dgdz += 1/auxp;
  if(auxm > p->info->min_zeta) dgdz -= 1/auxm;
  dgdz *= 1.0/3.0;

  dtdrs = -r->xt/(2.0*tconv*g*r->rs*pw.rs[0]);
  dtdxt =  t/r->xt;
  dtdg  = -t/g;

  r->dfdrs    = dH1drs + (1.0 + dH0dec)*pw.dedrs + (dH0dt + dH1dt)*dtdrs;
  r->dfdz     = (1.0 + dH0dec)*pw.dedz  + (dH0dg + dH1dg + (dH0dt + dH1dt)*dtdg)*dgdz;
  r->dfdxt    = (dH0dt + dH1dt)*dtdxt;
  r->dfdxs[0] = 0.0;
  r->dfdxs[1] = 0.0;

  if(r->order < 2) return;

  d2gdz2 = 0.0;
  if(auxp > p->info->min_zeta) d2gdz2 += 1.0/((1.0 + r->zeta)*auxp);
  if(auxm > p->info->min_zeta) d2gdz2 += 1.0/((1.0 - r->zeta)*auxm);
  d2gdz2 *= -1.0/9.0;

  d2tdrs2  =  3.0*r->xt/(4.0*tconv*g*pw.rs[2]*pw.rs[0]);
  d2tdrsxt =  dtdrs/r->xt;
  d2tdg2   = -2.0*dtdg/g;
  d2tdgrs  = -dtdrs/g;
  d2tdxtg  =  dtdg/r->xt;
  
  r->d2fdrs2     = d2H1drs2 + d2H1dtrs*dtdrs + (1.0 + dH0dec)*pw.d2edrs2 + d2H0dec2*pw.dedrs*pw.dedrs
    + 2.0*d2H0dtec*pw.dedrs*dtdrs + (d2H0dt2 + d2H1dt2)*dtdrs*dtdrs + (dH0dt + dH1dt)*d2tdrs2;
  r->d2fdrsz     = (1.0 + dH0dec)*pw.d2edrsz + pw.dedrs*(d2H0dec2*pw.dedz + dgdz*(d2H0dtec*dtdg + d2H0dgec))
    + (d2H1dgrs + d2H1dtrs*dtdg)*dgdz + (dH0dt + dH1dt)*dgdz*d2tdgrs + 
    dtdrs*(d2H0dtec*pw.dedz + dgdz*((d2H0dt2 + d2H1dt2)*dtdg + d2H0dgt + d2H1dgt));
  r->d2fdrsxt    = d2H1dtrs*dtdxt + dtdxt*(d2H0dtec*pw.dedrs + (d2H0dt2 + d2H1dt2)*dtdrs) + (dH0dt + dH1dt)*d2tdrsxt;
  r->d2fdrsxs[0] = 0.0;
  r->d2fdrsxs[1] = 0.0;
  r->d2fdz2      = (1.0 + dH0dec)*pw.d2edz2 + d2H0dec2*pw.dedz*pw.dedz + (dH0dt + dH1dt)*(dtdg*d2gdz2 + d2tdg2*dgdz*dgdz)
    + (dH0dg + dH1dg)*d2gdz2 + 2.0*dgdz*pw.dedz*(d2H0dtec*dtdg + d2H0dgec)
    + dgdz*dgdz*((d2H0dt2 + d2H1dt2)*dtdg*dtdg + 2.0*(d2H0dgt + d2H1dgt)*dtdg + d2H0dg2 + d2H1dg2);
  r->d2fdzxt     = (dH0dt + dH1dt)*d2tdxtg*dgdz + dtdxt*(d2H0dtec*pw.dedz + dgdz*((d2H0dt2 + d2H1dt2)*dtdg + d2H0dgt + d2H1dgt));
  r->d2fdzxs[0]  = 0.0;
  r->d2fdzxs[1]  = 0.0;
  r->d2fdxt2     = (d2H0dt2 + d2H1dt2)*dtdxt*dtdxt;
  r->d2fdxtxs[0] = 0.0;
  r->d2fdxtxs[1] = 0.0;
  r->d2fdxs2[0]  = 0.0;
  r->d2fdxs2[1]  = 0.0;
  r->d2fdxs2[2]  = 0.0;
}

#define func XC(gga_c_pw91_func)
#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_pw91) = {
  XC_GGA_C_PW91,
  XC_CORRELATION,
  "Perdew & Wang 91",
  XC_FAMILY_GGA,
  "JP Perdew, JA Chevary, SH Vosko, KA Jackson, MR Pederson, DJ Singh, and C Fiolhais, Phys. Rev. B 46, 6671 (1992)\n"
  "JP Perdew, JA Chevary, SH Vosko, KA Jackson, MR Pederson, DJ Singh, and C Fiolhais, Phys. Rev. B 48, 4978(E) (1993)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-12, 1e-32, 0.0, 1e-32,
  gga_c_pw91_init,
  NULL, NULL,
  work_gga_c,
};
