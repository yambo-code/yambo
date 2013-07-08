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
#include <assert.h>
#include "util.h"

#define XC_GGA_X_PW91         109 /* Perdew & Wang 91 */
#define XC_GGA_X_mPW91        119 /* Modified form of PW91 by Adamo & Barone */
#define XC_GGA_K_LC94         521 /* Lembarki & Chermette */

static void 
gga_x_pw91_init(void *p_)
{
  XC(gga_type) *p = (XC(gga_type) *)p_;

  switch(p->info->number){
  case XC_GGA_X_PW91:    p->func = 0; break;
  case XC_GGA_X_mPW91:   p->func = 1; break;
  case XC_GGA_K_LC94:    p->func = 2; break;
  } 
}

static inline void 
func(const XC(gga_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  /* The parameters, written in terms of b and beta=5*(36 pi)^(-5/3), are
     aa = 6*b/X2S
     bb = 1/X2S
     cc = b/(X_FACTOR_C*X2S*X2S)
     dd = (b-beta)/(X_FACTOR_C*X2S^2)
     ff = 1e-6/(X_FACTOR_C*X2S^expo)

     with b_PW91~0.0042 and b_mPW91=0.0046
  */

  const FLOAT aa[]   = {0.19645,  0.215157295352585598013916978744,  0.093907};
  const FLOAT bb[]   = { 7.7956,  7.795554179441507081094187014969, 76.320};
  const FLOAT cc[]   = { 0.2743,  0.300416257087080973420256668760,  0.26608};
  const FLOAT dd[]   = {-0.1508, -0.176959466963624190150028425705, -0.0809615};
  const FLOAT ff[]   = {  0.004,  0.002279611815362395620121471751,  0.000057767};
  const FLOAT alpha  = 100.0;
  const FLOAT expo[] = {4.0, 3.73, 4.0};

  FLOAT ss, ss2, ss4;
  FLOAT f1, df1, d2f1, f2, df2, d2f2, f3, df3, d2f3, f4, df4, d2f4;

  ss  = X2S*x;
  ss2 = ss*ss;
  ss4 = POW(ss, expo[p->func]);

  f1 = dd[p->func]*exp(-alpha*ss2);
  f2 = aa[p->func]*asinh(bb[p->func]*ss);
  f3 = (cc[p->func] + f1)*ss2 - ff[p->func]*ss4;
  f4 = 1.0 + ss*f2 + ff[p->func]*ss4;

  *f = 1.0 + f3/f4;

  if(order < 1) return;

  df1 = -2.0*alpha*ss*f1;
  df2 = aa[p->func]*bb[p->func]/SQRT(1.0 + bb[p->func]*bb[p->func]*ss2);
  df3 = 2.0*ss*(cc[p->func] + f1) + ss2*df1 - expo[p->func]*ff[p->func]*POW(ss, expo[p->func] - 1.0);
  df4 = f2 + ss*df2 + expo[p->func]*ff[p->func]*POW(ss, expo[p->func] - 1.0);

  *dfdx  = (df3*f4 - f3*df4)/(f4*f4);
  *dfdx *= X2S;

  if(order < 2) return;

  d2f1 = -2.0*alpha*(f1 + ss*df1);
  d2f2 = -aa[p->func]*bb[p->func]*bb[p->func]*bb[p->func]*ss/POW(1.0 + bb[p->func]*bb[p->func]*ss2, 3.0/2.0);
  d2f3 = 2.0*(cc[p->func] + f1 + 2.0*ss*df1) + ss2*d2f1 - 
    expo[p->func]*(expo[p->func]-1)*ff[p->func]*POW(ss, expo[p->func] - 2.0);
  d2f4 = 2.0*df2 + ss*d2f2 + 
    expo[p->func]*(expo[p->func]-1)*ff[p->func]*POW(ss, expo[p->func] - 2.0);

  *d2fdx2  = (2.0*f3*df4*df4 + d2f3*f4*f4 - f4*(2.0*df3*df4 + f3*d2f4))/(f4*f4*f4);
  *d2fdx2 *= X2S*X2S;
}

#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_pw91) = {
  XC_GGA_X_PW91,
  XC_EXCHANGE,
  "Perdew & Wang 91",
  XC_FAMILY_GGA,
  "JP Perdew, in Proceedings of the 21st Annual International Symposium on the Electronic Structure of Solids, ed. by P Ziesche and H Eschrig (Akademie Verlag, Berlin, 1991), p. 11.\n"
  "JP Perdew, JA Chevary, SH Vosko, KA Jackson, MR Pederson, DJ Singh, and C Fiolhais, Phys. Rev. B 46, 6671 (1992)\n"
  "JP Perdew, JA Chevary, SH Vosko, KA Jackson, MR Pederson, DJ Singh, and C Fiolhais, Phys. Rev. B 48, 4978(E) (1993)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_x_pw91_init,
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_mpw91) = {
  XC_GGA_X_mPW91,
  XC_EXCHANGE,
  "mPW91 of Adamo & Barone",
  XC_FAMILY_GGA,
  "C Adamo and V Barone, J. Chem. Phys. 108, 664 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_x_pw91_init,
  NULL, NULL,
  work_gga_x
};

#define XC_KINETIC_FUNCTIONAL
#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_k_lc94) = {
  XC_GGA_K_LC94,
  XC_KINETIC,
  "Lembarki & Chermette",
  XC_FAMILY_GGA,
  "A Lembarki and H Chermette, Phys. Rev. A 50, 5328–5331 (1994)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_x_pw91_init,
  NULL, NULL,
  work_gga_k
};
