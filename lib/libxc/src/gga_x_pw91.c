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

#define XC_GGA_X_PW91         109 /* Perdew & Wang 91 */
#define XC_GGA_X_MPW91        119 /* Modified form of PW91 by Adamo & Barone */
#define XC_GGA_K_LC94         521 /* Lembarki & Chermette */

typedef struct{
  FLOAT a, b, c, d, f, alpha, expo;
} gga_x_pw91_params;


static void 
gga_x_pw91_init(XC(func_type) *p)
{
  assert(p!=NULL && p->params == NULL);
  p->params = malloc(sizeof(gga_x_pw91_params));

  switch(p->info->number){
  case XC_GGA_X_PW91:
    /* b_PW91 ~ 0.0042 */
    XC(gga_x_pw91_set_params)(p, 0.19645, 7.7956, 0.2743, -0.1508, 0.004, 100.0, 4.0);
    break;
  case XC_GGA_X_MPW91:
    /*
      === from nwchem source (xc_xmpw91.F) ===
      C. Adamo confirmed that there is a typo in the JCP paper
      b_mPW91 is 0.00426 instead of 0.0046
      
      also the power seems to be 3.72 and not 3.73
    */
    XC(gga_x_pw91_set_params2)(p, 0.00426, 100.0, 3.72);
    break;
  case XC_GGA_K_LC94:
    XC(gga_x_pw91_set_params)(p, 0.093907, 76.320, 0.26608, -0.0809615, 0.000057767, 100.0, 4.0);
    break;
  default:
    fprintf(stderr, "Internal error in gga_x_pw91\n");
    exit(1);
  } 
}

void 
XC(gga_x_pw91_set_params)(XC(func_type) *p, FLOAT a, FLOAT b, FLOAT c, FLOAT d, FLOAT f, FLOAT alpha, FLOAT expo)
{
  gga_x_pw91_params *params;

  assert(p != NULL && p->params != NULL);
  params = (gga_x_pw91_params *) (p->params);

  params->a     = a;
  params->b     = b;
  params->c     = c;
  params->d     = d;
  params->f     = f;
  params->alpha = alpha;
  params->expo  = expo;
}

void 
XC(gga_x_pw91_set_params2)(XC(func_type) *p, FLOAT bt, FLOAT alpha, FLOAT expo)
{
  FLOAT beta;
  FLOAT a, b, c, d, f;

  beta =  5.0*POW(36.0*M_PI,-5.0/3.0);
  a    =  6.0*bt/X2S;
  b    =  1.0/X2S;
  c    =  bt/(X_FACTOR_C*X2S*X2S);
  d    = -(bt - beta)/(X_FACTOR_C*X2S*X2S);
  f    = 1.0e-6/(X_FACTOR_C*POW(X2S, expo));

  XC(gga_x_pw91_set_params)(p, a, b, c, d, f, alpha, expo);
}


void XC(gga_x_pw91_enhance)
  (const XC(func_type) *p, int order, FLOAT x, 
   FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{

  gga_x_pw91_params *params;
  FLOAT ss, ss2, ss4;
  FLOAT f1, df1, d2f1, f2, df2, d2f2, f3, df3, d2f3, f4, df4, d2f4;

  assert(p != NULL && p->params != NULL);
  params = (gga_x_pw91_params *) (p->params);

  ss  = X2S*x;
  ss2 = ss*ss;
  ss4 = POW(ss, params->expo);

  f1 = params->d*exp(-params->alpha*ss2);
  f2 = params->a*asinh(params->b*ss);
  f3 = (params->c + f1)*ss2 - params->f*ss4;
  f4 = 1.0 + ss*f2 + params->f*ss4;

  *f = 1.0 + f3/f4;

  if(order < 1) return;

  df1 = -2.0*params->alpha*ss*f1;
  df2 = params->a*params->b/SQRT(1.0 + params->b*params->b*ss2);
  df3 = 2.0*ss*(params->c + f1) + ss2*df1 - params->expo*params->f*POW(ss, params->expo - 1.0);
  df4 = f2 + ss*df2 + params->expo*params->f*POW(ss, params->expo - 1.0);

  *dfdx  = (df3*f4 - f3*df4)/(f4*f4);
  *dfdx *= X2S;

  if(order < 2) return;

  d2f1 = -2.0*params->alpha*(f1 + ss*df1);
  d2f2 = -params->a*params->b*params->b*params->b*ss/POW(1.0 + params->b*params->b*ss2, 3.0/2.0);
  d2f3 = 2.0*(params->c + f1 + 2.0*ss*df1) + ss2*d2f1 - 
    params->expo*(params->expo - 1.0)*params->f*POW(ss, params->expo - 2.0);
  d2f4 = 2.0*df2 + ss*d2f2 + 
    params->expo*(params->expo - 1.0)*params->f*POW(ss, params->expo - 2.0);

  *d2fdx2  = (2.0*f3*df4*df4 + d2f3*f4*f4 - f4*(2.0*df3*df4 + f3*d2f4))/(f4*f4*f4);
  *d2fdx2 *= X2S*X2S;
}

#define func XC(gga_x_pw91_enhance)
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
  1e-25, 1e-25, 0.0, 1e-32,
  gga_x_pw91_init,
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_mpw91) = {
  XC_GGA_X_MPW91,
  XC_EXCHANGE,
  "mPW91 of Adamo & Barone",
  XC_FAMILY_GGA,
  "C Adamo and V Barone, J. Chem. Phys. 108, 664 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-31, 1e-31, 0.0, 1e-32,
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
  "A Lembarki and H Chermette, Phys. Rev. A 50, 5328-5331 (1994)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-30, 1e-30, 0.0, 1e-32,
  gga_x_pw91_init,
  NULL, NULL,
  work_gga_k
};
