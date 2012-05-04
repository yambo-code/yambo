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

#define XC_GGA_X_FT97_A       114 /* Filatov & Thiel 97 (version A) */
#define XC_GGA_X_FT97_B       115 /* Filatov & Thiel 97 (version B) */

static inline void
func(const XC(gga_type) *p, int order, FLOAT x, FLOAT sigma, 
     FLOAT *f, FLOAT *dfdx,
     FLOAT *vsigma, FLOAT *d2fdx2, FLOAT *v2sigma2, FLOAT *v2sigmax)
{
  static const FLOAT 
    beta0 = 0.002913644, beta1 = 0.0009474169, beta2 = 6255746.320201; /* beta2 = 2501.149^2 ?? (Eq. (16a) */

  FLOAT x2, beta, dbetadsigma, d2betadsigma2; 
  FLOAT f0, f1, f2, df2, d2f2, f3, df3, d2f3;
  FLOAT df3df2, d2f3df2, d2f3df2x;
  int func;

  switch(p->info->number){
  case XC_GGA_X_FT97_B: func = 1; break;
  default:              func = 0; /*  XC_GGA_X_FT97_A */
  }

  if(func==0){
    beta = 0.00293;
  }else{
    f1   = beta2 + sigma;
    beta = beta0 + beta1*sigma/f1;
  }

  x2 = x*x;
  f2 = beta*asinh(x2);
  f3 = SQRT(1.0 + 9.0*x2*f2*f2);
  *f = 1.0 + beta/X_FACTOR_C*x2/f3;
 
  if(order < 1) return;

  f0  = SQRT(1.0 + x2*x2);
  df2 = beta*2.0*x/f0;
  df3 = 9.0*x*f2*(f2 + x*df2)/f3;

  dbetadsigma = (func == 0) ? 0.0 : beta1*beta2/(f1*f1);

  *dfdx = beta/X_FACTOR_C*x*(2.0*f3 - x*df3)/(f3*f3);

  df3df2  = 9.0*x2*f2/f3;
  *vsigma = dbetadsigma*x2/(f3*X_FACTOR_C)*(1.0 - f2*df3df2/f3);

  if(order < 2) return;

  d2f2 = beta*2.0*(1.0 - x2*x2)/(f0*f0*f0);
  d2f3 = 9.0*(x2*f3*df2*df2 + f2*f2*(f3 - x*df3) + x*f2*(df2*(4.0*f3 - x*df3) + x*f3*d2f2)) /
    (f3*f3);

  *d2fdx2 = beta/X_FACTOR_C*(2.0*(f3*f3 - 2.0*x*f3*df3 + x2*df3*df3) - x2*f3*d2f3)/(f3*f3*f3);

  d2betadsigma2 = (func == 0) ? 0.0 : -2.0*dbetadsigma/f1;
  d2f3df2       = 9.0*x2*(f3 - f2*df3df2)/(f3*f3);
  *v2sigma2     = d2betadsigma2*x2/(f3*X_FACTOR_C)*(1.0 - f2*df3df2/f3)
    - dbetadsigma*dbetadsigma*x2*f2/(f3*f3*X_FACTOR_C*beta)*(2.0*df3df2 - 2.0*df3df2*df3df2*f2/f3 + d2f3df2*f2);

  d2f3df2x      = 9.0*x*(2*f2/f3 + x*(df2*f3 - f2*df3)/(f3*f3));
  *v2sigmax     = dbetadsigma*2.0*x/(f3*X_FACTOR_C)*(1.0 - f2*df3df2/f3)
    - dbetadsigma/X_FACTOR_C*x2/(f3*f3)*(df3 + (df2*f3 - 2.0*f2*df3)/f3*df3df2 + d2f3df2x*f2);
}

#define HEADER 2
#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_ft97_a) = {
  XC_GGA_X_FT97_A,
  XC_EXCHANGE,
  "Filatov & Thiel 97 (version A)",
  XC_FAMILY_GGA,
  "M Filatov and W Thiel, Mol. Phys 91, 847 (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  NULL, NULL, NULL, 
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_ft97_b) = {
  XC_GGA_X_FT97_B,
  XC_EXCHANGE,
  "Filatov & Thiel 97 (version B)",
  XC_FAMILY_GGA,
  "M Filatov and W Thiel, Mol. Phys 91, 847 (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  NULL, NULL, NULL, 
  work_gga_x
};
