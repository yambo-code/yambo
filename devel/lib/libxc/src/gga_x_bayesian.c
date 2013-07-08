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

#define XC_GGA_X_BAYESIAN          125 /* Bayesian best fit for the enhancement factor */

static inline void 
func(const XC(gga_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  static const FLOAT theta[3] = {
    1.0008, 0.1926, 1.8962
  };

  FLOAT ss, f0, df0, d2f0;

  ss = X2S*x;

  f0 = ss/(1.0 + ss);

  *f = theta[0] + f0*f0*(theta[1] + f0*f0*theta[2]);

  if(order < 1) return;

  df0 = 1.0/((1.0 + ss)*(1.0 + ss));

  *dfdx  = X2S*f0*(2.0*theta[1] + 4.0*f0*f0*theta[2])*df0;

  if(order < 2) return;

  d2f0 = -2.0*df0/(1.0 + ss);
  *d2fdx2 = X2S*X2S*
    (f0*(2.0*theta[1] + 4.0*f0*f0*theta[2])*d2f0 +
     (2.0*theta[1] + 4.0*3.0*f0*f0*theta[2])*df0*df0);
}

#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_bayesian) = {
  XC_GGA_X_BAYESIAN,
  XC_EXCHANGE,
  "Bayesian best fit for the enhancement factor",
  XC_FAMILY_GGA,
  "JJ Mortensen, K Kaasbjerg, SL Frederiksen, JK Nørskov, JP Sethna, and KW Jacobsen, Phys. Rev. Lett. 95, 216401 (2005)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  NULL, NULL, NULL,
  work_gga_x
};
