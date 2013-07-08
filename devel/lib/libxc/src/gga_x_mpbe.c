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

#define XC_GGA_X_MPBE         122 /* Adamo & Barone modification to PBE             */

static inline void 
func(const XC(gga_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  static FLOAT a = 0.157;
  static FLOAT c1 = 0.21951, c2 = -0.015;

  FLOAT ss, ss2, f0, df0, d2f0, f1;

  ss  = X2S*x;
  ss2 = ss*ss;

  f1 = 1.0 + a*ss2;
  f0 = ss2/f1;
  *f = 1.0 + c1*f0 + c2*f0*f0;

  if(order < 1) return;

  df0 = 2.0*ss/(f1*f1);

  *dfdx  = X2S*(c1 + 2.0*c2*f0)*df0;

  if(order < 2) return;

  d2f0 = (2.0 - 6.0*a*ss*ss)/(f1*f1*f1);
  *d2fdx2 = X2S*X2S*((c1 + 2.0*c2*f0)*d2f0 + 2.0*c2*df0*df0);
}


#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_mpbe) = {
  XC_GGA_X_MPBE,
  XC_EXCHANGE,
  "Adamo & Barone modification to PBE",
  XC_FAMILY_GGA,
  "C Adamo and V Barone, J. Chem. Phys. 116, 5933 (2002)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  NULL, NULL, NULL,
  work_gga_x
};
