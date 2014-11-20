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
#include "util.h"

#define XC_GGA_X_LG93  113 /* Lacks & Gordon 93 */

static inline void 
func(const XC(func_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  static const FLOAT ad = 1e-8, a4 = 29.790, a6 = 22.417;
  static const FLOAT a8 = 12.119, a10 = 1570.1, a12 = 55.944;
  static const FLOAT a2 = 4.94113918475214219939; /* (ad + 0.1234)/b, b = 0.024974 */
  static const FLOAT b  = 0.024974;

  FLOAT ss, ss2, ss4, ss6, ss8, ss10;
  FLOAT f0, f1, f2, df0, df1, df2, d2f0, d2f1, d2f2;

  ss  = X2S*x;    ss2  = ss*ss;
  ss4 = ss2*ss2;  ss6  = ss4*ss2;
  ss8 = ss6*ss2;  ss10 = ss8*ss2;

  f0 = 1.0 + a2*ss2 + a4*ss4 + a6*ss6 + a8*ss8 + a10*ss10 + a12*ss2*ss10;
  f1 = POW(f0, b);
  f2 = 1.0 + ad*ss2;

  *f = f1/f2;

  if(order < 1) return;

  df0 = ss*(2.0*a2 + 4.0*a4*ss2 + 6.0*a6*ss4 + 8.0*a8*ss6 + 10.0*a10*ss8 + 12.0*a12*ss10);
  df1 = b*df0*POW(f0, b-1.0);
  df2 = 2.0*ss*ad;

  *dfdx  = X2S*(df1*f2 - f1*df2)/(f2*f2);

  if(order < 2) return;
  
  d2f0 = 2.0*1.0*a2 + 4.0*3.0*a4*ss2 + 6.0*5.0*a6*ss4 + 8.0*7.0*a8*ss6 + 
    10.0*9.0*a10*ss8 + 12.0*11.0*a12*ss10;
  d2f1 = b*POW(f0, b-1.0)*(d2f0 + (b-1.0)*df0*df0/f0);
  d2f2 = 2.0*ad;

  *d2fdx2 = X2S*X2S*(2.0*f1*df2*df2 + d2f1*f2*f2 - f2*(2.0*df1*df2 + f1*d2f2))/(f2*f2*f2);
}

#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_lg93) = {
  XC_GGA_X_LG93,
  XC_EXCHANGE,
  "Lacks & Gordon 93",
  XC_FAMILY_GGA,
  "DJ Lacks and RG Gordon, Phys. Rev. A 47, 4681 (1993)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  NULL, NULL, NULL,
  work_gga_x
};

