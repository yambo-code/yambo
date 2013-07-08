/*
 Copyright (C) 2008 Georg Madsen

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

#define XC_GGA_X_PBEA  121 /* Madsen (PBE-like) */

/* PBEA: see PBE for more details */
static inline void 
func(const XC(gga_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  static const FLOAT kappa = 0.8040;
  static const FLOAT mu = 0.00361218645365094697;
  /* hard-coded alpha*/
  static const FLOAT alpha = 0.5;

  FLOAT f0, df0, d2f0;

  f0 = 1.0 + mu*x*x/(alpha*kappa);
  *f = 1.0 + kappa*(1.0 - POW(f0, -alpha));

  if(order < 1) return;

  df0 = 2.0*mu*x/(alpha*kappa);

  *dfdx  = alpha*kappa*df0*POW(f0, -(alpha + 1.0));

  if(order < 2) return;

  d2f0 = 2.0*mu/(alpha*kappa);
  *d2fdx2 = alpha*kappa*POW(f0, -alpha - 1.0)*
    (d2f0 - (alpha + 1.0)*df0*df0/f0);
}

#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_pbea) = {
  XC_GGA_X_PBEA,
  XC_EXCHANGE,
  "Madsen 07",
  XC_FAMILY_GGA,
  "G Madsen, Phys. Rev. B 75, 195108 (2007)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  NULL, NULL, NULL,
  work_gga_x
};
