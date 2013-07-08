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

#define XC_GGA_K_PEARSON          511 /* Pearson */

static inline void 
func(const XC(gga_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  FLOAT ss, ss2, ss6, denom;

  ss  = X2S*x;
  ss2 = ss*ss;
  ss6 = ss2*ss2*ss2;
  denom = 1.0 + ss6;

  *f = 1.0 + 5.0/27.0 * ss2/denom;

  if(order < 1) return;

  *dfdx = X2S*5.0/27.0 * 2.0*ss*(1.0 - 2.0*ss6)/(denom*denom);
  
  if(order < 2) return;

  *d2fdx2 = X2S*X2S*5.0/27.0 * (2.0 - 50.0*ss6 + 20.0*ss6*ss6)/(denom*denom*denom);
}

#define XC_KINETIC_FUNCTIONAL
#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_k_pearson) = {
  XC_GGA_K_PEARSON,
  XC_KINETIC,
  "Pearson 1992",
  XC_FAMILY_GGA,
  "DJ Lacks and RG Gordon, J. Chem. Phys. 100, 4446 (1994)\n"
  "E W Pearson and R G Gordon, J. Chem. Phys. 82, 881 (1985)\n"
  "E W Pearson, Ph.D. thesis, Harvard University (1983)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  NULL, NULL, NULL,
  work_gga_k
};
