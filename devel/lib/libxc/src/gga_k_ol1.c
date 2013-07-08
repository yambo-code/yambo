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

#define XC_GGA_K_OL1          512 /* Ou-Yang and Levy v.1 */

static inline void 
func(const XC(gga_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  const FLOAT c4 = 0.00677;
  FLOAT ss, ss2;

  ss  = x/M_CBRT2;
  ss2 = ss*ss;

  *f = 1.0 + (ss2/72.0 + c4*ss)/K_FACTOR_C;

  if(order < 1) return;

  *dfdx = (2.0*ss/72.0 + c4)/(K_FACTOR_C*M_CBRT2);
  
  if(order < 2) return;

  *d2fdx2 = 2.0/(72.0*K_FACTOR_C*M_CBRT2*M_CBRT2);
}

#define XC_KINETIC_FUNCTIONAL
#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_k_ol1) = {
  XC_GGA_K_OL1,
  XC_KINETIC,
  "Ou-Yang and Levy v.1",
  XC_FAMILY_GGA,
  "H Ou-Yang, M Levy, Int. J. of Quant. Chem. 40, 379–388 (1991)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  NULL, NULL, NULL,
  work_gga_k
};
