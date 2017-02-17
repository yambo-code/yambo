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

#define XC_GGA_X_KT1          145 /* Keal and Tozer version 1             */
#define XC_GGA_XC_KT2         146 /* Keal and Tozer version 2             */

#define HEADER 3

static inline void 
func(const XC(gga_type) *p, int order, FLOAT x, FLOAT ds,
     FLOAT *f, FLOAT *dfdx, FLOAT *lvrho)
{
  FLOAT gamma = -0.006, delta = 0.1;

  FLOAT dd, n13, n43;

  n13 = CBRT(ds);
  n43 = ds*n13;
  dd  = 1.0/(n43 + delta);
 
  *f = 1.0 - gamma/X_FACTOR_C * x*x * n43*dd;

  if(order < 1) return;

  *dfdx  = - gamma/X_FACTOR_C * 2.0*x * n43*dd;
  *lvrho = - gamma/X_FACTOR_C * x*x * (4.0/3.0)*n13 * delta * dd*dd;

  if(order < 2) return;

  /* to be done */
}


#include "work_gga_x.c"

static void
gga_xc_kt2_init(void *p_)
{
  static int   funcs_id  [3] = {XC_LDA_X, XC_GGA_X_KT1, XC_LDA_C_VWN};
  static FLOAT funcs_coef[3] = {1.07173 - 1.0, 1.0, 0.576727};
  XC(gga_type) *p = (XC(gga_type) *)p_;

  XC(gga_init_mix)(p, 2, funcs_id, funcs_coef);  
}


const XC(func_info_type) XC(func_info_gga_x_kt1) = {
  XC_GGA_X_KT1,
  XC_EXCHANGE,
  "Keal and Tozer, version 1",
  XC_FAMILY_GGA,
  "TW Keal and DJ Tozer, J. Chem. Phys. 119, 3015 (2003)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  NULL, 
  NULL, NULL,
  work_gga_x
};


const XC(func_info_type) XC(func_info_gga_xc_kt2) = {
  XC_GGA_XC_KT2,
  XC_EXCHANGE_CORRELATION,
  "Keal and Tozer, version 2",
  XC_FAMILY_GGA,
  "TW Keal and DJ Tozer, J. Chem. Phys. 119, 3015 (2003)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_xc_kt2_init, 
  NULL, NULL, NULL
};


