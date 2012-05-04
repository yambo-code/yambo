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

#define XC_GGA_X_2D_B86          128 /* Becke 86 Xalfa,beta,gamma                      */

static inline void 
func(const XC(gga_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  FLOAT beta=0.002105, gamma=0.000119;

  FLOAT f1, f2, df1, df2, d2f1, d2f2;

  f1    = 1.0 + beta*x*x;
  f2    = 1.0 + gamma*x*x;
  *f    = f1/f2;
  
  if(order < 1) return;

  df1   = 2.0*beta*x;
  df2   = 2.0*gamma*x;

  *dfdx  = (df1*f2 - f1*df2)/(f2*f2);

  if(order < 2) return;

  d2f1 = 2.0*beta;
  d2f2 = 2.0*gamma;

  *d2fdx2 = (2.0*f1*df2*df2 + d2f1*f2*f2 - f2*(2.0*df1*df2 + f1*d2f2))/(f2*f2*f2);
}

#define XC_DIMENSIONS 2
#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_2d_b86) = {
  XC_GGA_X_2D_B86,
  XC_EXCHANGE,
  "Becke 86 in 2D",
  XC_FAMILY_GGA,
  "G Vilhena and MAL Marques, unpublished\n"
  "AD Becke, J. Chem. Phys 84, 4524 (1986)",
  XC_FLAGS_2D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  NULL, NULL, NULL,
  work_gga_x
};

