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

#define XC_GGA_X_G96          107 /* Gill 96                                        */

void
XC(gga_x_g96_enhance)(const XC(func_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  static const FLOAT c1 = 1.0/137.0;
  FLOAT sx = SQRT(x);

  *f     = 1.0 + c1/X_FACTOR_C*x*sx;

  if(order < 1) return;

  *dfdx  = 3.0*c1/(2.0*X_FACTOR_C)*sx;

  if(order < 2) return;

  *d2fdx2 = 3.0*c1/(4.0*X_FACTOR_C*sx);
}

#define func XC(gga_x_g96_enhance)
#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_g96) = {
  XC_GGA_X_G96,
  XC_EXCHANGE,
  "Gill 96",
  XC_FAMILY_GGA,
  "PMW Gill, Mol. Phys. 89, 433 (1996)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  NULL, NULL, NULL,
  work_gga_x
};
