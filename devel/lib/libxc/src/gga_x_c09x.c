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

#define XC_GGA_X_C09X         158 /* C09x to be used with the VdW of Rutgers-Chalmers     */

static inline void 
func(const XC(gga_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  static FLOAT mu = 0.0617, kappa = 1.245, alpha = 0.0483;

  FLOAT ss, ss2, aux;

  ss  = X2S*x;
  ss2 = ss*ss;

  aux = exp(-alpha*ss2);

  *f = 1.0 + mu*ss2*aux + kappa*(1.0 - aux);

  if(order < 1) return;

  *dfdx = X2S * 2.0*ss*aux*(mu + alpha*(kappa - mu*ss2));

  if(order < 2) return;

  *d2fdx2 = X2S*X2S * 2.0*aux*(mu + alpha*(kappa - (2.0*alpha*kappa + 5.0*mu)*ss2 + 2.0*alpha*mu*ss2*ss2));
}


#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_c09x) = {
  XC_GGA_X_C09X,
  XC_EXCHANGE,
  "C09x to be used with the VdW of Rutgers-Chalmers",
  XC_FAMILY_GGA,
  "VR Cooper, PRB 81, 161104(R) (2010)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  NULL, NULL, NULL,
  work_gga_x
};
