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

#define XC_GGA_X_OPTX         110 /* Handy & Cohen OPTX 01                          */

typedef struct{
  FLOAT a, b, gamma;
} gga_x_optx_params;


static void 
gga_x_optx_init(XC(func_type) *p)
{
  assert(p!=NULL && p->params == NULL);
  p->params = malloc(sizeof(gga_x_optx_params));

  XC(gga_x_optx_set_params)(p, 1.05151, 1.43169/X_FACTOR_C, 0.006);
}


void 
XC(gga_x_optx_set_params)(XC(func_type) *p, FLOAT a, FLOAT b, FLOAT gamma)
{
  gga_x_optx_params *params;

  assert(p != NULL && p->params != NULL);
  params = (gga_x_optx_params *) (p->params);

  params->a     = a;
  params->b     = b;
  params->gamma = gamma;
}


static inline void
func(const XC(func_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  FLOAT a, b, gamma;
  FLOAT f1, u, du, d2u;

  assert(p->params != NULL);
  a     = ((gga_x_optx_params *) (p->params))->a;
  b     = ((gga_x_optx_params *) (p->params))->b;
  gamma = ((gga_x_optx_params *) (p->params))->gamma;

  f1 = 1.0 + gamma*x*x;
  u  = gamma*x*x/f1;

  *f     = a + b*u*u;
  
  if(order < 1) return;

  du  = 2.0*gamma*x/(f1*f1);

  *dfdx  = 2.0*b*u*du;

  if(order < 2) return;

  d2u = 2.0*gamma/(f1*f1)*(1.0 - 4.0*gamma*x*x/f1);
  *d2fdx2 = 2.0*b*(du*du + u*d2u);
}

#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_optx) = {
  XC_GGA_X_OPTX,
  XC_EXCHANGE,
  "Handy & Cohen OPTX 01",
  XC_FAMILY_GGA,
  "NC Handy and AJ Cohen, Mol. Phys. 99, 403 (2001)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_optx_init,
  NULL, NULL,
  work_gga_x
};
