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

#define XC_GGA_X_RPBE  117 /* Hammer, Hansen & Norskov (PBE-like) */


typedef struct{
  FLOAT kappa, mu;
} gga_x_rpbe_params;


static void 
gga_x_rpbe_init(void *p_)
{
  XC(gga_type) *p = (XC(gga_type) *)p_;

  assert(p->params == NULL);
  p->params = malloc(sizeof(gga_x_rpbe_params));

  /* same parameters as standard PBE */
  XC(gga_x_rpbe_set_params_)(p, 0.8040, 0.2195149727645171);
}


void 
XC(gga_x_rpbe_set_params)(XC(func_type) *p, FLOAT kappa, FLOAT mu)
{
  assert(p != NULL && p->gga != NULL);
  XC(gga_x_rpbe_set_params_)(p->gga, kappa, mu);
}


void 
XC(gga_x_rpbe_set_params_)(XC(gga_type) *p, FLOAT kappa, FLOAT mu)
{
  gga_x_rpbe_params *params;

  assert(p->params != NULL);
  params = (gga_x_rpbe_params *) (p->params);

  params->kappa = kappa;
  params->mu    = mu;
}


/* RPBE: see PBE for more details */
void XC(gga_x_rpbe_enhance) 
  (const XC(gga_type) *p, int order, FLOAT x, 
   FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  FLOAT kappa, mu, f0, df0, d2f0;

  assert(p->params != NULL);
  kappa = ((gga_x_rpbe_params *) (p->params))->kappa;
  mu    = ((gga_x_rpbe_params *) (p->params))->mu*X2S*X2S;

  f0 = exp(-mu*x*x/kappa);
  *f = 1.0 + kappa*(1.0 - f0);

  if(order < 1) return;

  df0 = -2.0*x*mu/kappa*f0;
  
  *dfdx  = -kappa*df0;

  if(order < 2) return;

  d2f0    = -2.0*mu/kappa*f0*(1.0 - 2.0*x*x*mu/kappa);
  *d2fdx2 = -kappa*d2f0;
}


#define func XC(gga_x_rpbe_enhance)
#include "work_gga_x.c"


const XC(func_info_type) XC(func_info_gga_x_rpbe) = {
  XC_GGA_X_RPBE,
  XC_EXCHANGE,
  "Hammer, Hansen, and Nørskov",
  XC_FAMILY_GGA,
  "B Hammer, LB Hansen and JK Nørskov, Phys. Rev. B 59, 7413 (1999)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_x_rpbe_init, 
  NULL, NULL,
  work_gga_x
};
