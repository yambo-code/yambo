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

#define XC_MGGA_X_MS0          221 /* MS exchange of Sun, Xiao, and Ruzsinszky */
#define XC_MGGA_X_MS1          222 /* MS1 exchange of Sun, et al */
#define XC_MGGA_X_MS2          223 /* MS2 exchange of Sun, et al */
#define XC_MGGA_X_MS2H         224 /* MS2 hybrid exchange of Sun, et al */

typedef struct{
  FLOAT kappa, c, b;
} mgga_x_ms_params;

static void 
mgga_x_ms_init(XC(func_type) *p)
{
  mgga_x_ms_params *params;

  assert(p!=NULL && p->params == NULL);
  p->params = malloc(sizeof(mgga_x_ms_params));
  params = (mgga_x_ms_params *)p->params;

  /* value of beta in standard Becke 88 functional */
  switch(p->info->number){
  case XC_MGGA_X_MS0:
    params->kappa = 0.29;
    params->c     = 0.28771;
    params->b     = 1.0;
    break;
  case XC_MGGA_X_MS1:
    params->kappa = 0.404;
    params->c     = 0.18150;
    params->b     = 1.0;
    break;
  case XC_MGGA_X_MS2:
    params->kappa = 0.504;
    params->c     = 0.14601;
    params->b     = 4.0;
    break;
  case XC_MGGA_X_MS2H:
    p->cam_alpha  = 0.09;
    params->kappa = 0.504;
    params->c     = 0.14601;
    params->b     = 4.0;
    break;
  default:
    fprintf(stderr, "Internal error in mgga_x_ms\n");
    exit(1);
  }
}


static void 
func_fa(FLOAT b, int order, FLOAT a, 
	FLOAT *f, FLOAT *dfda, FLOAT *d2fda2)
{
  FLOAT a2, a4, a6, aux, num, den, dnum, dden, d2num, d2den;

  a2 = a*a;
  a4 = a2*a2;
  a6 = a2*a4;

  aux = 1.0 - a2;
  
  num = aux*aux*aux;
  den = 1.0 + a*a2 + b*a6;

  *f  = num/den;

  if(order < 1) return;              /* And now the derivatives */

  dnum  = -6.0*a*aux*aux;
  dden  = 3.0*a2 + 6.0*b*a*a4;

  *dfda = (dnum*den - num*dden)/(den*den);

  if(order < 2) return;

  d2num   = -6.0*aux*(1.0 - 5.0*a2);
  d2den   = 6.0*a + 30.0*b*a4;

  *d2fda2 = ((d2num*den - num*d2den)*den - 2.0*dden*(dnum*den - dden*num))/(den*den*den);
}


static void
func_f(FLOAT kappa, FLOAT c, int order, FLOAT p, 
	FLOAT *f, FLOAT *dfdp, FLOAT *d2fdp2)
{
  FLOAT mu = 10.0/81.0;
  FLOAT den, dden;

  den = kappa + mu*p + c;

  *f = 1.0 + kappa*(1.0 - kappa/den);

  if(order < 1) return;              /* And now the derivatives */

  dden  = mu;
  *dfdp = kappa*kappa*dden/(den*den);

  if(order < 2) return;

  *d2fdp2 = -2.0*kappa*kappa*dden*dden/(den*den*den);
}


static void 
func(const XC(func_type) *pt, XC(mgga_work_x_t) *r)
{
  FLOAT ss;
  FLOAT alpha, dalphadt, dalphadx, d2alphadx2, fa, dfada, d2fada2;
  FLOAT pp, dpdx, d2pdx2, f0, df0dp, d2f0dp2, f1, df1dp, d2f1dp2;
  mgga_x_ms_params *params;

  params = (mgga_x_ms_params *)pt->params;
  assert(params != NULL);

  ss = X2S*r->x;
  pp = ss*ss;

  alpha = (r->t - r->x*r->x/8.0)/K_FACTOR_C;

  func_fa(params->b, r->order, alpha, &fa, &dfada, &d2fada2);
  func_f (params->kappa, params->c, r->order, pp, &f0, &df0dp, &d2f0dp2);
  func_f (params->kappa,       0.0, r->order, pp, &f1, &df1dp, &d2f1dp2);

  r->f = f1 + fa*(f0 - f1);

  if(r->order < 1) return;

  dpdx = 2.0*ss*X2S;
  dalphadx = -2.0*r->x/(8.0*K_FACTOR_C);
  dalphadt = 1.0/K_FACTOR_C;

  r->dfdx = (df1dp + fa*(df0dp - df1dp))*dpdx + (f0 - f1)*dfada*dalphadx;
  r->dfdt = (f0 - f1)*dfada*dalphadt;
  r->dfdu = 0.0;

  if(r->order < 2) return;

  d2pdx2 = 2.0*X2S*X2S;
  d2alphadx2 = -2.0/(8.0*K_FACTOR_C);

  r->d2fdx2 = (d2f1dp2 + fa*(d2f0dp2 - d2f1dp2))*dpdx*dpdx + (df1dp + fa*(df0dp - df1dp))*d2pdx2
    + 2.0*(df0dp - df1dp)*dpdx*dfada*dalphadx
    + (f0 - f1)*(d2fada2*dalphadx*dalphadx + dfada*d2alphadx2);
  r->d2fdt2 = (f0 - f1)*d2fada2*dalphadt*dalphadt;
  r->d2fdxt = (f0 - f1)*d2fada2*dalphadt*dalphadx;
}


#include "work_mgga_x.c"


XC(func_info_type) XC(func_info_mgga_x_ms0) = {
  XC_MGGA_X_MS0,
  XC_EXCHANGE,
  "MS exchange of Sun, Xiao, and Ruzsinszky",
  XC_FAMILY_MGGA,
  "J Sun, B Xiao, and A Ruzsinszky, J. Chem. Phys. 137, 051101 (2012)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_x_ms_init,
  NULL, NULL, NULL,
  work_mgga_x,
};

XC(func_info_type) XC(func_info_mgga_x_ms1) = {
  XC_MGGA_X_MS1,
  XC_EXCHANGE,
  "MS1 exchange of Sun, et al",
  XC_FAMILY_MGGA,
  "J Sun, R Haunschild, B Xiao, IW Bulik, GE Scuseria, JP Perdew, arXiv:1301.2239 (2013)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_x_ms_init,
  NULL, NULL, NULL,
  work_mgga_x,
};

XC(func_info_type) XC(func_info_mgga_x_ms2) = {
  XC_MGGA_X_MS2,
  XC_EXCHANGE,
  "MS2 exchange of Sun, et al",
  XC_FAMILY_MGGA,
  "J Sun, R Haunschild, B Xiao, IW Bulik, GE Scuseria, JP Perdew, arXiv:1301.2239 (2013)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_x_ms_init,
  NULL, NULL, NULL,
  work_mgga_x,
};

XC(func_info_type) XC(func_info_mgga_x_ms2h) = {
  XC_MGGA_X_MS2H,
  XC_EXCHANGE,
  "MS2 hybrid exchange of Sun, et al",
  XC_FAMILY_MGGA,
  "J Sun, R Haunschild, B Xiao, IW Bulik, GE Scuseria, JP Perdew, arXiv:1301.2239 (2013)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_x_ms_init,
  NULL, NULL, NULL,
  work_mgga_x,
};
