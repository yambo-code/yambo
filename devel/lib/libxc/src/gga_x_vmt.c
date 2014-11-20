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

#define XC_GGA_X_VMT_PBE          71 /* Vela, Medel, and Trickey with mu = mu_PBE */
#define XC_GGA_X_VMT_GE           70 /* Vela, Medel, and Trickey with mu = mu_GE  */
#define XC_GGA_X_VMT84_PBE        69 /* VMT{8,4} with constraint satisfaction with mu = mu_PBE  */
#define XC_GGA_X_VMT84_GE         68 /* VMT{8,4} with constraint satisfaction with mu = mu_GE  */

typedef struct{
  FLOAT mu;
  FLOAT alpha;
} gga_x_vmt_params;


static void 
gga_x_vmt_init(XC(func_type) *p)
{
  gga_x_vmt_params *params;

  assert(p != NULL && p->params == NULL);
  p->params = malloc(sizeof(gga_x_vmt_params));
  params = (gga_x_vmt_params *) (p->params);

  switch(p->info->number){
  case XC_GGA_X_VMT_PBE:
    params->mu    = 0.2195149727645171;
    params->alpha = 0.002762;
    break;
  case XC_GGA_X_VMT_GE:
    params->mu = 10.0/81.0;
    params->alpha = 0.001553;
    break;
  case XC_GGA_X_VMT84_PBE:
    params->mu    = 0.2195149727645171;
    params->alpha = 0.000074;
    break;
  case XC_GGA_X_VMT84_GE:
    params->mu = 10.0/81.0;
    params->alpha = 0.000023;
    break;
  default:
    fprintf(stderr, "Internal error in gga_x_vmt\n");
    exit(1);
  }
}


void XC(gga_x_vmt_enhance) 
  (const XC(func_type) *p, int order, FLOAT x, 
   FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  FLOAT mu, alpha, ss, ss2, ss4, ss6, ss8, ss10, f0, f1, df0, d2f0, g0, g1;

  assert(p->params != NULL);
  mu    = ((gga_x_vmt_params *) (p->params))->mu;
  alpha = ((gga_x_vmt_params *) (p->params))->alpha;

  ss  = X2S*x;
  ss2 = ss*ss;
  ss4 = ss2*ss2;
 
  f0 = EXP(-alpha*ss2);
  f1 = 1.0 + mu*ss2;

  *f = 1.0 + mu*ss2*f0/f1;

  if(p->info->number == XC_GGA_X_VMT84_PBE || p->info->number == XC_GGA_X_VMT84_GE){
    g0 = EXP(-alpha*ss4);
    
    *f += (1.0 - g0)*(1/ss2 - 1.0);
  }

  if(order < 1) return;
  
  ss6 = ss2*ss4;

  *dfdx = X2S*2.0*mu*ss*(1.0 - alpha*ss2 - alpha*mu*ss4)*f0/(f1*f1);

  if(p->info->number == XC_GGA_X_VMT84_PBE || p->info->number == XC_GGA_X_VMT84_GE){
    *dfdx += -2.0*X2S*(1.0 + g0*(2.0*alpha*ss6 - 2.0*alpha*ss4 - 1.0))/(ss*ss2);
  }

  if(order < 2) return;

  ss8  = ss2*ss6;
  ss10 = ss2*ss8;

  *d2fdx2 = X2S*X2S*2.0*mu*
    (1.0 + ss2*(-3.0*mu - 5.0*alpha) + ss4*(2.0*alpha*alpha - 6.0*alpha*mu) + 
     ss6*(-alpha*mu*mu + 4.0*alpha*alpha*mu) + ss8*(2.0*alpha*alpha*mu*mu))*f0/(f1*f1*f1);

  if(p->info->number == XC_GGA_X_VMT84_PBE || p->info->number == XC_GGA_X_VMT84_GE){
    *d2fdx2 += 2.0*X2S*X2S*(3.0 + g0*(8*alpha*alpha*ss10 - 8.0*alpha*alpha*ss8 - 6.0*alpha*ss6 - 2.0*alpha*ss4 - 3.0))/ss4;
  }
}


#define func XC(gga_x_vmt_enhance)
#include "work_gga_x.c"


const XC(func_info_type) XC(func_info_gga_x_vmt_pbe) = {
  XC_GGA_X_VMT_PBE,
  XC_EXCHANGE,
  "Vela, Medel, and Trickey with mu = mu_PBE",
  XC_FAMILY_GGA,
  "A. Vela, V. Medel, and S. B. Trickey, J. Chem. Phys. 130, 244103 (2009)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_vmt_init, 
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_vmt_ge) = {
  XC_GGA_X_VMT_GE,
  XC_EXCHANGE,
  "Vela, Medel, and Trickey with mu = mu_GE",
  XC_FAMILY_GGA,
  "A. Vela, V. Medel, and S. B. Trickey, J. Chem. Phys. 130, 244103 (2009)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_vmt_init, 
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_vmt84_pbe) = {
  XC_GGA_X_VMT84_PBE,
  XC_EXCHANGE,
  "VMT{8,4} with constraint satisfaction with mu = mu_PBE",
  XC_FAMILY_GGA,
  "A Vela, JC Pacheco-Kato, JL Gazquez, JM del Campo, and SB Trickey, J. Chem. Phys. 136, 144115 (2012)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_vmt_init, 
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_vmt84_ge) = {
  XC_GGA_X_VMT84_PBE,
  XC_EXCHANGE,
  "VMT{8,4} with constraint satisfaction with mu = mu_GE",
  XC_FAMILY_GGA,
  "A Vela, JC Pacheco-Kato, JL Gazquez, JM del Campo, and SB Trickey, J. Chem. Phys. 136, 144115 (2012)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_vmt_init, 
  NULL, NULL,
  work_gga_x
};
