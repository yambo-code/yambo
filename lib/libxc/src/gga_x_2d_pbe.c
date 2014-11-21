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

#define XC_GGA_X_2D_PBE          129 /* Perdew, Burke & Ernzerhof exchange in 2D          */

static inline void 
func(const XC(func_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  //FILE *fin;
  static const FLOAT kappa[1] = {
    0.4604,  /* original PBE */
  };

  FLOAT mu[1] = {
    0.354546875,
  };

  FLOAT ss, f0, df0, d2f0;
  int func;

  switch(p->info->number){
  default:                func = 0; /* original PBE */
  }

  //fin = fopen("gga_x_2d_b88_params", "r");
  //fscanf(fin, "%lf", &mu[0]);
  //fclose(fin);

  ss = X2S_2D*x;

  f0 = kappa[func] + mu[func]*ss*ss;
  *f = 1.0 + kappa[func]*(1.0 - kappa[func]/f0);

  if(order < 1) return;

  df0 = 2.0*ss*mu[func];

  *dfdx  = X2S_2D*kappa[func]*kappa[func]*df0/(f0*f0);

  if(order < 2) return;

  d2f0 = 2.0*mu[func];
  *d2fdx2 = X2S_2D*X2S_2D*kappa[func]*kappa[func]/(f0*f0)*(d2f0 - 2.0*df0*df0/f0);
}

#define XC_DIMENSIONS 2
#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_2d_pbe) = {
  XC_GGA_X_2D_PBE,
  XC_EXCHANGE,
  "Perdew, Burke & Ernzerhof in 2D",
  XC_FAMILY_GGA,
  "G Vilhena and MAL Marques, unpublished\n"
  "JP Perdew, K Burke, and M Ernzerhof, Phys. Rev. Lett. 77, 3865 (1996)\n"
  "JP Perdew, K Burke, and M Ernzerhof, Phys. Rev. Lett. 78, 1396(E) (1997)",
  XC_FLAGS_2D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  NULL, NULL, NULL,
  work_gga_x
};
