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

#define XC_GGA_K_DK          516 /* DePristo and Kress                    */
#define XC_GGA_K_PERDEW      517 /* Perdew                                */
#define XC_GGA_K_VSK         518 /* Vitos, Skriver, and Kollar            */
#define XC_GGA_K_VJKS        519 /* Vitos, Johansson, Kollar, and Skriver */
#define XC_GGA_K_ERNZERHOF   520 /* Ernzerhof */

typedef struct{
  FLOAT aa[5], bb[5];
} gga_k_dk_params;

static void 
gga_k_dk_init(XC(func_type) *p)
{
  int i;
  FLOAT ff, *aa, *bb;

  assert(p->params == NULL);
  p->params = malloc(sizeof(gga_k_dk_params));

  /* shortcuts for a and b */
  aa  = ((gga_k_dk_params *) (p->params))->aa;
  bb  = ((gga_k_dk_params *) (p->params))->bb;

  /* initialize parameters to zero */
  for(i=0; i<5; i++){
    aa[i] = 0.0;
    bb[i] = 0.0;
  }

  switch(p->info->number){
  case XC_GGA_K_DK:
    ff = 5.0*X2S*X2S/27.0; /* = t2/t0 = 1.0/(72.0*K_FACTOR_C) */

    bb[0] =  1.0;
    bb[1] = -0.05   *ff;
    bb[2] =  9.99802*(ff*ff);
    bb[3] =  2.96085*(ff*ff*ff);

    aa[0] =   1.0;
    aa[1] =   0.95   *ff;
    aa[2] =  14.28111*(ff*ff);
    aa[3] = -19.57962*(ff*ff*ff);
    aa[4] =   9.0*bb[3]*ff;

    break;

  case XC_GGA_K_PERDEW:
    ff = X2S*X2S;

    bb[0] =  1.0;
    bb[1] = 88.3960*ff;
    bb[2] = 16.3683*(ff*ff);

    aa[0] =   1.0;
    aa[1] =  88.2108*ff;

    break;

  case XC_GGA_K_VSK:
    ff = 5.0*X2S*X2S/27.0; /* = t2/t0 = 1.0/(72.0*K_FACTOR_C) */

    bb[0] =  1.0;
    bb[1] = -0.05     *ff;
    bb[2] =  0.396    *(ff*ff);

    aa[0] =  1.0;
    aa[1] =  0.95     *ff;
    aa[3] =  9.0*bb[2]*ff;

    break;

  case XC_GGA_K_VJKS:
    ff = X2S*X2S;

    bb[0] =  1.0;
    bb[1] =  0.6511 *ff;
    bb[2] =  0.0431 *(ff*ff);

    aa[0] =  1.0;
    aa[1] =  0.8944 *ff;
    aa[3] = -bb[2]  *ff;

    break;

  case XC_GGA_K_ERNZERHOF:
    ff = X2S*X2S;

    bb[0] =  135.0;
    bb[1] =    3.0*ff;

    aa[0] =  135.0;
    aa[1] =   28.0*ff;
    aa[2] =    5.0*(ff*ff);

    break;
  }
}


static inline void 
func(const XC(func_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  FLOAT xx2, xx4, num, denom, dnum, ddenom, d2num, d2denom;
  FLOAT *aa, *bb;

  assert(p->params != NULL);
  aa  = ((gga_k_dk_params *) (p->params))->aa;
  bb  = ((gga_k_dk_params *) (p->params))->bb;

  xx2 = x*x;
  xx4 = xx2*xx2;

  num   = aa[0] + aa[1]*xx2 + aa[2]*xx4 + aa[3]*xx2*xx4 + aa[4]*xx4*xx4;
  denom = bb[0] + bb[1]*xx2 + bb[2]*xx4 + bb[3]*xx2*xx4 + bb[4]*xx4*xx4;

  *f = num/denom;

  if(order < 1) return;

  dnum   = 2.0*aa[1]*x + 4.0*aa[2]*x*xx2 + 6.0*aa[3]*x*xx4 + 8.0*aa[4]*x*xx2*xx4;
  ddenom = 2.0*bb[1]*x + 4.0*bb[2]*x*xx2 + 6.0*bb[3]*x*xx4 + 8.0*bb[4]*x*xx2*xx4;

  *dfdx  = (dnum*denom - num*ddenom)/(denom*denom);
  
  if(order < 2) return;

  d2num   = 2.0*aa[1] + 4.0*3.0*aa[2]*xx2 + 6.0*5.0*aa[3]*xx4 + 8.0*7.0*aa[4]*xx2*xx4;
  d2denom = 2.0*bb[1] + 4.0*3.0*bb[2]*xx2 + 6.0*5.0*bb[3]*xx4 + 8.0*7.0*bb[4]*xx2*xx4;

  *d2fdx2  = ((d2num*denom - num*d2denom)*denom - 2.0*ddenom*(dnum*denom - ddenom*num))/(denom*denom*denom);
}

#define XC_KINETIC_FUNCTIONAL
#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_k_dk) = {
  XC_GGA_K_DK,
  XC_KINETIC,
  "DePristo and Kress",
  XC_FAMILY_GGA,
  "AE DePristo and JD Kress, Phys. Rev. A 35, 438-441 (1987)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_k_dk_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_perdew) = {
  XC_GGA_K_PERDEW,
  XC_KINETIC,
  "Perdew",
  XC_FAMILY_GGA,
  "JP Perdew, Phys. Lett. A 165, 79 (1992)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_k_dk_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_vsk) = {
  XC_GGA_K_VSK,
  XC_KINETIC,
  "Vitos, Skriver, and Kollar",
  XC_FAMILY_GGA,
  "L Vitos, HL Skriver, and J. Kollár, Phys. Rev. B 57, 12611-12615 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_k_dk_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_vjks) = {
  XC_GGA_K_VJKS,
  XC_KINETIC,
  "Vitos, Johansson, Kollar, and Skriver",
  XC_FAMILY_GGA,
  "L Vitos, B Johansson, J. Kollár, and HL Skriver, Phys. Rev. A 61, 052511 (2000)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_k_dk_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_ernzerhof) = {
  XC_GGA_K_ERNZERHOF,
  XC_KINETIC,
  "Ernzerhof",
  XC_FAMILY_GGA,
  "M Ernzerhof, J. Mol. Struct.:THEOCHEM 501-502, 59 (2000)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_k_dk_init,
  NULL, NULL,
  work_gga_k
};
