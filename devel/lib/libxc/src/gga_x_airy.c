/*
 Copyright (C) 2008 Georg Madsen

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

#define XC_GGA_X_AIRY  192 /* Constantin et al based on the Airy gas */
#define XC_GGA_X_LAG   193 /* Local Airy Gas */

static void 
gga_x_airy_init(XC(func_type) *p)
{
  switch(p->info->number){
  case XC_GGA_X_AIRY:       p->func = 0;  break;
  case XC_GGA_X_LAG:        p->func = 1;  break;
  default:
    fprintf(stderr, "Internal error in gga_x_airy\n");
    exit(1);
  }
}


static inline void 
func(const XC(func_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  static FLOAT 
    a1  =   0.041106, 
    a2  =   2.626712, 
    a3  =   0.092070, 
    a4  =   0.657946, 
    a5  = 133.983631, 
    a6  =   3.217063, 
    a7  = 136.707378, 
    a8  =   3.223476, 
    a9  =   2.675484, 
    a10 =   3.473804;

  FLOAT ss, ssa2, ssa6, ssa8, ssa10;
  FLOAT   den1,   aux1,   num1,   den2,   num2;
  FLOAT  dden1,  daux1,  dnum1,  dden2,  dnum2;
  FLOAT d2den1, d2aux1, d2num1, d2den2, d2num2;

  ss = X2S*x;
  ssa2  = POW(ss, a2);
  ssa6  = POW(ss, a6);
  ssa8  = POW(ss, a8);
  ssa10 = POW(ss, a10);

  num1 = a1*ssa2;
  aux1 = 1.0 + a3*ssa2;
  den1 = POW(aux1, a4);

  *f = num1/den1;

  if(p->func == 0){
    num2 = 1.0 - a5*ssa6 + a7*ssa8;
    den2 = 1.0 + a9*ssa10;

    *f += num2/den2;
  }

  if(order < 1) return;

  dnum1 = a2*num1/ss;
  daux1 = a3*a2*ssa2/ss;
  dden1 = a4*daux1*den1/aux1;

  *dfdx  = (dnum1*den1 - num1*dden1)/(den1*den1);

  if(p->func == 0){
    dnum2 = -a5*a6*ssa6/ss + a7*a8*ssa8/ss;
    dden2 = a9*a10*ssa10/ss;

    *dfdx += (dnum2*den2 - num2*dden2)/(den2*den2);
  }

  *dfdx *= X2S;

  if(order < 2) return;

  d2num1 = (a2 - 1.0)*dnum1/ss;
  d2aux1 = (a2 - 1.0)*daux1/ss;
  d2den1 = a4*d2aux1*den1/aux1 + (a4 - 1.0)*daux1*dden1/aux1;

  *d2fdx2 = (2.0*num1*dden1*dden1 - 2.0*den1*dden1*dnum1 - den1*num1*d2den1 + den1*den1*d2num1)/(den1*den1*den1);

  if(p->func == 0){
    d2num2 = -a5*a6*(a6 - 1.0)*ssa6/(ss*ss) + a7*a8*(a8 - 1.0)*ssa8/(ss*ss);
    d2den2 = (a10 - 1.0)*dden2/ss;

    *d2fdx2 += (2.0*num2*dden2*dden2 - 2.0*den2*dden2*dnum2 - den2*num2*d2den2 + den2*den2*d2num2)/(den2*den2*den2);
  }
  
  *d2fdx2 *= X2S*X2S;
}

#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_airy) = {
  XC_GGA_X_AIRY,
  XC_EXCHANGE,
  "Constantin et al based on the Airy gas",
  XC_FAMILY_GGA,
  "LA Constantin, A Ruzsinszky, and JP Perdew, Phys. Rev. B 80, 035125 (2009)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_airy_init,
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_lag) = {
  XC_GGA_X_LAG,
  XC_EXCHANGE,
  "Local Airy Gas",
  XC_FAMILY_GGA,
  "L Vitos, B Johansson, J Kollar, and HL Skriver, Phys. Rev. B 62, 10046-10050 (2000)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_airy_init,
  NULL, NULL,
  work_gga_x
};
