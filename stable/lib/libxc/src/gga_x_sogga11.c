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

#define XC_GGA_X_SOGGA11        151 /* Second-order generalized gradient approximation 2011 */
#define XC_HYB_GGA_X_SOGGA11_X  426 /* Hybrid based on SOGGA11 form */

static void 
gga_x_sogga11_init(void *p_)
{
  XC(gga_type) *p = (XC(gga_type) *)p_;

  switch(p->info->number){
  case XC_GGA_X_SOGGA11:
    p->func = 0;
    break;
  case XC_HYB_GGA_X_SOGGA11_X:
    p->func = 1;
    p->exx_coef = 0.4015;
    break;
  default:
    fprintf(stderr, "Internal error in gga_x_sogga11\n");
    exit(1);
  }
}

void XC(gga_x_sogga11_enhance)
  (const XC(gga_type) *p, int order, FLOAT x, 
   FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  const FLOAT kappa = 0.552;
  const FLOAT mu = 10.0/81.0;
  const FLOAT alpha = mu*X2S*X2S/kappa;
  const FLOAT aa[][6] = {
    {0.50000, -2.95535,  15.7974, -91.1804,  96.2030, 0.18683},   /* SOGGA11   */
    {0.50000,  5.37406, -5.94160,  12.7962, -18.8521, 8.78551}    /* SOGGA11-X */
  };
  const FLOAT bb[][6] = {
    {0.50000,  3.50743, -12.9523,  49.7870, -33.2545, -11.1396},  /* SOGGA11   */
    {0.50000, -4.82197,   5.40713, -4.10014, -6.27393,  6.62678}  /* SOGGA11-X */
  };
    
  FLOAT f0, df0, d2f0, den0, den1, t0, t1, f1, df1, d2f1;
  FLOAT f0_2, f0_3, f0_4, f0_5;

  den0 = -1.0/(1.0 + alpha*x*x);
  f0   =  1.0 + den0;
  den1 = -exp(-alpha*x*x);
  f1   =  1.0 + den1;

  *f = aa[p->func][0] + f0*(aa[p->func][1] + f0*(aa[p->func][2] + f0*(aa[p->func][3] + f0*(aa[p->func][4] + f0*aa[p->func][5]))))
    +  bb[p->func][0] + f1*(bb[p->func][1] + f1*(bb[p->func][2] + f1*(bb[p->func][3] + f1*(bb[p->func][4] + f1*bb[p->func][5]))));

  if(order < 1) return;

  df0 =  2.0*alpha*x*den0*den0;
  df1 = -2.0*alpha*x*den1;

  t0  = aa[p->func][1] + f0*(2.0*aa[p->func][2] + f0*(3.0*aa[p->func][3] + f0*(4.0*aa[p->func][4] + f0*5.0*aa[p->func][5])));
  t1  = bb[p->func][1] + f1*(2.0*bb[p->func][2] + f1*(3.0*bb[p->func][3] + f1*(4.0*bb[p->func][4] + f1*5.0*bb[p->func][5])));

  *dfdx = df0*t0 + df1*t1;

  if(order < 2) return;

  d2f0 = 2.0*alpha*(3.0*alpha*x*x - 1.0)*den0*den0*den0;
  d2f1 = 2.0*alpha*(2.0*alpha*x*x - 1.0)*den1;

  *d2fdx2 = d2f0*t0 + d2f1*t1 +
    df0*df0*(2.0*aa[p->func][2] + f0*(6.0*aa[p->func][3] + f0*(12.0*aa[p->func][4] + f0*20.0*aa[p->func][5]))) +
    df1*df1*(2.0*bb[p->func][2] + f1*(6.0*bb[p->func][3] + f1*(12.0*bb[p->func][4] + f1*20.0*bb[p->func][5])));
}


#define func XC(gga_x_sogga11_enhance)
#include "work_gga_x.c"


const XC(func_info_type) XC(func_info_gga_x_sogga11) = {
  XC_GGA_X_SOGGA11,
  XC_EXCHANGE,
  "Second-order generalized gradient approximation 2011",
  XC_FAMILY_GGA,
  "R Peverati, Y Zhao, and DG Truhlar, J. Phys. Chem. Lett. 2, 1911-1997 (2011)\n"
  "http://comp.chem.umn.edu/mfm/index.html",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_x_sogga11_init, 
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_hyb_gga_x_sogga11_x) = {
  XC_HYB_GGA_X_SOGGA11_X,
  XC_EXCHANGE,
  "Hybrid based on SOGGA11 form",
  XC_FAMILY_HYB_GGA,
  "R Peverati and DG Truhlar, J. Chem. Phys. 135, 191102 (2011)\n"
  "http://comp.chem.umn.edu/mfm/index.html",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_x_sogga11_init, 
  NULL, NULL,
  work_gga_x
};
