/*
 Copyright (C) 2008 Lara Ferrigni, Georg Madsen, M.A.L. Marques

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

/************************************************************************
 Implements Zhao, Truhlar
   Meta-gga M06-Local

  Exchange part
************************************************************************/

#define XC_MGGA_X_M06L          203 /* Zhao, Truhlar exchange */

static FLOAT CFermi;

static void
mgga_x_m06l_init(void *p_)
{
  XC(mgga_type) *p = (XC(mgga_type) *)p_;

  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(sizeof(XC(func_type) *)*p->n_func_aux);
  p->func_aux[0] = (XC(func_type) *)  malloc(sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_GGA_X_PBE, p->nspin);

  CFermi = (3.0/5.0) * POW(6.0*M_PI*M_PI, 2.0/3.0);
}


/* Eq. (8) */
static void 
x_m06l_fw(int order, FLOAT t, FLOAT *fw, FLOAT *dfwdt)
{
  /*define the parameters for fw of Eq. (8) as in the reference paper*/
  static FLOAT a[12] = {
    0.3987756, 0.2548219, 0.3923994, -2.103655, -6.302147, 10.97615,
    30.97273,  -23.18489, -56.73480, 21.60364, 34.21814, -9.049762};

  FLOAT w, wp;
  int i;

  w = (CFermi - 2.0*t)/(CFermi + 2.0*t);

  *fw = 0.0;
  if(order>0) *dfwdt = 0.0;
  wp = 1.0;
  for(i=0; i<12; i++){
    *fw += a[i]*wp;
    if(order>0) *dfwdt += ((FLOAT)i)*a[i]*wp;

    wp *= w;
  }
  if (order>0){
    *dfwdt /= w;
    *dfwdt *= -4.0*CFermi/((CFermi + 2.0*t)*(CFermi + 2.0*t));
  }
}


static void 
func(const XC(mgga_type) *pt, XC(work_mgga_x_params) *r)
{
  const FLOAT d[6] = {0.6012244, 0.004748822, -0.008635108, -0.000009308062, 0.00004482811, 0.0};
  const FLOAT alpha = 0.00186726;   /* set alpha of Eq. (4) */

  FLOAT f_pbe, dfdx_pbe;
  FLOAT h, dhdx, dhdz, fw, dfwdt;

  XC(gga_x_pbe_enhance)(pt->func_aux[0]->gga, r->x, r->order, &f_pbe, &dfdx_pbe, NULL);

  x_m06l_fw(r->order, r->t, &fw, &dfwdt);

  /* there is a factor if 2 in the definition of z, as in Theor. Chem. Account 120, 215 (2008) */
  XC(mgga_x_gvt4_func)(r->order, r->x, 2.0*r->t - CFermi, alpha, d, &h, &dhdx, &dhdz);

  /* A MINUS was missing in Eq. (7) of the paper */
  r->f = f_pbe*fw + h;

  if(r->order < 1) return;

  r->dfdx = dfdx_pbe*fw + dhdx;
  r->dfdt = f_pbe*dfwdt + 2.0*dhdz;
}


#include "work_mgga_x.c"

const XC(func_info_type) XC(func_info_mgga_x_m06l) = {
  XC_MGGA_X_M06L,
  XC_EXCHANGE,
  "M06-L",
  XC_FAMILY_MGGA,
  "Y Zhao and DG Truhlar, JCP 125, 194101 (2006)\n"
  "Y Zhao and DG Truhlar, Theor. Chem. Account 120, 215 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_x_m06l_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_x,
};
