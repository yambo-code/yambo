/*
 Copyright (C) 2006-2008 M.A.L. Marques

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

#include <stdlib.h>
#include <assert.h>
#include "util.h"

/* Local tau approximation */

#define XC_MGGA_X_MK00          230 /* Exchange for accurate virtual orbital energies */
#define XC_MGGA_X_MK00B         243 /* Exchange for accurate virtual orbital energies (v. B) */

static void 
func(const XC(func_type) *pt, XC(mgga_work_x_t) *r)
{
  FLOAT cnst, den, den2, den3;

  cnst = 3.0*M_PI/X_FACTOR_C;

  den  = 2.0*r->t - r->u/4.0;

  if(ABS(den) < pt->info->min_tau){
    r->f = 0.0;
    if(r->order >= 1) r->dfdt = r->dfdu = 0.0;
    if(r->order >= 2) r->d2fdt2 = r->d2fdtu = r->d2fdu2 = 0.0;
    return;
  }

  r->f = cnst/den;

  if(r->order < 1) return;
  
  den2 = den*den;

  r->dfdx = 0.0;
  r->dfdt = -2.0*cnst/den2;
  r->dfdu = cnst/(4.0*den2);

  if(r->order < 2) return;
  
  den3 = den2*den;

  r->d2fdx2 =  0.0;
  r->d2fdxt =  0.0;
  r->d2fdxu =  0.0;
  r->d2fdt2 =  8.0*cnst/den3;
  r->d2fdtu = -cnst/den3;
  r->d2fdu2 =  cnst/(8.0*den3);
}

#include "work_mgga_x.c"

const XC(func_info_type) XC(func_info_mgga_x_mk00) = {
  XC_MGGA_X_MK00,
  XC_EXCHANGE,
  "Exchange for accurate virtual orbital energies",
  XC_FAMILY_MGGA,
  "FR Manby and PJ Knowles, J. Chem. Phys. 112, 7002 (2000)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  NULL, NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_x,
};


static void
mgga_x_mk00b_init(XC(func_type) *p)
{
  static int   funcs_id  [2] = {XC_GGA_X_B88, XC_MGGA_X_MK00};
  static FLOAT funcs_coef[2] = {1.0, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);  

  XC(gga_x_b88_set_params)(p->func_aux[0], 0.0016, 6.0);
}

const XC(func_info_type) XC(func_info_mgga_x_mk00b) = {
  XC_MGGA_X_MK00B,
  XC_EXCHANGE,
  "Exchange for accurate virtual orbital energies (v. B)",
  XC_FAMILY_MGGA,
  "FR Manby and PJ Knowles, J. Chem. Phys. 112, 7002 (2000)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_x_mk00b_init,
  NULL, NULL, NULL, NULL,
};
