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

#define XC_GGA_K_OL2          513 /* Ou-Yang and Levy v.2 */
#define XC_GGA_X_OL2          183 /* Exchange form based on Ou-Yang and Levy v.2 */

static void 
gga_k_ol2_init(XC(func_type) *p)
{
  switch(p->info->number){
  case XC_GGA_K_OL2: p->func = 0; break;
  case XC_GGA_X_OL2: p->func = 1; break;
  }
}

static inline void 
func(const XC(func_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  static const FLOAT aa[2] = {    1.0,            M_CBRT2*0.07064/X_FACTOR_C};
  static const FLOAT bb[2] = {    1.0/K_FACTOR_C, M_CBRT2*0.07064/X_FACTOR_C};
  static const FLOAT cc[2] = {0.00887/K_FACTOR_C, M_CBRT2*M_CBRT2*0.07064*34.0135/X_FACTOR_C};
  FLOAT denom;

  denom = M_CBRT2 + 4.0*x;

  *f = aa[p->func] + bb[p->func]*x*x/72.0 + cc[p->func]*x/denom;

  if(order < 1) return;

  *dfdx = 2.0*bb[p->func]*x/72.0 + cc[p->func]*M_CBRT2/(denom*denom);
  
  if(order < 2) return;

  *d2fdx2 = 2.0*bb[p->func]/72.0 - 8.0*cc[p->func]*M_CBRT2/(denom*denom*denom);
}

#include "work_gga_x.c"
const XC(func_info_type) XC(func_info_gga_x_ol2) = {
  XC_GGA_X_OL2,
  XC_EXCHANGE,
  "Exchange form based on Ou-Yang and Levy v.2",
  XC_FAMILY_GGA,
  "P Fuentealba and O Reyes, Chem. Phys. Lett. 232, 31-34 (1995)\n"
  "H Ou-Yang, M Levy, Int. J. of Quant. Chem. 40, 379-388 (1991)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_k_ol2_init,
  NULL, NULL,
  work_gga_x
};


#define XC_KINETIC_FUNCTIONAL
#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_k_ol2) = {
  XC_GGA_K_OL2,
  XC_KINETIC,
  "Ou-Yang and Levy v.2",
  XC_FAMILY_GGA,
  "H Ou-Yang, M Levy, Int. J. of Quant. Chem. 40, 379-388 (1991)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_k_ol2_init, 
  NULL, NULL,
  work_gga_k
};
