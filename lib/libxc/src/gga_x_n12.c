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

#define XC_GGA_X_N12          82 /* N12 functional from Minnesota    */
#define XC_GGA_X_N12_SX       81 /* N12-SX functional from Minnesota */

static const FLOAT CC_N12[4][4] = {
  { 1.00000e+00,  5.07880e-01,  1.68233e-01,  1.28887e-01},
  { 8.60211e-02, -1.71008e+01,  6.50814e+01, -7.01726e+01},
  {-3.90755e-01,  5.13392e+01, -1.66220e+02,  1.42738e+02},
  { 4.03611e-01, -3.44631e+01,  7.61661e+01, -2.41834e+00}
};

static const FLOAT CC_N12_SX[4][4] = {
  { 6.81116e-01,  1.88858e+00,  1.78590e+00,  8.79456e-01},
  {-8.12270e-02, -1.08723e+00, -4.18682e+00, -3.00000e+01},
  { 5.36236e-01, -5.45678e+00,  3.00000e+01,  5.51105e+01},
  {-7.09913e-01,  1.30001e+01, -7.24877e+01,  2.98363e+01}
};

static void 
func(const XC(func_type) *pt, XC(gga_work_c_t) *r)
{
  int is;
  const FLOAT (*CC)[4];
  const FLOAT sign[2] = {1.0, -1.0}, omega_x=2.5, gamma_x=0.004;

  FLOAT cnst_rs, opz, opz13, rss, x2;
  FLOAT vx, vx2, vx3, ux_d, ux, ux2, ux3;
  FLOAT pol1, pol2, pol3, pol4;
  FLOAT ex, FN12;

  FLOAT drssdrs, drssdz, dvxdrss, duxdxs;
  FLOAT dpol1, dpol2, dpol3, dpol4;
  FLOAT dexdz, dexdrss, dFN12dux, dFN12dvx;

  CC = (pt->info->number == XC_GGA_X_N12) ? CC_N12 : CC_N12_SX;
  cnst_rs = CBRT(4.0*M_PI/3.0);

  r->f = 0.0;
  if(r->order >= 1)
    r->dfdrs = r->dfdz = r->dfdxt = r->dfdxs[0] = r->dfdxs[1] = 0.0;

  /* now the spin-resolved part */
  for(is = 0; is < 2; is++){
    opz   = 1.0 + sign[is]*r->zeta;
    if(opz < pt->info->min_zeta) continue;

    opz13 = CBRT(opz);
    rss   = r->rs*M_CBRT2/opz13;
    x2    = r->xs[is]*r->xs[is];

    vx    = 1.0/(1.0 + (cnst_rs/omega_x)*rss);

    ux_d  = 1.0/(1.0 + gamma_x*x2);
    ux    = gamma_x*x2*ux_d;

    vx2 = vx*vx; vx3 = vx2*vx;
    ux2 = ux*ux; ux3 = ux2*ux;

    pol1 = CC[0][0] + CC[0][1]*ux + CC[0][2]*ux2 + CC[0][3]*ux3;
    pol2 = CC[1][0] + CC[1][1]*ux + CC[1][2]*ux2 + CC[1][3]*ux3;
    pol3 = CC[2][0] + CC[2][1]*ux + CC[2][2]*ux2 + CC[2][3]*ux3;
    pol4 = CC[3][0] + CC[3][1]*ux + CC[3][2]*ux2 + CC[3][3]*ux3;

    FN12 = pol1 + vx*pol2 + vx2*pol3 + vx3*pol4;

    ex    = -X_FACTOR_C*opz/(2.0*cnst_rs*rss);
    r->f += ex*FN12;

    if(r->order < 1) continue;

    drssdrs = M_CBRT2/opz13;
    drssdz  = -sign[is]*rss/(3.0*opz);

    dvxdrss = -(cnst_rs/omega_x)*vx*vx;
    duxdxs  = 2.0*gamma_x*r->xs[is]*ux_d*ux_d;

    dpol1 = CC[0][1] + 2.0*CC[0][2]*ux + 3.0*CC[0][3]*ux2;
    dpol2 = CC[1][1] + 2.0*CC[1][2]*ux + 3.0*CC[1][3]*ux2;
    dpol3 = CC[2][1] + 2.0*CC[2][2]*ux + 3.0*CC[2][3]*ux2;
    dpol4 = CC[3][1] + 2.0*CC[3][2]*ux + 3.0*CC[3][3]*ux2;

    dFN12dux = dpol1 + vx*dpol2 + vx2*dpol3 + vx3*dpol4;
    dFN12dvx = pol2 + 2.0*vx*pol3 + 3.0*vx2*pol4;

    dexdrss = -ex/rss;
    dexdz   = sign[is]*ex/opz;

    r->dfdrs    += (dexdrss*FN12 + ex*dFN12dvx*dvxdrss)*drssdrs;
    r->dfdz     += dexdz*FN12 + (dexdrss*FN12 + ex*dFN12dvx*dvxdrss)*drssdz;
    r->dfdxs[is] = ex*dFN12dux*duxdxs;
  }
}


#include "work_gga_c.c"


XC(func_info_type) XC(func_info_gga_x_n12) = {
  XC_GGA_X_N12,
  XC_EXCHANGE,
  "N12 functional of Minnesota",
  XC_FAMILY_GGA,
  "R Peverati and DG Truhlar, J. Chem. Theory Comput. 8, 2310-2319 (2012)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  NULL,
  NULL, NULL,
  work_gga_c,
};

XC(func_info_type) XC(func_info_gga_x_n12_sx) = {
  XC_GGA_X_N12_SX,
  XC_EXCHANGE,
  "N12-SX functional of Minnesota",
  XC_FAMILY_GGA,
  "R Peverati and DG Truhlar, Phys. Chem. Chem. Phys. 14, 16187-16191 (2012)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  NULL,
  NULL, NULL,
  work_gga_c,
};
