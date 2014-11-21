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

#define XC_MGGA_X_MN12_L        227 /* MN12-L functional from Minnesota  */
#define XC_MGGA_X_MN12_SX       228 /* MN12-SX functional from Minnesota */

/* the ordering is 
CC000 [ 0], CC001 [ 1], CC002 [ 2], CC003 [ 3], CC004 [ 4], CC005 [ 5]
CC010 [ 6], CC011 [ 7], CC012 [ 8], CC013 [ 9], CC014 [10],
CC020 [11], CC021 [12], CC022 [13], CC023 [14],
CC030 [15], CC031 [16], CC032 [17],
CC100 [18], CC101 [19], CC102 [20], CC103 [21], CC104 [22]
CC110 [23], CC111 [24], CC112 [25], CC113 [26],
CC120 [27], CC121 [28], CC122 [29],
CC200 [30], CC201 [31], CC202 [32], CC203 [33],
CC210 [34], CC211 [35], CC212 [36],
CC300 [37], CC301 [38], CC302 [39]
*/
static const FLOAT CC_MN12_L[] =
  { 6.735981e-01, -2.270598e+00, -2.613712e+00,  3.993609e+00,  4.635575e+00, 1.250676e+00,
    8.444920e-01, -1.301173e+01, -1.777730e+01, -4.627211e+00,  5.976605e+00,
    1.142897e+00, -2.040226e+01, -2.382843e+01,  7.119109e+00,
   -2.335726e+01, -1.622633e+01,  1.482732e+01,
    1.449285e+00,  1.020598e+01,  4.407450e+00, -2.008193e+01, -1.253561e+01,
   -5.435031e+00,  1.656736e+01,  2.000229e+01, -2.513105e+00,
    9.658436e+00, -3.825281e+00, -2.500000e+01,
   -2.070080e+00, -9.951913e+00,  8.731211e-01,  2.210891e+01,
    8.822633e+00,  2.499949e+01,  2.500000e+01,
    6.851693e-01, -7.406948e-02, -6.788000e-01
  };

static const FLOAT CC_MN12_SX[] =
  { 5.226556e-01, -2.681208e-01, -4.670705e+00,  3.067320e+00,  4.095370e+00,  2.653023e+00,
    5.165969e-01, -2.035442e+01, -9.946472e+00,  2.938637e+00,  1.131100e+01,
    4.752452e+00, -3.061331e+00, -2.523173e+01,  1.710903e+01,
   -2.357480e+01, -2.727754e+01,  1.603291e+01,  
    1.842503e+00,  1.927120e+00,  1.107987e+01, -1.182087e+01, -1.117768e+01,
   -5.821000e+00,  2.266545e+01,  8.246708e+00, -4.778364e+00,
    5.329122e-01, -6.666755e+00,  1.671429e+00,
   -3.311409e+00,  3.415913e-01, -6.413076e+00,  1.038584e+01,
    9.026277e+00,  1.929689e+01,  2.669232e+01,
    1.517278e+00, -3.442503e+00,  1.100161e+00
  };


static void 
func(const XC(func_type) *pt, XC(mgga_work_c_t) *r)
{
  int is;
  const FLOAT *CC, sign[2] = {1.0, -1.0}, omega_x=2.5, gamma_x=0.004;

  FLOAT cnst_rs, opz, opz13, rss, x2;
  FLOAT vx, vx2, vx3, ux_d, ux, ux2, ux3, wx_d, wx, wx2, wx3, wx4, wx5;
  FLOAT pol1, pol2, pol3, pol4, pol5, pol6, pol7, pol8, pol9, pol10;
  FLOAT ex, FMN12;

  FLOAT drssdrs, drssdz, dvxdrss, duxdxs, dwxdts;
  FLOAT dpol1, dpol2, dpol3, dpol4, dpol5, dpol6, dpol7, dpol8, dpol9, dpol10;
  FLOAT dexdz, dexdrss, dFMN12dwx, dFMN12dux, dFMN12dvx;

  CC = (pt->info->number == XC_MGGA_X_MN12_L) ? CC_MN12_L : CC_MN12_SX;
  cnst_rs = CBRT(4.0*M_PI/3.0);

  r->f = 0.0;
  if(r->order >= 1)
    r->dfdrs = r->dfdz = r->dfdxt = r->dfdxs[0] = r->dfdxs[1] = r->dfdts[0] = r->dfdts[1] = 0.0;

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

    wx_d  = 1.0/(K_FACTOR_C + r->ts[is]);
    wx    = (K_FACTOR_C - r->ts[is])*wx_d;

    vx2 = vx*vx; vx3 = vx2*vx;
    ux2 = ux*ux; ux3 = ux2*ux;
    wx2 = wx*wx; wx3 = wx2*wx; wx4 = wx3*wx; wx5 = wx4*wx;

    pol1  = CC[ 0] + CC[ 1]*wx + CC[ 2]*wx2 + CC[ 3]*wx3 + CC[ 4]*wx4 + CC[ 5]*wx5;
    pol2  = CC[ 6] + CC[ 7]*wx + CC[ 8]*wx2 + CC[ 9]*wx3 + CC[10]*wx4;
    pol3  = CC[11] + CC[12]*wx + CC[13]*wx2 + CC[14]*wx3;
    pol4  = CC[15] + CC[16]*wx + CC[17]*wx2;
    pol5  = CC[18] + CC[19]*wx + CC[20]*wx2 + CC[21]*wx3 + CC[22]*wx4;
    pol6  = CC[23] + CC[24]*wx + CC[25]*wx2 + CC[26]*wx3;
    pol7  = CC[27] + CC[28]*wx + CC[29]*wx2;
    pol8  = CC[30] + CC[31]*wx + CC[32]*wx2 + CC[33]*wx3;
    pol9  = CC[34] + CC[35]*wx + CC[36]*wx2;
    pol10 = CC[37] + CC[38]*wx + CC[39]*wx2;

    FMN12 = pol1 + ux*pol2 + ux2*pol3 + ux3*pol4 +
      vx*pol5 + ux*vx*pol6 + ux2*vx*pol7 + vx2*pol8 + ux*vx2*pol9 + vx3*pol10;

    ex    = -X_FACTOR_C*opz/(2.0*cnst_rs*rss);
    r->f += ex*FMN12;
    
    if(r->order < 1) continue;

    drssdrs = M_CBRT2/opz13;
    drssdz  = -sign[is]*rss/(3.0*opz);

    dvxdrss = -(cnst_rs/omega_x)*vx*vx;
    duxdxs  = 2.0*gamma_x*r->xs[is]*ux_d*ux_d;
    dwxdts  = -2.0*K_FACTOR_C*wx_d*wx_d;

    dpol1 =  CC[ 1] + 2.0*CC[ 2]*wx + 3.0*CC[ 3]*wx2 + 4.0*CC[ 4]*wx3 + 5.0*CC[ 5]*wx4;
    dpol2  = CC[ 7] + 2.0*CC[ 8]*wx + 3.0*CC[ 9]*wx2 + 4.0*CC[10]*wx3;
    dpol3  = CC[12] + 2.0*CC[13]*wx + 3.0*CC[14]*wx2;
    dpol4  = CC[16] + 2.0*CC[17]*wx;
    dpol5  = CC[19] + 2.0*CC[20]*wx + 3.0*CC[21]*wx2 + 4.0*CC[22]*wx3;
    dpol6  = CC[24] + 2.0*CC[25]*wx + 3.0*CC[26]*wx2;
    dpol7  = CC[28] + 2.0*CC[29]*wx;
    dpol8  = CC[31] + 2.0*CC[32]*wx + 3.0*CC[33]*wx2;
    dpol9  = CC[35] + 2.0*CC[36]*wx;
    dpol10 = CC[38] + 2.0*CC[39]*wx;

    dFMN12dwx = dpol1 + ux*dpol2 + ux2*dpol3 + ux3*dpol4 +
      vx*dpol5 + ux*vx*dpol6 + ux2*vx*dpol7 + vx2*dpol8 + ux*vx2*dpol9 + vx3*dpol10;
    dFMN12dux = pol2 + 2.0*ux*pol3 + 3.0*ux2*pol4 + vx*pol6 + 2.0*ux*vx*pol7 + vx2*pol9;
    dFMN12dvx = pol5 + ux*pol6 + ux2*pol7 + 2.0*vx*pol8 + 2.0*ux*vx*pol9 + 3.0*vx2*pol10;

    dexdrss = -ex/rss;
    dexdz   = sign[is]*ex/opz;

    r->dfdrs    += (dexdrss*FMN12 + ex*dFMN12dvx*dvxdrss)*drssdrs;
    r->dfdz     += dexdz*FMN12 + (dexdrss*FMN12 + ex*dFMN12dvx*dvxdrss)*drssdz;
    r->dfdxs[is] = ex*dFMN12dux*duxdxs;
    r->dfdts[is] = ex*dFMN12dwx*dwxdts;
  }
}


#include "work_mgga_c.c"


XC(func_info_type) XC(func_info_mgga_x_mn12_l) = {
  XC_MGGA_X_MN12_L,
  XC_EXCHANGE,
  "MN12-L functional of Minnesota",
  XC_FAMILY_MGGA,
  "R Peverati and DG Truhlar, Phys. Chem. Chem. Phys. 14, 13171-13174 (2012)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  NULL,
  NULL, NULL, NULL,
  work_mgga_c,
};

XC(func_info_type) XC(func_info_mgga_x_mn12_sx) = {
  XC_MGGA_X_MN12_SX,
  XC_EXCHANGE,
  "MN12-SX functional of Minnesota",
  XC_FAMILY_MGGA,
  "R Peverati and DG Truhlar, Phys. Chem. Chem. Phys. 14, accepted (2012)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  NULL,
  NULL, NULL, NULL,
  work_mgga_c,
};
