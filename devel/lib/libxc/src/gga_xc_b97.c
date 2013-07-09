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
#include <stdlib.h>
#include "util.h"

#define XC_GGA_XC_HCTH_93  161 /* HCTH functional fitted to  93 molecules  */
#define XC_GGA_XC_HCTH_120 162 /* HCTH functional fitted to 120 molecules  */
#define XC_GGA_XC_HCTH_147 163 /* HCTH functional fitted to 147 molecules  */
#define XC_GGA_XC_HCTH_407 164 /* HCTH functional fitted to 147 molecules  */
#define XC_GGA_XC_B97      167 /* Becke 97                                 */
#define XC_GGA_XC_B97_1    168 /* Becke 97-1                               */
#define XC_GGA_XC_B97_2    169 /* Becke 97-2                               */
#define XC_GGA_XC_B97_D    170 /* Grimme functional to be used with C6 vdW term */
#define XC_GGA_XC_B97_K    171 /* Boese-Martin for Kinetics                */
#define XC_GGA_XC_B97_3    172 /* Becke 97-3                               */
#define XC_GGA_XC_SB98_1a  176 /* Schmider-Becke 98 parameterization 1a    */
#define XC_GGA_XC_SB98_1b  177 /* Schmider-Becke 98 parameterization 1b    */
#define XC_GGA_XC_SB98_1c  178 /* Schmider-Becke 98 parameterization 1c    */
#define XC_GGA_XC_SB98_2a  179 /* Schmider-Becke 98 parameterization 2a    */
#define XC_GGA_XC_SB98_2b  180 /* Schmider-Becke 98 parameterization 2b    */
#define XC_GGA_XC_SB98_2c  181 /* Schmider-Becke 98 parameterization 2c    */

static void 
func_g(const XC(gga_type) *p, int type, FLOAT s, int order, FLOAT *g, FLOAT *dgds, FLOAT *d2gds2)
{
  static const FLOAT c[][3][5] = {
    {      /* HCTH/93 */
      {1.09320,  -0.744056,    5.59920,   -6.78549,   4.49357}, /* X   */
      {0.222601, -0.0338622,  -0.0125170, -0.802496,  1.55396}, /* Css */
      {0.729974,  3.35287,   -11.5430,     8.08564,  -4.47857}  /* Cab */
    }, {   /* HCTH/120 */
      {1.09163,  -0.747215,  5.07833,  -4.10746,   1.17173},    /* X   */
      {0.489508, -0.260699,  0.432917, -1.99247,   2.48531},    /* Css */
      {0.514730,  6.92982, -24.7073,   23.1098,  -11.3234 }     /* Cab */
    }, {   /* HCTH/147 */
      {1.09025, -0.799194,   5.57212, -5.86760,  3.04544 },     /* X   */
      {0.562576, 0.0171436, -1.30636,  1.05747,  0.885429},     /* Css */
      {0.542352, 7.01464,  -28.3822,  35.0329, -20.4284  },     /* Cab */
    }, {   /* HCTH/407 */
      {1.08184, -0.518339,  3.42562, -2.62901,  2.28855},       /* X   */
      {1.18777, -2.40292,   5.61741, -9.17923,  6.24798},       /* Css */
      {0.589076, 4.42374, -19.2218,  42.5721, -42.0052 }        /* Cab */
    }, {   /* Becke 97 */
      {0.8094, 0.5073,  0.7481, 0.0, 0.0},                      /* X   */
      {0.1737, 2.3487, -2.4868, 0.0, 0.0},                      /* Css */
      {0.9454, 0.7471, -4.5961, 0.0, 0.0}                       /* Cab */
    }, {   /* Becke 97-1 */
      {0.789518, 0.573805,  0.660975, 0.0, 0.0},                /* X   */
      {0.0820011, 2.71681, -2.87103,  0.0, 0.0},                /* Css */
      {0.955689, 0.788552, -5.47869,  0.0, 0.0}                 /* Cab */
    }, {   /* Becke 97-2 */
      {0.827642,  0.0478400, 1.76125,  0.0, 0.0},               /* X   */
      {0.585808, -0.691682,  0.394796, 0.0, 0.0},               /* Css */
      {0.999849,  1.40626,  -7.44060,  0.0, 0.0}                /* Cab */
    }, {   /* Becke 97-D */
      {1.08662, -0.52127,  3.25429, 0.0, 0.0},                  /* X   */
      {0.22340, -1.56208,  1.94293, 0.0, 0.0},                  /* Css */
      {0.69041,  6.30270, -14.9712, 0.0, 0.0}                   /* Cab */
    }, {   /* Becke 97-K */
      {0.507863, 1.46873, -1.51301, 0.0, 0.0},                  /* X   */
      {0.12355,  2.65399, -3.20694, 0.0, 0.0},                  /* Css */
      {1.58613, -6.20977,  6.46106, 0.0, 0.0}                   /* Cab */
    }, {   /* Becke 97-3 */
      {+7.334648E-01, +2.925270E-01, +3.338789E+00, -1.051158E+01, +1.060907E+01},  /* X   */
      {+5.623649E-01, -1.322980E+00, +6.359191E+00, -7.464002E+00, +1.827082E+00},  /* Css */
      {+1.133830E+00, -2.811967E+00, +7.431302E+00, -1.969342E+00, -1.174423E+01}   /* Cab */
    }, {   /* SB98-1a */
      { 0.845975,  0.228183,  0.749949, 0.0, 0.0},  /* X   */
      {-0.817637, -0.054676,  0.592163, 0.0, 0.0},  /* Css */
      { 0.975483,  0.398379, -3.73540,  0.0, 0.0}   /* Cab */
    }, {   /* SB98-1b */
      { 0.800103, -0.084192,  1.47742, 0.0, 0.0},  /* X   */
      { 1.44946,  -2.37073,   2.13564, 0.0, 0.0},  /* Css */
      { 0.977621,  0.931199, -4.76973, 0.0, 0.0}   /* Cab */
    }, {   /* SB98-1c */
      { 0.810936, 0.496090,  0.772385, 0.0, 0.0},  /* X   */
      { 0.262077, 2.12576,  -2.30465,  0.0, 0.0},  /* Css */
      { 0.939269, 0.898121, -4.91276,  0.0, 0.0}   /* Cab */
    }, {   /* SB98-2a */
      { 0.749200, 0.402322,  0.620779, 0.0, 0.0},  /* X   */
      { 1.26686,  1.67146,  -1.22565,  0.0, 0.0},  /* Css */
      { 0.964641, 0.050527, -3.01966,  0.0, 0.0}   /* Cab */
    }, {   /* SB98-2b */
      { 0.770587, 0.180767,  0.955246, 0.0, 0.0},  /* X   */
      { 0.170473, 1.24051,  -0.862711, 0.0, 0.0},  /* Css */
      { 0.965362, 0.863300, -4.61778,  0.0, 0.0}   /* Cab */
    }, {   /* SB98-2c */
      { 0.790194, 0.400271,  0.832857, 0.0, 0.0},  /* X   */
      {-0.120163, 2.82332,  -2.59412,  0.0, 0.0},  /* Css */
      { 0.934715, 1.14105,  -5.33398,  0.0, 0.0}   /* Cab */
    },
  };
  const FLOAT gamma[3] = {
    0.004, 0.2, 0.006
  };

  FLOAT s2, dd, x, dxds, d2xds2, dgdx, d2gdx2;
  const FLOAT *cc;
  int func;

  switch(p->info->number){
  case XC_GGA_XC_HCTH_93:  func = 0; break;
  case XC_GGA_XC_HCTH_120: func = 1; break;
  case XC_GGA_XC_HCTH_147: func = 2; break;
  case XC_GGA_XC_HCTH_407: func = 3; break;
  case XC_GGA_XC_B97:      func = 4; break;
  case XC_GGA_XC_B97_1:    func = 5; break;
  case XC_GGA_XC_B97_2:    func = 6; break;
  case XC_GGA_XC_B97_D:    func = 7; break;
  case XC_GGA_XC_B97_K:    func = 8; break;
  case XC_GGA_XC_B97_3:    func = 9; break;
  case XC_GGA_XC_SB98_1a:  func = 10; break;
  case XC_GGA_XC_SB98_1b:  func = 11; break;
  case XC_GGA_XC_SB98_1c:  func = 12; break;
  case XC_GGA_XC_SB98_2a:  func = 13; break;
  case XC_GGA_XC_SB98_2b:  func = 14; break;
  case XC_GGA_XC_SB98_2c:  func = 15; break;
  default:
    fprintf(stderr, "Internal error in gga_b97\n");
    exit(1);
    break;
  }

  cc = c[func][type];
  s2 = s*s;
  dd = 1.0 + gamma[type]*s2;
  x  = gamma[type] * s2/dd;

  *g = cc[0] + x*(cc[1] + x*(cc[2] + x*(cc[3] + x*cc[4])));

  if(order < 1) return;

  dxds  = gamma[type] * 2.0*s/(dd*dd);
  dgdx  = cc[1] + x*(2.0*cc[2] + x*(3.0*cc[3] + x*4.0*cc[4]));
  *dgds = dgdx*dxds;

  /* *ldg = cc[1]*gamma[type]; */ /* times 2s */

  if(order < 2) return;
  
  d2gdx2  = 2.0*cc[2] + x*(6.0*cc[3] + x*12.0*cc[4]);
  d2xds2  = 2.0*gamma[type]*(1.0 - 3.0*gamma[type]*s2)/(dd*dd*dd);
  *d2gds2 = d2gdx2*dxds*dxds + dgdx*d2xds2;
}

static inline void
func_gga_becke_exchange(const XC(gga_type) *p, FLOAT xt, int order, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  func_g(p, 0, xt, order, f, dfdx, d2fdx2);
}

static inline void
func_gga_becke_parallel(const XC(gga_type) *p, FLOAT xt, int order, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  func_g(p, 1, xt, order, f, dfdx, d2fdx2);
}

static inline void
func_gga_becke_opposite(const XC(gga_type) *p, FLOAT xt, int order, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  func_g(p, 2, xt, order, f, dfdx, d2fdx2);
}

#include "work_gga_becke.c"

const XC(func_info_type) XC(func_info_gga_xc_b97) = {
  XC_GGA_XC_B97,
  XC_EXCHANGE_CORRELATION,
  "Becke 97",
  XC_FAMILY_GGA,
  "AD Becke, J. Chem. Phys. 107, 8554 (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_b97_1) = {
  XC_GGA_XC_B97_1,
  XC_EXCHANGE_CORRELATION,
  "Becke 97-1",
  XC_FAMILY_GGA,
  "FA Hamprecht, AJ Cohen, DJ Tozer, and NC Handy, J. Chem. Phys. 109, 6264 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init,
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_b97_2) = {
  XC_GGA_XC_B97_2,
  XC_EXCHANGE_CORRELATION,
  "Becke 97-2",
  XC_FAMILY_GGA,
  "PJ Wilson, TJ Bradley, and DJ Tozer, J. Chem. Phys. 115, 9233 (2001)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_b97_d) = {
  XC_GGA_XC_B97_D,
  XC_EXCHANGE_CORRELATION,
  "Becke 97-D",
  XC_FAMILY_GGA,
  "S Grimme, J. Comput. Chem. 27, 1787 (2006)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_b97_k) = {
  XC_GGA_XC_B97_K,
  XC_EXCHANGE_CORRELATION,
  "Boese-Martin for Kinetics",
  XC_FAMILY_GGA,
  "AD Boese and JML Martin, J. Chem. Phys., Vol. 121, 3405 (2004)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_b97_3) = {
  XC_GGA_XC_B97_3,
  XC_EXCHANGE_CORRELATION,
  "Becke 97-3",
  XC_FAMILY_GGA,
  "TW Keal and DJ Tozer, J. Chem. Phys. 123, 121103 (2005)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_hcth_93) = {
  XC_GGA_XC_HCTH_93,
  XC_EXCHANGE_CORRELATION,
  "HCTH/93",
  XC_FAMILY_GGA,
  "FA Hamprecht, AJ Cohen, DJ Tozer, and NC Handy, J. Chem. Phys. 109, 6264 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_hcth_120) = {
  XC_GGA_XC_HCTH_120,
  XC_EXCHANGE_CORRELATION,
  "HCTH/120",
  XC_FAMILY_GGA,
  "AD Boese, NL Doltsinis, NC Handy, and M Sprik, J. Chem. Phys. 112, 1670 (2000)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_hcth_147) = {
  XC_GGA_XC_HCTH_147,
  XC_EXCHANGE_CORRELATION,
  "HCTH/147",
  XC_FAMILY_GGA,
  "AD Boese, NL Doltsinis, NC Handy, and M Sprik, J. Chem. Phys. 112, 1670 (2000)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_hcth_407) = {
  XC_GGA_XC_HCTH_407,
  XC_EXCHANGE_CORRELATION,
  "HCTH/407",
  XC_FAMILY_GGA,
  "AD Boese and NC Handy, J. Chem. Phys. 114, 5497 (2001)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_sb98_1a) = {
  XC_GGA_XC_SB98_1a,
  XC_EXCHANGE_CORRELATION,
  "SB98 (1a)",
  XC_FAMILY_GGA,
  "HL Schmider and AD Becke, J. Chem. Phys. 108, 9624 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_sb98_1b) = {
  XC_GGA_XC_SB98_1b,
  XC_EXCHANGE_CORRELATION,
  "SB98 (1b)",
  XC_FAMILY_GGA,
  "HL Schmider and AD Becke, J. Chem. Phys. 108, 9624 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_sb98_1c) = {
  XC_GGA_XC_SB98_1c,
  XC_EXCHANGE_CORRELATION,
  "SB98 (1c)",
  XC_FAMILY_GGA,
  "HL Schmider and AD Becke, J. Chem. Phys. 108, 9624 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_sb98_2a) = {
  XC_GGA_XC_SB98_2a,
  XC_EXCHANGE_CORRELATION,
  "SB98 (2a)",
  XC_FAMILY_GGA,
  "HL Schmider and AD Becke, J. Chem. Phys. 108, 9624 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_sb98_2b) = {
  XC_GGA_XC_SB98_2b,
  XC_EXCHANGE_CORRELATION,
  "SB98 (2b)",
  XC_FAMILY_GGA,
  "HL Schmider and AD Becke, J. Chem. Phys. 108, 9624 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};

const XC(func_info_type) XC(func_info_gga_xc_sb98_2c) = {
  XC_GGA_XC_SB98_2c,
  XC_EXCHANGE_CORRELATION,
  "SB98 (2c)",
  XC_FAMILY_GGA,
  "HL Schmider and AD Becke, J. Chem. Phys. 108, 9624 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  work_gga_becke_init, 
  NULL,
  NULL,
  work_gga_becke
};
