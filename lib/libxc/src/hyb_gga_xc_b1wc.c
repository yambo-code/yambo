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

#define XC_HYB_GGA_XC_B1WC      412  /* Becke 1-parameter mixture of WC and PBE          */
#define XC_HYB_GGA_XC_B1LYP     416  /* Becke 1-parameter mixture of B88 and LYP         */
#define XC_HYB_GGA_XC_B1PW91    417  /* Becke 1-parameter mixture of B88 and PW91        */
#define XC_HYB_GGA_XC_mPW1PW    418  /* Becke 1-parameter mixture of mPW91 and PW91      */
#define XC_HYB_GGA_XC_mPW1K     405  /* mixture of mPW91 and PW91 optimized for kinetics */
#define XC_HYB_GGA_XC_BHANDH    435  /* Becke half-and-half                              */
#define XC_HYB_GGA_XC_BHANDHLYP 436  /* Becke half-and-half with B88 exchange            */
#define XC_HYB_GGA_XC_MPWLYP1M  453  /* MPW with 1 par. for metals/LYP                   */

void
XC(hyb_gga_xc_b1wc_init)(XC(func_type) *p)
{
  static int   funcs_id  [2] = {XC_GGA_X_WC, XC_GGA_C_PBE};
  static FLOAT funcs_coef[2] = {1.0 - 0.16, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);
  p->cam_alpha = 0.16;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_b1wc) = {
  XC_HYB_GGA_XC_B1WC,
  XC_EXCHANGE_CORRELATION,
  "B1WC",
  XC_FAMILY_HYB_GGA,
  "DI Bilc, R Orlando, R Shaltaf, G-M Rignanese, J Iniguez, and Ph Ghosez, Phys. Rev. B 77, 165107 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  XC(hyb_gga_xc_b1wc_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_b1lyp_init)(XC(func_type) *p)
{
  static int   funcs_id  [2] = {XC_GGA_X_B88, XC_GGA_C_LYP};
  static FLOAT funcs_coef[2] = {1.0 - 0.25, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);
  p->cam_alpha = 0.25;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_b1lyp) = {
  XC_HYB_GGA_XC_B1LYP,
  XC_EXCHANGE_CORRELATION,
  "B1LYP",
  XC_FAMILY_HYB_GGA,
  "C. Adamo, V. Barone, Chem. Phys. Lett. 274, 242 (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  XC(hyb_gga_xc_b1lyp_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_b1pw91_init)(XC(func_type) *p)
{
  static int   funcs_id  [2] = {XC_GGA_X_B88, XC_GGA_C_PW91};
  static FLOAT funcs_coef[2] = {1.0 - 0.25, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);
  p->cam_alpha = 0.25;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_b1pw91) = {
  XC_HYB_GGA_XC_B1PW91,
  XC_EXCHANGE_CORRELATION,
  "B1PW91",
  XC_FAMILY_HYB_GGA,
  "C. Adamo, V. Barone, Chem. Phys. Lett. 274, 242 (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  XC(hyb_gga_xc_b1pw91_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_mpw1pw_init)(XC(func_type) *p)
{
  static int   funcs_id  [2] = {XC_GGA_X_MPW91, XC_GGA_C_PW91};
  static FLOAT funcs_coef[2] = {1.0 - 0.25, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);
  p->cam_alpha = 0.25;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_mpw1pw) = {
  XC_HYB_GGA_XC_mPW1PW,
  XC_EXCHANGE_CORRELATION,
  "mPW1PW",
  XC_FAMILY_HYB_GGA,
  "C. Adamo, V. Barone, J. Chem. Phys. 108, 664 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  XC(hyb_gga_xc_mpw1pw_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_mpw1k_init)(XC(func_type) *p)
{
  static int   funcs_id  [2] = {XC_GGA_X_MPW91, XC_GGA_C_PW91};
  static FLOAT funcs_coef[2] = {1.0 - 0.428, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);
  p->cam_alpha = 0.428;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_mpw1k) = {
  XC_HYB_GGA_XC_mPW1K,
  XC_EXCHANGE_CORRELATION,
  "mPW1K",
  XC_FAMILY_HYB_GGA,
  "BJ Lynch, PL Fast, M Harris, DGJ Truhlar, Phys. Chem. A 104, 4811 (2000)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  XC(hyb_gga_xc_mpw1k_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_bhandh_init)(XC(func_type) *p)
{
  static int   funcs_id  [2] = {XC_LDA_X, XC_GGA_C_LYP};
  static FLOAT funcs_coef[2] = {0.5, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);
  p->cam_alpha = 0.5;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_bhandh) = {
  XC_HYB_GGA_XC_BHANDH,
  XC_EXCHANGE_CORRELATION,
  "BHandH",
  XC_FAMILY_HYB_GGA,
  "AD Becke, J. Chem. Phys., 98 1372-77 (1993)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  XC(hyb_gga_xc_bhandh_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_bhandhlyp_init)(XC(func_type) *p)
{
  static int   funcs_id  [2] = {XC_GGA_X_B88, XC_GGA_C_LYP};
  static FLOAT funcs_coef[2] = {0.5, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);
  p->cam_alpha = 0.5;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_bhandhlyp) = {
  XC_HYB_GGA_XC_BHANDHLYP,
  XC_EXCHANGE_CORRELATION,
  "BHandHLYP",
  XC_FAMILY_HYB_GGA,
  "AD Becke, J. Chem. Phys., 98 1372-77 (1993)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  XC(hyb_gga_xc_bhandhlyp_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_mpwlyp1m_init)(XC(func_type) *p)
{
  static int   funcs_id  [2] = {XC_GGA_X_MPW91, XC_GGA_C_LYP};
  static FLOAT funcs_coef[2] = {1.0 - 0.05, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);
  p->cam_alpha = 0.05;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_mpwlyp1m) = {
  XC_HYB_GGA_XC_MPWLYP1M,
  XC_EXCHANGE_CORRELATION,
  "MPW with 1 par. for metals/LYP",
  XC_FAMILY_HYB_GGA,
  "NE Schultz, Y Zhao, and DG Truhlar, J. Phys. Chem. A 109, 11127-11143 (2005)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  XC(hyb_gga_xc_mpwlyp1m_init),
  NULL, NULL, NULL
};
