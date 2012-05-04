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

#define XC_HYB_GGA_XC_B1WC   412 /* Becke 1-parameter mixture of WC and PBE */
#define XC_HYB_GGA_XC_B1LYP  416 /* Becke 1-parameter mixture of B88 and LYP */
#define XC_HYB_GGA_XC_B1PW91 417 /* Becke 1-parameter mixture of B88 and PW91 */
#define XC_HYB_GGA_XC_mPW1PW 418 /* Becke 1-parameter mixture of mPW91 and PW91 */
#define XC_HYB_GGA_XC_mPW1K  405 /* mixture of mPW91 and PW91 optimized for kinetics */

void
XC(hyb_gga_xc_b1wc_init)(void *p_)
{
  static int   funcs_id  [2] = {XC_GGA_X_WC, XC_GGA_C_PBE};
  static FLOAT funcs_coef[2] = {1.0 - 0.16, 1.0};
  XC(gga_type) *p = (XC(gga_type) *)p_;

  XC(gga_init_mix)(p, 2, funcs_id, funcs_coef);
  p->exx_coef = 0.16;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_b1wc) = {
  XC_HYB_GGA_XC_B1WC,
  XC_EXCHANGE_CORRELATION,
  "B1WC",
  XC_FAMILY_HYB_GGA,
  "DI Bilc, R Orlando, R Shaltaf, G-M Rignanese, J Iniguez, and Ph Ghosez, Phys. Rev. B 77, 165107 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  XC(hyb_gga_xc_b1wc_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_b1lyp_init)(void *p_)
{
  static int   funcs_id  [2] = {XC_GGA_X_B88, XC_GGA_C_LYP};
  static FLOAT funcs_coef[2] = {1.0 - 0.25, 1.0};
  XC(gga_type) *p = (XC(gga_type) *)p_;

  XC(gga_init_mix)(p, 2, funcs_id, funcs_coef);
  p->exx_coef = 0.25;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_b1lyp) = {
  XC_HYB_GGA_XC_B1LYP,
  XC_EXCHANGE_CORRELATION,
  "B1LYP",
  XC_FAMILY_HYB_GGA,
  "C. Adamo, V. Barone, Chem. Phys. Lett. 274, 242 (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  XC(hyb_gga_xc_b1lyp_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_b1pw91_init)(void *p_)
{
  static int   funcs_id  [2] = {XC_GGA_X_B88, XC_GGA_C_PW91};
  static FLOAT funcs_coef[2] = {1.0 - 0.25, 1.0};
  XC(gga_type) *p = (XC(gga_type) *)p_;

  XC(gga_init_mix)(p, 2, funcs_id, funcs_coef);
  p->exx_coef = 0.25;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_b1pw91) = {
  XC_HYB_GGA_XC_B1PW91,
  XC_EXCHANGE_CORRELATION,
  "B1PW91",
  XC_FAMILY_HYB_GGA,
  "C. Adamo, V. Barone, Chem. Phys. Lett. 274, 242 (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  XC(hyb_gga_xc_b1pw91_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_mpw1pw_init)(void *p_)
{
  static int   funcs_id  [2] = {XC_GGA_X_mPW91, XC_GGA_C_PW91};
  static FLOAT funcs_coef[2] = {1.0 - 0.25, 1.0};
  XC(gga_type) *p = (XC(gga_type) *)p_;

  XC(gga_init_mix)(p, 2, funcs_id, funcs_coef);
  p->exx_coef = 0.25;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_mpw1pw) = {
  XC_HYB_GGA_XC_mPW1PW,
  XC_EXCHANGE_CORRELATION,
  "mPW1PW",
  XC_FAMILY_HYB_GGA,
  "C. Adamo, V. Barone, J. Chem. Phys. 108, 664 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  XC(hyb_gga_xc_mpw1pw_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_mpw1k_init)(void *p_)
{
  static int   funcs_id  [2] = {XC_GGA_X_mPW91, XC_GGA_C_PW91};
  static FLOAT funcs_coef[2] = {1.0 - 0.428, 1.0};
  XC(gga_type) *p = (XC(gga_type) *)p_;

  XC(gga_init_mix)(p, 2, funcs_id, funcs_coef);
  p->exx_coef = 0.428;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_mpw1k) = {
  XC_HYB_GGA_XC_mPW1K,
  XC_EXCHANGE_CORRELATION,
  "mPW1K",
  XC_FAMILY_HYB_GGA,
  "BJ Lynch, PL Fast, M Harris, DGJ Truhlar, Phys. Chem. A 104, 4811 (2000)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  XC(hyb_gga_xc_mpw1k_init),
  NULL, NULL, NULL
};
