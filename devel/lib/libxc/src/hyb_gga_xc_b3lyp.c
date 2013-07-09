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

#define XC_HYB_GGA_XC_B3PW91  401 /* The original hybrid proposed by Becke */
#define XC_HYB_GGA_XC_B3LYP   402 /* The (in)famous B3LYP                  */
#define XC_HYB_GGA_XC_B3P86   403 /* Perdew 86 hybrid similar to B3PW91    */
#define XC_HYB_GGA_XC_mPW3PW  415 /* mixture with the mPW functional */
#define XC_HYB_GGA_XC_mPW3LYP 419 /* mixture of mPW and LYP */

void
XC(hyb_gga_xc_b3pw91_init)(void *p_)
{
  static int   funcs_id  [4] = {XC_LDA_X, XC_GGA_X_B88, XC_LDA_C_PW, XC_GGA_C_PW91};
  static FLOAT funcs_coef[4] = {1.0 - 0.20 - 0.72, 0.72, 1.0 - 0.81, 0.81};
  XC(gga_type) *p = (XC(gga_type) *)p_;

  XC(gga_init_mix)(p, 4, funcs_id, funcs_coef);
  p->exx_coef = 0.20;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_b3pw91) = {
  XC_HYB_GGA_XC_B3PW91,
  XC_EXCHANGE_CORRELATION,
  "B3PW91",
  XC_FAMILY_HYB_GGA,
  "AD Becke, J. Chem. Phys. 98, 5648 (1993)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  XC(hyb_gga_xc_b3pw91_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_b3lyp_init)(void *p_)
{
  static int   funcs_id  [4] = {XC_LDA_X, XC_GGA_X_B88, XC_LDA_C_VWN_RPA, XC_GGA_C_LYP};
  static FLOAT funcs_coef[4] = {1.0 - 0.20 - 0.72, 0.72, 1.0 - 0.81, 0.81};
  XC(gga_type) *p = (XC(gga_type) *)p_;

  XC(gga_init_mix)(p, 4, funcs_id, funcs_coef);
  XC(lda_c_vwn_set_params)(p->func_aux[2], 1);
  p->exx_coef = 0.20;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_b3lyp) = {
  XC_HYB_GGA_XC_B3LYP,
  XC_EXCHANGE_CORRELATION,
  "B3LYP",
  XC_FAMILY_HYB_GGA,
  "PJ Stephens, FJ Devlin, CF Chabalowski, MJ Frisch, J. Phys. Chem. 98 11623 (1994)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  XC(hyb_gga_xc_b3lyp_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_b3p86_init)(void *p_)
{
  static int   funcs_id  [4] = {XC_LDA_X, XC_GGA_X_B88, XC_LDA_C_VWN_RPA, XC_GGA_C_P86};
  static FLOAT funcs_coef[4] = {1.0 - 0.20 - 0.72, 0.72, 1.0 - 0.81, 0.81};
  XC(gga_type) *p = (XC(gga_type) *)p_;

  XC(gga_init_mix)(p, 4, funcs_id, funcs_coef);
  XC(lda_c_vwn_set_params)(p->func_aux[2], 1);
  p->exx_coef = 0.20;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_b3p86) = {
  XC_HYB_GGA_XC_B3P86,
  XC_EXCHANGE_CORRELATION,
  "B3P86",
  XC_FAMILY_HYB_GGA,
  "Defined through Gaussian implementation",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  XC(hyb_gga_xc_b3p86_init),
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_mpw3pw_init)(void *p_)
{
  static int   funcs_id  [4] = {XC_LDA_X, XC_GGA_X_mPW91, XC_LDA_C_VWN_RPA, XC_GGA_C_PW91};
  static FLOAT funcs_coef[4] = {1.0 - 0.20 - 0.72, 0.72, 1.0 - 0.81, 0.81};
  XC(gga_type) *p = (XC(gga_type) *)p_;

  XC(gga_init_mix)(p, 4, funcs_id, funcs_coef);
  XC(lda_c_vwn_set_params)(p->func_aux[2], 1);
  p->exx_coef = 0.20;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_mpw3pw) = {
  XC_HYB_GGA_XC_mPW3PW,
  XC_EXCHANGE_CORRELATION,
  "mPW3PW of Adamo & Barone",
  XC_FAMILY_GGA,
  "C Adamo and V Barone, J. Chem. Phys. 108, 664 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  XC(hyb_gga_xc_mpw3pw_init), 
  NULL, NULL, NULL
};


void
XC(hyb_gga_xc_mpw3lyp_init)(void *p_)
{
  static int   funcs_id  [4] = {XC_LDA_X, XC_GGA_X_mPW91, XC_LDA_C_VWN_RPA, XC_GGA_C_LYP};
  static FLOAT funcs_coef[4] = {1.0 - 0.218 - 0.709, 0.709, 1.0 - 0.871, 0.871};
  XC(gga_type) *p = (XC(gga_type) *)p_;

  XC(gga_init_mix)(p, 4, funcs_id, funcs_coef);
  XC(lda_c_vwn_set_params)(p->func_aux[2], 1);
  p->exx_coef = 0.218;
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_mpw3lyp) = {
  XC_HYB_GGA_XC_mPW3LYP,
  XC_EXCHANGE_CORRELATION,
  "mPW3LYP",
  XC_FAMILY_GGA,
  "Y Zhao and DGJ Truhlar, Phys. Chem. A 108, 6908 (2004)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  XC(hyb_gga_xc_mpw3lyp_init), 
  NULL, NULL, NULL
};
