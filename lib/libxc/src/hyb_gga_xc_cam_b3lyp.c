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

#define XC_HYB_GGA_XC_CAM_B3LYP        433 /* CAM version of B3LYP */
#define XC_HYB_GGA_XC_TUNED_CAM_B3LYP  434 /* CAM version of B3LYP tunes for excitations*/

void
XC(hyb_gga_xc_cam_b3lyp_init)(XC(func_type) *p)
{
  static FLOAT ac = 0.81;
  static int   funcs_id  [4] = {XC_GGA_X_B88, XC_GGA_X_ITYH, XC_LDA_C_VWN, XC_GGA_C_LYP};
  static FLOAT funcs_coef[4];

  switch(p->info->number){
  case XC_HYB_GGA_XC_CAM_B3LYP:
    p->cam_omega = 0.33;
    p->cam_alpha = 0.19;
    p->cam_beta  = 0.46;
    break;
  case XC_HYB_GGA_XC_TUNED_CAM_B3LYP:
    p->cam_omega = 0.150;
    p->cam_alpha = 0.0799;
    p->cam_beta  = 0.9201;
    break;
  }

  funcs_coef[0] = 1.0 - p->cam_alpha - p->cam_beta;
  funcs_coef[1] = p->cam_beta;
  funcs_coef[2] = 1.0 - ac;
  funcs_coef[3] = ac;

  XC(mix_init)(p, 4, funcs_id, funcs_coef);

  XC(gga_x_ityh_set_params)(p->func_aux[1], XC_GGA_X_B88, p->cam_omega);
}

const XC(func_info_type) XC(func_info_hyb_gga_xc_cam_b3lyp) = {
  XC_HYB_GGA_XC_CAM_B3LYP,
  XC_EXCHANGE_CORRELATION,
  "CAM version of B3LYP",
  XC_FAMILY_HYB_GGA,
  "T Yanai, DP Tew, NC Handy, Chem. Phys. Lett. 393, 51-57 (2004)",
  XC_FLAGS_3D | XC_FLAGS_HYB_CAM | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 0.0, 1e-32,
  XC(hyb_gga_xc_cam_b3lyp_init),
  NULL, NULL, NULL
};


const XC(func_info_type) XC(func_info_hyb_gga_xc_tuned_cam_b3lyp) = {
  XC_HYB_GGA_XC_TUNED_CAM_B3LYP,
  XC_EXCHANGE_CORRELATION,
  "CAM version of B3LYP",
  XC_FAMILY_HYB_GGA,
  "K Okuno, Y Shigeta, R Kishi, H Miyasaka, M Nakano, J. Photochem. Photobiol., A 235, 29-34 (2012)",
  XC_FLAGS_3D | XC_FLAGS_HYB_CAM | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 0.0, 1e-32,
  XC(hyb_gga_xc_cam_b3lyp_init),
  NULL, NULL, NULL
};
