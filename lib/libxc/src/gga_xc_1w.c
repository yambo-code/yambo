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

#define XC_GGA_XC_XLYP       166  /* XLYP functional */
#define XC_GGA_XC_PBE1W      173  /* Functionals fitted for water */
#define XC_GGA_XC_MPWLYP1W   174  /* Functionals fitted for water */
#define XC_GGA_XC_PBELYP1W   175  /* Functionals fitted for water */

static void
gga_xc_xlyp_init(XC(func_type) *p)
{
  static int   funcs_id  [4] = {XC_LDA_X, XC_GGA_X_B88, XC_GGA_X_PW91, XC_GGA_C_LYP};
  static FLOAT funcs_coef[4] = {1.0 - 0.722 - 0.347, 0.722, 0.347, 1.0};

  XC(mix_init)(p, 4, funcs_id, funcs_coef);
}

const XC(func_info_type) XC(func_info_gga_xc_xlyp) = {
  XC_GGA_XC_XLYP,
  XC_EXCHANGE_CORRELATION,
  "XLYP",
  XC_FAMILY_GGA,
  "X Xu and WA Goddard, III, PNAS 101, 2673 (2004)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_xc_xlyp_init, 
  NULL, NULL, NULL
};


static void
gga_xc_pbe1w_init(XC(func_type) *p)
{
  static int   funcs_id  [3] = {XC_LDA_C_VWN, XC_GGA_X_PBE, XC_GGA_C_PBE};
  static FLOAT funcs_coef[3] = {1.0 - 74.0/100.0, 1.0, 74.0/100.0};

  XC(mix_init)(p, 3, funcs_id, funcs_coef);
}

const XC(func_info_type) XC(func_info_gga_xc_pbe1w) = {
  XC_GGA_XC_PBE1W,
  XC_EXCHANGE_CORRELATION,
  "PBE1W",
  XC_FAMILY_GGA,
  "EE Dahlke and DG Truhlar, J. Phys. Chem. B 109, 15677 (2005)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_xc_pbe1w_init, 
  NULL, NULL, NULL
};


static void
gga_xc_mpwlyp1w_init(XC(func_type) *p)
{
  static int   funcs_id  [3] = {XC_LDA_C_VWN, XC_GGA_X_MPW91, XC_GGA_C_LYP};
  static FLOAT funcs_coef[3] = {1.0 - 88.0/100.0, 1.0, 88.0/100.0};

  XC(mix_init)(p, 3, funcs_id, funcs_coef);
}

const XC(func_info_type) XC(func_info_gga_xc_mpwlyp1w) = {
  XC_GGA_XC_MPWLYP1W,
  XC_EXCHANGE_CORRELATION,
  "mPWLYP1w",
  XC_FAMILY_GGA,
  "EE Dahlke and DG Truhlar, J. Phys. Chem. B 109, 15677 (2005)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_xc_mpwlyp1w_init, 
  NULL, NULL, NULL
};


static void
gga_xc_pbelyp1w_init(XC(func_type) *p)
{
  static int   funcs_id  [3] = {XC_LDA_C_VWN, XC_GGA_X_PBE, XC_GGA_C_LYP};
  static FLOAT funcs_coef[3] = {1.0 - 74.0/100.0, 1.0, 74.0/100.0};

  XC(mix_init)(p, 3, funcs_id, funcs_coef);
}

const XC(func_info_type) XC(func_info_gga_xc_pbelyp1w) = {
  XC_GGA_XC_PBELYP1W,
  XC_EXCHANGE_CORRELATION,
  "PBELYP1W",
  XC_FAMILY_GGA,
  "EE Dahlke and DG Truhlar, J. Phys. Chem. B 109, 15677 (2005)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_xc_pbelyp1w_init, 
  NULL, NULL, NULL
};

