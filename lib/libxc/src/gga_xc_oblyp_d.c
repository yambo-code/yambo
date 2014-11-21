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

#define XC_GGA_XC_OBLYP_D       67  /* oBLYP-D functional of Goerigk and Grimme  */
#define XC_GGA_XC_OPWLYP_D      66  /* oPWLYP-D functional of Goerigk and Grimme */
#define XC_GGA_XC_OPBE_D        65  /* oPBE_D functional of Goerigk and Grimme   */

static void
gga_xc_oblyp_d_init(XC(func_type) *p)
{
  static int   funcs_id  [4] = {XC_GGA_X_B88, XC_GGA_C_LYP};
  static FLOAT funcs_coef[4] = {1.0, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);
  XC(gga_x_b88_set_params)(p->func_aux[0], 0.00401, 6.0);
  XC(gga_c_lyp_set_params)(p->func_aux[1], 0.05047, 0.140, 0.2196, 0.363);
}

const XC(func_info_type) XC(func_info_gga_xc_oblyp_d) = {
  XC_GGA_XC_OBLYP_D,
  XC_EXCHANGE_CORRELATION,
  "oBLYP-D functional of Goerigk and Grimme",
  XC_FAMILY_GGA,
  "L Goerigk and S Grimme, J. Chem. Theory Comput. 6, 107-126 (2010)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_xc_oblyp_d_init,
  NULL, NULL, NULL
};


static void
gga_xc_opwlyp_d_init(XC(func_type) *p)
{
  static int   funcs_id  [4] = {XC_GGA_X_PW91, XC_GGA_C_LYP};
  static FLOAT funcs_coef[4] = {1.0, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);
  XC(gga_x_pw91_set_params2)(p->func_aux[0], 0.00402, 0.8894/(X2S*X2S), 0.79);
  XC(gga_c_lyp_set_params)(p->func_aux[1], 0.04960, 0.144, 0.2262, 0.346);
}

const XC(func_info_type) XC(func_info_gga_xc_opwlyp_d) = {
  XC_GGA_XC_OPWLYP_D,
  XC_EXCHANGE_CORRELATION,
  "oPWLYP-D functional of Goerigk and Grimme",
  XC_FAMILY_GGA,
  "L Goerigk and S Grimme, J. Chem. Theory Comput. 6, 107-126 (2010)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_xc_opwlyp_d_init,
  NULL, NULL, NULL
};


static void
gga_xc_opbe_d_init(XC(func_type) *p)
{
  static int   funcs_id  [4] = {XC_GGA_X_PBE, XC_GGA_C_PBE};
  static FLOAT funcs_coef[4] = {1.0, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);
  XC(gga_x_pbe_set_params)(p->func_aux[0], 1.2010, 0.21198);
  XC(gga_c_pbe_set_params)(p->func_aux[1], 0.04636);
}

const XC(func_info_type) XC(func_info_gga_xc_opbe_d) = {
  XC_GGA_XC_OPBE_D,
  XC_EXCHANGE_CORRELATION,
  "oBLYP-D functional of Goerigk and Grimme",
  XC_FAMILY_GGA,
  "L Goerigk and S Grimme, J. Chem. Theory Comput. 6, 107-126 (2010)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_xc_opbe_d_init,
  NULL, NULL, NULL
};
