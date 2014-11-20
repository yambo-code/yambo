/*
 Copyright (C) 2006-2013 M.A.L. Marques

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

#define XC_MGGA_XC_OTPSS_D      64  /* oTPSS_D functional of Goerigk and Grimme   */

static void
mgga_xc_otpss_d_init(XC(func_type) *p)
{
  static int   funcs_id  [4] = {XC_MGGA_X_TPSS, XC_MGGA_C_TPSS};
  static FLOAT funcs_coef[4] = {1.0, 1.0};

  XC(mix_init)(p, 2, funcs_id, funcs_coef);
  XC(mgga_x_tpss_set_params)(p->func_aux[0], 3.43, 0.75896, 0.165, 0.778, 0.41567);
  XC(mgga_c_pkzb_set_params)(p->func_aux[1], 0.08861, 0.7, 0.59, 0.9269, 0.6225, 2.1540);
}

const XC(func_info_type) XC(func_info_mgga_xc_otpss_d) = {
  XC_MGGA_XC_OTPSS_D,
  XC_EXCHANGE_CORRELATION,
  "oTPSS-D functional of Goerigk and Grimme",
  XC_FAMILY_MGGA,
  "L Goerigk and S Grimme, J. Chem. Theory Comput. 6, 107-126 (2010)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 0.0, 1e-32,
  mgga_xc_otpss_d_init,
  NULL, NULL, NULL
};
