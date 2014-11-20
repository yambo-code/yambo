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

#define XC_GGA_C_WL  147 /* Wilson & Levy */

static inline void 
func(const XC(func_type) *p, XC(gga_work_c_t) *r)
{
  const FLOAT a=-0.74860, b=0.06001, c=3.60073, d=0.90000;

  FLOAT aux, num, den;

  if(1.0 - ABS(r->zeta) >= p->info->min_zeta){
    aux = SQRT(1.0 - r->zeta*r->zeta);
    num = a + b*r->xt;
    den = c + d*(r->xs[0] + r->xs[1]) + r->rs;

    r->f  = aux * num/den;
  }else
    r->f = 0.0;

  if(r->order < 1) return;

  if(1.0 - ABS(r->zeta) >= p->info->min_zeta){
    r->dfdrs    = -r->f/den;
    r->dfdz     = -r->zeta/aux * num/den;
    r->dfdxt    = b*aux/den;
    r->dfdxs[0] = -d*r->f/den;
    r->dfdxs[1] = r->dfdxs[0];
  }else{
    r->dfdrs = r->dfdz = r->dfdxt = 0.0;
    r->dfdxs[0] = r->dfdxs[1] = 0.0;
  }

  if(r->order < 2) return;

  if(1.0 - ABS(r->zeta) >= p->info->min_zeta){
    r->d2fdrs2     = -2.0*(r->dfdrs)/den;
    r->d2fdrsz     = r->zeta/aux * num/(den*den);
    r->d2fdrsxt    = -b*aux/(den*den);
    r->d2fdrsxs[0] = -2.0*d*(r->dfdrs)/den;
    r->d2fdrsxs[1] = r->d2fdrsxs[0];
    r->d2fdz2      = -num/(den*aux*aux*aux);
    r->d2fdzxt     = -r->zeta/aux * b/den;
    r->d2fdzxs[0]  = r->zeta/aux * d*num/(den*den);
    r->d2fdzxs[1]  = r->d2fdzxs[0];
    r->d2fdxt2     = 0.0;
    r->d2fdxtxs[0] = -b*d*aux/(den*den);
    r->d2fdxtxs[1] = r->d2fdxtxs[0];
    r->d2fdxs2[0]  = -2.0*d*r->dfdxs[0]/den;
    r->d2fdxs2[1]  = r->d2fdxs2[0];
    r->d2fdxs2[2]  = r->d2fdxs2[0];
  }else{
    r->d2fdrs2 = r->d2fdrsz = r->d2fdrsxt = r->d2fdrsxs[0] = r->d2fdrsxs[1] = 0.0;
    r->d2fdz2 = r->d2fdzxt = r->d2fdzxs[0] = r->d2fdzxs[1] = 0.0;
    r->d2fdxt2 = r->d2fdxtxs[0] = r->d2fdxtxs[1] = 0.0;
    r->d2fdxs2[0] = r->d2fdxs2[1] = r->d2fdxs2[2] = 0.0;
  }
}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_wl) = {
  XC_GGA_C_WL,
  XC_CORRELATION,
  "Wilson & Levy",
  XC_FAMILY_GGA,
  "LC Wilson and M Levy, Phys. Rev. B 41, 12930 (1990)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  NULL, NULL, NULL,
  work_gga_c
};
