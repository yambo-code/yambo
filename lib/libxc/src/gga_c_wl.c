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
func(const XC(gga_type) *p, int order, FLOAT rs, FLOAT zeta, FLOAT xt, FLOAT *xs,
     FLOAT *f, FLOAT *dfdrs, FLOAT *dfdz, FLOAT *dfdxt, FLOAT *dfdxs,
     FLOAT *d2fdrs2, FLOAT *d2fdrsz, FLOAT *d2fdrsxt, FLOAT *d2fdrsxs, FLOAT *d2fdz2, 
     FLOAT *d2fdzxt, FLOAT *d2fdzxs, FLOAT *d2fdxt2, FLOAT *d2fdxtxs, FLOAT *d2fdxs2)
{
  const FLOAT a=-0.74860, b=0.06001, c=3.60073, d=0.90000;

  FLOAT aux, num, den;

  aux = SQRT(1.0 - zeta*zeta);
  num = a + b*xt;
  den = c + d*(xs[0] + xs[1]) + rs;

  *f  = aux * num/den;

  if(order < 1) return;

  *dfdrs   = -(*f)/den;
  *dfdz    = -zeta/aux * num/den;
  *dfdxt   = b*aux/den;
  dfdxs[0] = -d*(*f)/den;
  dfdxs[1] = dfdxs[0];

  if(order < 2) return;

  *d2fdrs2    = -2.0*(*dfdrs)/den;
  *d2fdrsz    = zeta/aux * num/(den*den);
  *d2fdrsxt   = -b*aux/(den*den);
  d2fdrsxs[0] = -2.0*d*(*dfdrs)/den;
  d2fdrsxs[1] = d2fdrsxs[0];
  *d2fdz2     = -num/(den*aux*aux*aux);
  *d2fdzxt    = -zeta/aux * b/den;
  d2fdzxs[0]  = zeta/aux * d*num/(den*den);
  d2fdzxs[1]  = d2fdzxs[0];
  *d2fdxt2    = 0.0;
  d2fdxtxs[0] = -b*d*aux/(den*den);
  d2fdxtxs[1] = d2fdxtxs[0];
  d2fdxs2[0]  = -2.0*d*dfdxs[0]/den;
  d2fdxs2[1]  = d2fdxs2[0];
  d2fdxs2[2]  = d2fdxs2[0];
}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_wl) = {
  XC_GGA_C_WL,
  XC_CORRELATION,
  "Wilson & Levy",
  XC_FAMILY_GGA,
  "LC Wilson and M Levy, Phys. Rev. B 41, 12930 (1990)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  NULL, NULL, NULL,
  work_gga_c
};
