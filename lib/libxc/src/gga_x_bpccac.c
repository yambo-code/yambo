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
#include "util.h"

#define XC_GGA_X_BPCCAC  98 /* BPCCAC (GRAC for the energy) */

static void 
gga_x_bpccac_init(XC(func_type) *p)
{
  p->n_func_aux  = 2;
  p->func_aux    = (XC(func_type) **) malloc(2*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));
  p->func_aux[1] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_GGA_X_RPBE, p->nspin);
  XC(func_init)(p->func_aux[1], XC_GGA_X_PW91, p->nspin);
}


static inline void 
func(const XC(func_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  static const FLOAT alpha = 1.0, beta = 19.0;

  FLOAT f1, df1dx, d2f1dx2;
  FLOAT f2, df2dx, d2f2dx2;
  FLOAT aux, den, fab, dfab, d2fab;

  XC(gga_x_rpbe_enhance)(p->func_aux[0], order, x, &f1, &df1dx, &d2f1dx2);
  XC(gga_x_pw91_enhance)(p->func_aux[1], order, x, &f2, &df2dx, &d2f2dx2);

  aux = exp(-(alpha*(x - beta)));
  den = 1.0 + aux;

  fab = 1.0/den;
  *f  = (1.0 - fab)*f1 + fab*f2;

  if(order < 1) return;

  dfab  = -alpha*aux/(den*den);
  *dfdx = dfab*(f1 - f2) + (1.0 - fab)*df1dx + fab*df2dx;

  if(order < 2) return;
  
  d2fab   = -alpha*alpha*aux*(2.0*aux - den)/(den*den*den);
  *d2fdx2 = d2fab*(f1 - f2) + 2.0* dfab*(df1dx - df2dx) + (1.0 - fab)*d2f1dx2 + fab*d2f2dx2;
}

#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_bpccac) = {
  XC_GGA_X_BPCCAC,
  XC_EXCHANGE,
  "BPCCAC (GRAC for the energy)",
  XC_FAMILY_GGA,
  "E Bremond, D Pilard, I Ciofini, H Chermette, C Adamo, and P Cortona, Theor Chem Acc 131, 1184 (2012)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_bpccac_init, 
  NULL, NULL,
  work_gga_x
};
