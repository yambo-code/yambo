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

#define XC_GGA_C_WI0 153 /* Wilson & Ivanov initial version */
#define XC_GGA_C_WI  148 /* Wilson & Ivanov */

static void 
gga_c_wi_init(XC(func_type) *p)
{
  switch(p->info->number){
  case XC_GGA_C_WI0: p->func = 0;  break;
  case XC_GGA_C_WI:  p->func = 1;  break;
  default:
    fprintf(stderr, "Internal error in gga_c_wi\n");
    exit(1);
  }
}


static inline void 
func(const XC(func_type) *p, XC(gga_work_c_t) *r)
{
  static struct {
    FLOAT a, b, c, d, k;
  } *par, wi_par[2] = {{-0.44,    0.0032407, 7.8,  0.0073, 0.000311},
		       {-0.00652, 0.0007,    0.21, 0.002,  0.001}};

  FLOAT xt0, xt2, xt52, xt72, cnst_rs, num, den;

  par = &(wi_par[p->func]);

  cnst_rs = CBRT(4.0*M_PI/3.0);
  xt2     = r->xt*r->xt;
  xt0     = sqrt(r->xt);
  xt52    = xt2*xt0;
  xt72    = r->xt*xt52;

  num = par->a + par->b*xt2*exp(-par->k*xt2);
  den = par->c + r->rs*(1.0 + par->d*cnst_rs*xt72);

  r->f  = num/den;

  if(r->order < 1) return;

  r->dfdrs    = -(1.0 + par->d*cnst_rs*xt72)*r->f/den;
  r->dfdz     = 0.0;
  r->dfdxt    = (-7.0/2.0*par->d*cnst_rs*r->rs*xt52*r->f + 2.0*par->b*r->xt*(1.0 - par->k*xt2)*exp(-par->k*xt2))/den;
  r->dfdxs[0] = 0.0;
  r->dfdxs[1] = 0.0;

  if(r->order < 2) return;

  r->d2fdrs2     = -2.0*(1.0 + par->d*cnst_rs*xt72)*r->dfdrs/den;
  r->d2fdrsz     = 0.0;
  r->d2fdrsxt    = -7.0*par->d*cnst_rs*r->rs*xt52*r->dfdrs/den - 7.0/2.0*par->d*cnst_rs*xt52*r->f/den
    - 2.0*par->b*r->xt*exp(-par->k*xt2)*(1.0 - par->k*xt2)*(1.0 + par->d*cnst_rs*xt72)/(den*den);
  r->d2fdrsxs[0] = 0.0;
  r->d2fdrsxs[1] = 0.0;
  r->d2fdz2      = 0.0;
  r->d2fdzxt     = 0.0;
  r->d2fdzxs[0]  = 0.0;
  r->d2fdzxs[1]  = 0.0;
  r->d2fdxt2     = (49.0/2.0*par->d*cnst_rs*r->rs*xt2*xt2/den - 35.0/4.0*xt0)*r->f*par->d*cnst_rs*r->rs*r->xt/den
    + (-7.0*par->d*cnst_rs*r->rs*xt72*(1.0 - par->k*xt2)/den + 1.0 - 
       5.0*par->k*xt2 + 2.0*par->k*par->k*xt2*xt2)*2.0*par->b*exp(-par->k*xt2)/den;
  r->d2fdxtxs[0] = 0.0;
  r->d2fdxtxs[1] = 0.0;
  r->d2fdxs2[0]  = 0.0;
  r->d2fdxs2[1]  = 0.0;
  r->d2fdxs2[2]  = 0.0;
}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_wi0) = {
  XC_GGA_C_WI0,
  XC_CORRELATION,
  "Wilson & Ivanov initial version",
  XC_FAMILY_GGA,
  "LC Wilson & S Ivanov, Int. J. Quantum Chem. 69, 523-532 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_c_wi_init,
  NULL, NULL,
  work_gga_c
};

const XC(func_info_type) XC(func_info_gga_c_wi) = {
  XC_GGA_C_WI,
  XC_CORRELATION,
  "Wilson & Ivanov",
  XC_FAMILY_GGA,
  "LC Wilson & S Ivanov, Int. J. Quantum Chem. 69, 523-532 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_c_wi_init,
  NULL, NULL,
  work_gga_c
};
