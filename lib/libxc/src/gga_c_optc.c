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

#define XC_GGA_C_OPTC       200 /* Optimized correlation functional of Cohen and Handy */

static void 
gga_c_optc_init(XC(func_type) *p)
{
  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  /* PW91 has always to be called polarized */
  XC(func_init)(p->func_aux[0], XC_GGA_C_PW91, XC_POLARIZED);
}


static inline void 
func(const XC(func_type) *p, XC(gga_work_c_t) *r)
{
  static FLOAT c1 = 1.1015, c2 = 0.6625;
  FLOAT opz, omz, copz, comz, o_opz, o_copz, o_omz, o_comz;

  XC(gga_work_c_t) f_par[2], f_anti;

  opz = 1.0 + r->zeta;
  omz = 1.0 - r->zeta;
  copz = CBRT(opz);
  comz = CBRT(omz);

  /* calculate the total part */
  f_anti.order = r->order;
  f_anti.rs    = r->rs;
  f_anti.zeta  = r->zeta;
  f_anti.xt    = r->xt;
  f_anti.xs[0] = r->xs[0];
  f_anti.xs[1] = r->xs[1];

  XC(gga_c_pw91_func) (p->func_aux[0],  &f_anti);

  /* now the spin up */
  if(opz > p->info->min_zeta){
    f_par[0].order = r->order;
    f_par[0].rs    = r->rs*M_CBRT2*copz;
    f_par[0].zeta  = 1.0;
    f_par[0].xt    = r->xs[0];
    f_par[0].xs[0] = r->xs[0];
    f_par[0].xs[1] = 0.0;

    XC(gga_c_pw91_func) (p->func_aux[0],  &(f_par[0]));
  }else{
    f_par[0].f = 0.0;
    if(r->order >= 1) f_par[0].dfdrs = f_par[0].dfdxt = f_par[0].dfdxs[0] = 0.0;
    if(r->order >= 2) f_par[0].d2fdrs2 = f_par[0].d2fdrsxt = f_par[0].d2fdrsxs[0] =
      f_par[0].d2fdxt2 = f_par[0].d2fdxtxs[0] = f_par[0].d2fdxs2[0] = 0.0;
  }


  /* now the spin down */
  if(omz > p->info->min_zeta){
    f_par[1].order = r->order;
    f_par[1].rs    = r->rs*M_CBRT2*comz;
    f_par[1].zeta  = -1.0;
    f_par[1].xt    = r->xs[1];
    f_par[1].xs[0] = 0.0;
    f_par[1].xs[1] = r->xs[1];

    XC(gga_c_pw91_func) (p->func_aux[0],  &(f_par[1]));
  }else{
    f_par[1].f = 0.0;
    if(r->order >= 1) f_par[1].dfdrs = f_par[1].dfdxt = f_par[1].dfdxs[0] = 0.0;
    if(r->order >= 2) f_par[1].d2fdrs2 = f_par[1].d2fdrsxt = f_par[1].d2fdrsxs[1] =
      f_par[1].d2fdxt2 = f_par[1].d2fdxtxs[1] = f_par[1].d2fdxs2[2] = 0.0;
  }
    

  /* now we add everything */
  
  r->f = c1*f_anti.f + (c2 - c1)*(f_par[0].f + f_par[1].f);

  if(r->order < 1) return;

  if(opz >= p->info->min_zeta){
    o_opz  = 1.0/opz;
    o_copz = 1.0/copz;
  }else
    o_opz = o_copz = 0.0;

  if(omz >= p->info->min_zeta){
    o_omz  = 1.0/omz;
    o_comz = 1.0/comz;
  }else
    o_omz = o_comz = 0.0;

  r->dfdrs    = c1*f_anti.dfdrs + (c2 - c1)*M_CBRT2*(f_par[0].dfdrs*copz + f_par[1].dfdrs*comz);
  r->dfdz     = c1*f_anti.dfdz  + (c2 - c1)*M_CBRT2*r->rs/3.0*(f_par[0].dfdrs*o_copz*o_copz - f_par[1].dfdrs*o_comz*o_comz);
  r->dfdxt    = c1*f_anti.dfdxt;
  r->dfdxs[0] = c1*f_anti.dfdxs[0] + (c2 - c1)*(f_par[0].dfdxt + f_par[0].dfdxs[0]);
  r->dfdxs[1] = c1*f_anti.dfdxs[1] + (c2 - c1)*(f_par[1].dfdxt + f_par[1].dfdxs[1]);

  if(r->order < 2) return;

  r->d2fdrs2     = c1*f_anti.d2fdrs2 + (c2 - c1)*M_CBRT2*M_CBRT2*(f_par[0].d2fdrs2*copz*copz + f_par[1].d2fdrs2*comz*comz);
  r->d2fdrsz     = c1*f_anti.d2fdrsz + (c2 - c1)*M_CBRT2/3.0*
    (f_par[0].dfdrs*o_copz*o_copz - f_par[1].dfdrs*o_comz*o_comz +
     M_CBRT2*(f_par[0].d2fdrs2*o_copz - f_par[1].d2fdrs2*o_comz));
  r->d2fdrsxt    = c1*f_anti.d2fdrsxt;
  r->d2fdrsxs[0] = c1*f_anti.d2fdrsxs[0] + (c2 - c1)*M_CBRT2*(f_par[0].d2fdrsxt + f_par[0].d2fdrsxs[0])*copz;
  r->d2fdrsxs[1] = c1*f_anti.d2fdrsxs[1] + (c2 - c1)*M_CBRT2*(f_par[1].d2fdrsxt + f_par[1].d2fdrsxs[1])*comz;
  r->d2fdz2      = c1*f_anti.d2fdz2 + (c2 - c1)*M_CBRT2*r->rs/3.0*
    (-2.0/3.0*(f_par[0].dfdrs*o_opz*o_copz*o_copz + f_par[1].dfdrs*o_omz*o_comz*o_comz) + 
     M_CBRT2*r->rs/3.0*(f_par[0].d2fdrs2*o_opz*o_copz + f_par[1].d2fdrs2*o_omz*o_omz));
  r->d2fdzxt     = c1*f_anti.d2fdzxt;
  r->d2fdzxs[0]  = c1*f_anti.d2fdzxs[0]  + (c2 - c1)*M_CBRT2*r->rs/3.0*(f_par[0].d2fdrsxt + f_par[0].d2fdrsxs[0])*o_copz*o_copz;
  r->d2fdzxs[1]  = c1*f_anti.d2fdzxs[1]  + (c2 - c1)*M_CBRT2*r->rs/3.0*(f_par[1].d2fdrsxt + f_par[1].d2fdrsxs[1])*o_comz*o_comz;
  r->d2fdxt2     = c1*f_anti.d2fdxt2;
  r->d2fdxtxs[0] = c1*f_anti.d2fdxtxs[0];
  r->d2fdxtxs[1] = c1*f_anti.d2fdxtxs[1];
  r->d2fdxs2[0]  = c1*f_anti.d2fdxs2[0]  + (c2 - c1)*(f_par[0].d2fdxt2 + 2.0*f_par[0].d2fdxtxs[0] + f_par[0].d2fdxs2[0]);
  r->d2fdxs2[1]  = c1*f_anti.d2fdxs2[1];
  r->d2fdxs2[2]  = c1*f_anti.d2fdxs2[2]  + (c2 - c1)*(f_par[1].d2fdxt2 + 2.0*f_par[1].d2fdxtxs[1] + f_par[1].d2fdxs2[2]);
}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_optc) = {
  XC_GGA_C_OPTC,
  XC_CORRELATION,
  "Optimized correlation functional of Cohen and Handy",
  XC_FAMILY_GGA,
  "AJ Cohen and NC Handy, Mol. Phys. 99, 607-615 (2001)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-26, 1e-32, 0.0, 1e-32, /* densities smaller than 1e-26 give rise to NaNs */
  gga_c_optc_init,
  NULL, NULL,
  work_gga_c,
};
