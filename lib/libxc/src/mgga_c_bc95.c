/*
 Copyright (C) 2008 M.A.L. Marques

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

#define XC_MGGA_C_BC95          240 /* Becke correlation 95 */

typedef struct{
  FLOAT css, copp;
} mgga_c_bc95_params;


static void 
mgga_c_bc95_init(XC(func_type) *p)
{
  assert(p!=NULL && p->params == NULL);
  p->params = malloc(sizeof(mgga_c_bc95_params));

  XC(mgga_c_bc95_set_params)(p, 0.038, 0.0031);

  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_LDA_C_PW_MOD, XC_POLARIZED);
}


void 
XC(mgga_c_bc95_set_params)(XC(func_type) *p, FLOAT css, FLOAT copp)
{
  mgga_c_bc95_params *params;

  assert(p != NULL && p->params != NULL);
  params = (mgga_c_bc95_params *) (p->params);

  params->css  = css;
  params->copp = copp;
}


static void 
func(const XC(func_type) *p, XC(mgga_work_c_t) *r)
{
  static const FLOAT sign[2] = {1.0, -1.0};
  mgga_c_bc95_params *params;

  XC(lda_work_t) LDA[3];
  FLOAT opz, dd, g, g2, dgdxs, ddddxs, ddddts;
  int is;

  assert(p != NULL && p->params != NULL);
  params = (mgga_c_bc95_params *) (p->params);  

  /* first we get the parallel and perpendicular LDAS */
  XC(lda_stoll) (p->func_aux[0], r->dens, r->zeta, r->order, LDA);

  /* initialize to zero */
  r->f = 0.0;
  if(r->order >= 1){
    r->dfdrs = r->dfdz = r->dfdxs[0] = r->dfdxs[1] = r->dfdxt = 0.0;
    r->dfdus[0] = r->dfdus[1] = r->dfdts[0] = r->dfdts[1] = 0.0;
  }
  if(r->order >= 2){
    r->d2fdrs2 = r->d2fdrsz = r->d2fdrsxt = r->d2fdrsxs[0] = r->d2fdrsxs[1] = 0.0;
    r->d2fdz2 = r->d2fdzxt = r->d2fdzxs[0] = r->d2fdzxs[1] = r->d2fdxt2 = 0.0;
    r->d2fdxtxs[0] = r->d2fdxtxs[1] = r->d2fdxs2[0] = r->d2fdxs2[1] = r->d2fdxs2[2] = 0.0;
  }

  /* now we calculate the g functions for exchange and parallel correlation */
  for(is = 0; is < 2; is++){
    opz   = 1.0 + sign[is]*r->zeta;

    if(r->dens*opz < 2.0*p->info->min_dens) continue;

    g  = 1.0 + params->css*r->xs[is]*r->xs[is];
    g2 = g*g;

    dd = (r->ts[is] - r->xs[is]*r->xs[is]/8.0)/K_FACTOR_C;

    r->f += LDA[is].zk*dd/g2;

    if(r->order < 1) continue;

    dgdxs  = 2.0*params->css*r->xs[is];
    ddddxs = -r->xs[is]/(4.0*K_FACTOR_C);
    ddddts = 1.0/K_FACTOR_C;

    r->dfdrs     += LDA[is].dedrs*dd/g2;
    r->dfdz      += LDA[is].dedz *dd/g2;
    r->dfdxs[is] += LDA[is].zk*(ddddxs*g - 2.0*dd*dgdxs)/(g*g2);
    r->dfdts[is] += LDA[is].zk*ddddts/g2;
  }

  /* and now we add the opposite-spin contribution */
  g     = 1.0 + params->copp*(r->xs[0]*r->xs[0] + r->xs[1]*r->xs[1]);
  g2    = g*g;

  r->f += LDA[2].zk/g;

  if(r->order < 1) return;

  r->dfdrs    +=  LDA[2].dedrs/g;
  r->dfdz     +=  LDA[2].dedz/g;
  r->dfdxs[0] += -LDA[2].zk*2.0*params->copp*r->xs[0]/g2;
  r->dfdxs[1] += -LDA[2].zk*2.0*params->copp*r->xs[1]/g2;
}

#include "work_mgga_c.c"

XC(func_info_type) XC(func_info_mgga_c_bc95) = {
  XC_MGGA_C_BC95,
  XC_CORRELATION,
  "Becke correlation 95",
  XC_FAMILY_MGGA,
  "AD Becke, J. Chem. Phys. 104, 1040 (1996)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_c_bc95_init,
  NULL, NULL, NULL,
  work_mgga_c,
};
