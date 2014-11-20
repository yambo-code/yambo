/*
 Copyright (C) 2008 Lara Ferrigni, Georg Madsen, M.A.L. Marques

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

#define XC_MGGA_X_M06_L         203 /* M06-Local functional of Minnesota */
#define XC_MGGA_X_M06_HF        216 /* M06-HF functional of Minnesota */
#define XC_MGGA_X_M06           217 /* M06 functional of Minnesota */

static const FLOAT a_m06l[12] = {
  0.3987756, 0.2548219, 0.3923994, -2.103655, -6.302147, 10.97615,
  30.97273,  -23.18489, -56.73480, 21.60364, 34.21814, -9.049762
};
static const FLOAT d_m06l[6] = {0.6012244, 0.004748822, -0.008635108, -0.000009308062, 0.00004482811, 0.0};

static const FLOAT a_m06hf[12] = {
   1.179732e-01, -1.066708e+00, -1.462405e-01,  7.481848e+00,  3.776679e+00, -4.436118e+01, 
  -1.830962e+01,  1.003903e+02,  3.864360e+01, -9.806018e+01, -2.557716e+01,  3.590404e+01
};
static const FLOAT d_m06hf[6] = {-1.179732e-01, -2.500000e-03, -1.180065e-02, 0.0, 0.0, 0.0};

static const FLOAT a_m06[12] = {
   5.877943e-01, -1.371776e-01,  2.682367e-01, -2.515898e+00, -2.978892e+00,  8.710679e+00,
   1.688195e+01, -4.489724e+00, -3.299983e+01, -1.449050e+01,  2.043747e+01,  1.256504e+01
};
static const FLOAT d_m06[6] = {1.422057e-01, 7.370319e-04, -1.601373e-02, 0.0, 0.0, 0.0};

typedef struct{
  const FLOAT *a, *d;
} mgga_x_m06l_params;


static void
mgga_x_m06l_init(XC(func_type) *p)
{
  mgga_x_m06l_params *params;

  assert(p!=NULL && p->params == NULL);
  p->params = malloc(sizeof(mgga_x_m06l_params));
  params = (mgga_x_m06l_params *)p->params;

  switch(p->info->number){
  case XC_MGGA_X_M06_L:
    params->a = a_m06l;
    params->d = d_m06l;
    break;
  case XC_MGGA_X_M06_HF:
    params->a = a_m06hf;
    params->d = d_m06hf;
    break;
  case XC_MGGA_X_M06:
    params->a = a_m06;
    params->d = d_m06;
    break;
  default:
    fprintf(stderr, "Internal error in mgga_x_m06l\n");
    exit(1);
  }

  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(sizeof(XC(func_type) *)*p->n_func_aux);
  p->func_aux[0] = (XC(func_type) *)  malloc(sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_GGA_X_PBE, p->nspin);
}


/* Eq. (8) */
void 
XC(mgga_series_w)(int order, int n, const FLOAT *a, FLOAT t, FLOAT *fw, FLOAT *dfwdt)
{
  FLOAT w, w_den, wp;
  int i;

  w_den = K_FACTOR_C + t;
  w = (K_FACTOR_C - t)/w_den;

  *fw = 0.0;
  if(order>0) *dfwdt = 0.0;
  wp = 1.0;
  for(i=0; i<n; i++){
    *fw += a[i]*wp;
    if(order>0) *dfwdt += i*a[i]*wp;

    wp *= w;
  }
  if (order>0){
    *dfwdt /= w;
    *dfwdt *= -2.0*K_FACTOR_C/(w_den*w_den);
  }
}


static void 
func(const XC(func_type) *pt, XC(mgga_work_x_t) *r)
{
  const FLOAT alpha = 0.00186726;   /* set alpha of Eq. (4) */

  mgga_x_m06l_params *params;
  FLOAT f_pbe, dfdx_pbe;
  FLOAT h, dhdx, dhdz, fw, dfwdt;

  assert(pt!=NULL && pt->params != NULL);
  params = (mgga_x_m06l_params *)pt->params;

  XC(gga_x_pbe_enhance)(pt->func_aux[0], r->order, r->x, &f_pbe, &dfdx_pbe, NULL);

  XC(mgga_series_w)(r->order, 12, params->a, r->t, &fw, &dfwdt);

  /* there is a factor if 2 in the definition of z, as in Theor. Chem. Account 120, 215 (2008) */
  XC(mgga_x_gvt4_func)(r->order, r->x, 2.0*(r->t - K_FACTOR_C), alpha, params->d, &h, &dhdx, &dhdz);

  /* A MINUS was missing in Eq. (7) of the paper */
  r->f = f_pbe*fw + h;

  if(r->order < 1) return;

  r->dfdx = dfdx_pbe*fw + dhdx;
  r->dfdt = f_pbe*dfwdt + 2.0*dhdz;
}


#include "work_mgga_x.c"

const XC(func_info_type) XC(func_info_mgga_x_m06_l) = {
  XC_MGGA_X_M06_L,
  XC_EXCHANGE,
  "M06-Local functional of Minnesota",
  XC_FAMILY_MGGA,
  "Y Zhao and DG Truhlar, JCP 125, 194101 (2006)\n"
  "Y Zhao and DG Truhlar, Theor. Chem. Account 120, 215 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_x_m06l_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_x,
};

const XC(func_info_type) XC(func_info_mgga_x_m06_hf) = {
  XC_MGGA_X_M06_HF,
  XC_EXCHANGE,
  "M06-HF functional of Minnesota",
  XC_FAMILY_MGGA,
  "Y Zhao and DG Truhlar, J. Phys. Chem. A 110, 13126 (2006)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_x_m06l_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_x,
};

const XC(func_info_type) XC(func_info_mgga_x_m06) = {
  XC_MGGA_X_M06,
  XC_EXCHANGE,
  "M06 functional of Minnesota",
  XC_FAMILY_MGGA,
  "Y Zhao and DG Truhlar, Theor. Chem. Acc. 120, 215 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_x_m06l_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_x,
};
