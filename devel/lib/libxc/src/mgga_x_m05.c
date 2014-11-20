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

#define XC_MGGA_X_M05          214 /* M05 functional of Minnesota */
#define XC_MGGA_X_M05_2X       215 /* M05-2X functional of Minnesota */
#define XC_MGGA_X_M06_2X       218 /* M06-2X functional of Minnesota */

static const FLOAT a_m05[12] = 
  {1.0, 0.08151, -0.43956, -3.22422, 2.01819, 8.79431, -0.00295,
   9.82029, -4.82351, -48.17574, 3.64802, 34.02248};

static const FLOAT a_m05_2x[12] =
  {1.0, -0.56833, -1.30057, 5.50070, 9.06402, -32.21075, -23.73298,
   70.22996, 29.88614, -60.25778, -13.22205, 15.23694};

static const FLOAT a_m06_2x[12] =
  {4.600000e-01, -2.206052e-01, -9.431788e-02,  2.164494e+00, -2.556466e+00, -1.422133e+01,
   1.555044e+01,  3.598078e+01, -2.722754e+01, -3.924093e+01,  1.522808e+01,  1.522227e+01};

typedef struct{
  int n;
  const FLOAT *a;
} mgga_x_m05_params;


static void
mgga_x_m05_init(XC(func_type) *p)
{
  mgga_x_m05_params *params;

  assert(p != NULL);

  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_GGA_X_PBE, p->nspin);

  assert(p->params == NULL);
  p->params = malloc(sizeof(mgga_x_m05_params));
  params = (mgga_x_m05_params *) (p->params);

  switch(p->info->number){
  case XC_MGGA_X_M05: 
    params->n = 12;
    params->a = a_m05;
    break;
  case XC_MGGA_X_M05_2X:
    params->n = 12;
    params->a = a_m05_2x;
    break;
  case XC_MGGA_X_M06_2X:
    params->n = 12;
    params->a = a_m06_2x;
    break;
  default:
    fprintf(stderr, "Internal error in mgga_x_m05\n");
    exit(1);
  }
}


static void 
func(const XC(func_type) *pt, XC(mgga_work_x_t) *r)
{
  mgga_x_m05_params *params;

  FLOAT e_f, e_dfdx, e_d2fdx2;
  FLOAT fw, dfwdt;

  assert(pt != NULL && pt->params != NULL);
  params = (mgga_x_m05_params *) (pt->params);
  
  XC(gga_x_pbe_enhance)(pt->func_aux[0], r->order, r->x, &e_f, &e_dfdx, &e_d2fdx2);
  
  XC(mgga_series_w)(r->order, params->n, params->a, r->t, &fw, &dfwdt);

  r->f = e_f*fw;

  if(r->order < 1) return;

  r->dfdx = e_dfdx*fw;
  r->dfdt = e_f*dfwdt;
  r->dfdu = 0.0;

  if(r->order < 2) return;

}


#include "work_mgga_x.c"


XC(func_info_type) XC(func_info_mgga_x_m05) = {
  XC_MGGA_X_M05,
  XC_EXCHANGE,
  "M05 functional of Minnesota",
  XC_FAMILY_MGGA,
  "Y Zhao, NE Schultz, and DG Truhlar, J. Chem. Phys. 123, 161103 (2005)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_x_m05_init,
  NULL, NULL, NULL,
  work_mgga_x,
};


XC(func_info_type) XC(func_info_mgga_x_m05_2x) = {
  XC_MGGA_X_M05_2X,
  XC_EXCHANGE,
  "M05-2X functional of Minnesota",
  XC_FAMILY_MGGA,
  "Y Zhao, NE Schultz, and DG Truhlar, J. Chem. Theory Comput. 2, 364 (2006)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_x_m05_init,
  NULL, NULL, NULL,
  work_mgga_x,
};

const XC(func_info_type) XC(func_info_mgga_x_m06_2x) = {
  XC_MGGA_X_M06_2X,
  XC_EXCHANGE,
  "M06-2X functional of Minnesota",
  XC_FAMILY_MGGA,
  "Y Zhao and DG Truhlar, Theor. Chem. Acc. 120, 215 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_x_m05_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_x,
};
