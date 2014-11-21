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

#define XC_MGGA_C_M08_HX       78 /* M08-HX functional of Minnesota  */
#define XC_MGGA_C_M08_SO       77 /* M08-SO functional of Minnesota  */
#define XC_MGGA_C_M11          76 /* M11 functional of Minnesota     */
#define XC_MGGA_C_M11_L        75 /* M11-L functional of Minnesota   */
#define XC_MGGA_C_MN12_L       74 /* MN12-L functional of Minnesota  */
#define XC_MGGA_C_MN12_SX      73 /* MN12-SX functional of Minnesota */


static const FLOAT a_m08_hx[12] = {
   1.0000000e+00, -4.0661387e-01, -3.3232530e+00,  1.5540980e+00,  4.4248033e+01, -8.4351930e+01,
  -1.1955581e+02,  3.9147081e+02,  1.8363851e+02, -6.3268223e+02, -1.1297403e+02,  3.3629312e+02
};
static const FLOAT b_m08_hx[12] = {
   1.3812334e+00, -2.4683806e+00, -1.1901501e+01, -5.4112667e+01,  1.0055846e+01,  1.4800687e+02,
   1.1561420e+02,  2.5591815e+02,  2.1320772e+02, -4.8412067e+02, -4.3430813e+02,  5.6627964e+01
};

static const FLOAT a_m08_so[12] = {
   1.0000000e+00,  0.0000000e+00, -3.9980886e+00,  1.2982340e+01,  1.0117507e+02, -8.9541984e+01,
  -3.5640242e+02,  2.0698803e+02,  4.6037780e+02, -2.4510559e+02, -1.9638425e+02,  1.1881459e+02
};
static const FLOAT b_m08_so[12] = {
   1.0000000e+00, -4.4117403e+00, -6.4128622e+00,  4.7583635e+01,  1.8630053e+02, -1.2800784e+02,
  -5.5385258e+02,  1.3873727e+02,  4.1646537e+02, -2.6626577e+02,  5.6676300e+01,  3.1673746e+02
};

static const FLOAT a_m11[12] = {
   1.0000000e+00,  0.0000000e+00, -3.8933250e+00, -2.1688455e+00,  9.3497200e+00, -1.9845140e+01,
   2.3455253e+00,  7.9246513e+01,  9.6042757e+00, -6.7856719e+01, -9.1841067e+00,  0.0000000e+00
};
static const FLOAT b_m11[12] = {
   7.2239798e-01,  4.3730564e-01, -1.6088809e+01, -6.5542437e+01,  3.2057230e+01,  1.8617888e+02,
   2.0483468e+01, -7.0853739e+01,  4.4483915e+01, -9.4484747e+01, -1.1459868e+02,  0.0000000e+00
};

static const FLOAT a_m11_l[12] = {
   1.000000e+00,  0.000000e+00,  2.750880e+00, -1.562287e+01,  9.363381e+00,  2.141024e+01,
  -1.424975e+01, -1.134712e+01,  1.022365e+01,  0.000000e+00,  0.000000e+00,  0.000000e+00
};
static const FLOAT b_m11_l[12] = {
   1.000000e+00, -9.082060e+00,  6.134682e+00, -1.333216e+01, -1.464115e+01,  1.713143e+01,
   2.480738e+00, -1.007036e+01, -1.117521e-01,  0.000000e+00,  0.000000e+00,  0.000000e+00
};

static const FLOAT a_mn12_l[12] = {
   8.844610e-01, -2.202279e-01,  5.701372e+00, -2.562378e+00, -9.646827e-01,  1.982183e-01,
   1.019976e+01,  9.789352e-01, -1.512722e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00
};
static const FLOAT b_mn12_l[12] = {
   5.323948e-01, -5.831909e+00,  3.882386e+00,  5.878488e+00,  1.493228e+01, -1.374636e+01,
  -8.492327e+00, -2.486548e+00, -1.822346e+01,  0.000000e+00,  0.000000e+00,  0.000000e+00
};

static const FLOAT a_mn12_sx[12] = {
   7.171161e-01, -2.380914e+00,  5.793565e+00, -1.243624e+00,  1.364920e+01, -2.110812e+01,
  -1.598767e+01,  1.429208e+01,  6.149191e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00
};
static const FLOAT b_mn12_sx[12] = {
   4.663699e-01, -9.110685e+00,  8.705051e+00, -1.813949e+00, -4.147211e-01, -1.021527e+01,
   8.240270e-01,  4.993815e+00, -2.563930e+01,  0.000000e+00,  0.000000e+00,  0.000000e+00
};


typedef struct{
  const FLOAT *a, *b;
} mgga_c_m08_params;


static void
mgga_c_m08_init(XC(func_type) *p)
{
  mgga_c_m08_params *params;

  assert(p != NULL);

  p->n_func_aux  = 2;
  p->func_aux    = (XC(func_type) **) malloc(2*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));
  p->func_aux[1] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_LDA_C_PW_MOD, p->nspin);
  XC(func_init)(p->func_aux[1], XC_GGA_C_PBE, p->nspin);

  assert(p->params == NULL);
  p->params = malloc(sizeof(mgga_c_m08_params));
  params = (mgga_c_m08_params *) (p->params);

  switch(p->info->number){
  case XC_MGGA_C_M08_HX: 
    params->a = a_m08_hx;
    params->b = b_m08_hx;
    break;
  case XC_MGGA_C_M08_SO:
    params->a = a_m08_so;
    params->b = b_m08_so;
    break;
  case XC_MGGA_C_M11:
    params->a = a_m11;
    params->b = b_m11;
    break;
  case XC_MGGA_C_M11_L:
    params->a = a_m11_l;
    params->b = b_m11_l;
    break;
  case XC_MGGA_C_MN12_L:
    params->a = a_mn12_l;
    params->b = b_mn12_l;
    break;
  case XC_MGGA_C_MN12_SX:
    params->a = a_mn12_sx;
    params->b = b_mn12_sx;
    break;
  default:
    fprintf(stderr, "Internal error in mgga_c_m08\n");
    exit(1);
  }
}


static void 
func(const XC(func_type) *pt, XC(mgga_work_c_t) *r)
{
  mgga_c_m08_params *params;

  XC(gga_work_c_t) pbe;
  XC(lda_work_t) pw;
  FLOAT t, dtdz, dtdts[2], opz, omz, opz13, omz13, opz23, omz23;
  FLOAT fw1, fw2, dfw1dt, dfw2dt;

  assert(pt != NULL && pt->params != NULL);
  params = (mgga_c_m08_params *) (pt->params);
  
  pw.order = r->order;
  pw.rs[0] = SQRT(r->rs);
  pw.rs[1] = r->rs;
  pw.rs[2] = r->rs*r->rs;
  pw.zeta  = r->zeta;
  XC(lda_c_pw_func)(pt->func_aux[0], &pw);

  pbe.order = r->order;
  pbe.rs    = r->rs;
  pbe.zeta  = r->zeta;
  pbe.xt    = r->xt;
  pbe.xs[0] = r->xs[0];
  pbe.xs[1] = r->xs[1];
  XC(gga_c_pbe_func)(pt->func_aux[1], &pbe);

  opz = 1.0 + r->zeta;
  omz = 1.0 - r->zeta;

  opz13 = CBRT(opz); opz23 = opz13*opz13;
  omz13 = CBRT(omz); omz23 = omz13*omz13;
  
  /* the 0.5 was chosen to get the right K_FACTOR_C in mgga_series_w */
  t = 0.5*(r->ts[0]*opz*opz23 + r->ts[1]*omz*omz23);
  
  XC(mgga_series_w)(r->order, 12, params->a, t, &fw1, &dfw1dt);
  XC(mgga_series_w)(r->order, 12, params->b, t, &fw2, &dfw2dt);

  r->f = fw1*pw.zk + fw2*pbe.f;

  if(r->order < 1) return;

  dtdz     = (5.0/6.0)*(r->ts[0]*opz23 - r->ts[1]*omz23);
  dtdts[0] = 0.5*opz*opz23;
  dtdts[1] = 0.5*omz*omz23;

  r->dfdrs    = fw1*pw.dedrs + fw2*pbe.dfdrs;
  r->dfdz     = fw1*pw.dedz  + fw2*pbe.dfdz + (dfw1dt*pw.zk + dfw2dt*pbe.f)*dtdz;
  r->dfdxt    = fw2*pbe.dfdxt;
  r->dfdxs[0] = fw2*pbe.dfdxs[0];
  r->dfdxs[1] = fw2*pbe.dfdxs[1];
  r->dfdus[0] = 0.0;
  r->dfdus[1] = 0.0;
  r->dfdts[0] = (dfw1dt*pw.zk + dfw2dt*pbe.f)*dtdts[0];
  r->dfdts[1] = (dfw1dt*pw.zk + dfw2dt*pbe.f)*dtdts[1];

  if(r->order < 2) return;

}


#include "work_mgga_c.c"


XC(func_info_type) XC(func_info_mgga_c_m08_hx) = {
  XC_MGGA_C_M08_HX,
  XC_EXCHANGE,
  "M08-HX functional of Minnesota",
  XC_FAMILY_MGGA,
  "Y Zhao and DG Truhlar, J. Chem. Theory Comput. 4, 1849-1868 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_c_m08_init,
  NULL, NULL, NULL,
  work_mgga_c,
};

XC(func_info_type) XC(func_info_mgga_c_m08_so) = {
  XC_MGGA_C_M08_SO,
  XC_EXCHANGE,
  "M08-SO functional of Minnesota",
  XC_FAMILY_MGGA,
  "Y Zhao and DG Truhlar, J. Chem. Theory Comput. 4, 1849-1868 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_c_m08_init,
  NULL, NULL, NULL,
  work_mgga_c,
};

XC(func_info_type) XC(func_info_mgga_c_m11) = {
  XC_MGGA_C_M11,
  XC_EXCHANGE,
  "M11 functional of Minnesota",
  XC_FAMILY_MGGA,
  "R Peverati, and DG Truhlar, J. Phys. Chem. Lett. 2, 2810 (2011)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_c_m08_init,
  NULL, NULL, NULL,
  work_mgga_c,
};

XC(func_info_type) XC(func_info_mgga_c_m11_l) = {
  XC_MGGA_C_M11_L,
  XC_EXCHANGE,
  "M11-L functional of Minnesota",
  XC_FAMILY_MGGA,
  "R Peverati, and DG Truhlar, J. Phys. Chem. Lett. 3, 117 (2012)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_c_m08_init,
  NULL, NULL, NULL,
  work_mgga_c,
};

XC(func_info_type) XC(func_info_mgga_c_mn12_l) = {
  XC_MGGA_C_MN12_L,
  XC_CORRELATION,
  "MN12-L functional of Minnesota",
  XC_FAMILY_MGGA,
  "R Peverati and DG Truhlar, Phys. Chem. Chem. Phys. 14, 13171-13174 (2012)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_c_m08_init,
  NULL, NULL, NULL,
  work_mgga_c,
};

XC(func_info_type) XC(func_info_mgga_c_mn12_sx) = {
  XC_MGGA_C_MN12_SX,
  XC_CORRELATION,
  "MN12-SX functional of Minnesota",
  XC_FAMILY_MGGA,
  "R Peverati and DG Truhlar, Phys. Chem. Chem. Phys. 14, accepted (2012)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_c_m08_init,
  NULL, NULL, NULL,
  work_mgga_c,
};
