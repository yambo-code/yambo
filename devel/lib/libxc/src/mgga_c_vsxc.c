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

#define XC_MGGA_C_VSXC          232 /* VSxc from Van Voorhis and Scuseria (correlation part) */
#define XC_MGGA_C_M05           237 /* M05 functional of Minnesota */
#define XC_MGGA_C_M05_2X        238 /* M05-2X functional of Minnesota */
#define XC_MGGA_C_M06_L         233 /* M06-Local functional of Minnesota */
#define XC_MGGA_C_M06_HF        234 /* M06-HF functional of Minnesota */
#define XC_MGGA_C_M06           235 /* M06 functional of Minnesota */
#define XC_MGGA_C_M06_2X        236 /* M06-2X functional of Minnesota */

static const FLOAT vsxc_dab[6]  = { 7.035010e-01,  7.694574e-03,  5.152765e-02,  3.394308e-05, -1.269420e-03,  1.296118e-03};
static const FLOAT vsxc_dss[6]  = { 3.270912e-01, -3.228915e-02, -2.942406e-02,  2.134222e-03, -5.451559e-03,  1.577575e-02};

static const FLOAT m05_cab[5]   = { 1.00000e0,  3.78569e0, -14.15261e0, -7.46589e0, 17.94491e0};
static const FLOAT m05_css[5]   = { 1.00000e0,  3.77344e0, -26.04463e0, 30.69913e0, -9.22695e0};

static const FLOAT m052x_cab[5] = { 1.00000e0,  1.09297e0, -3.79171e0,  2.82810e0, -10.58909e0};
static const FLOAT m052x_css[5] = { 1.00000e0, -3.05430e0,  7.61854e0,  1.47665e0, -11.92365e0};

static const FLOAT m06l_cab[5]  = { 6.042374e-01,  1.776783e+02, -2.513252e+02,  7.635173e+01, -1.255699e+01};
static const FLOAT m06l_css[5]  = { 5.349466e-01,  5.396620e-01, -3.161217e+01,  5.149592e+01, -2.919613e+01};
static const FLOAT m06l_dab[6]  = { 3.957626e-01, -5.614546e-01,  1.403963e-02,  9.831442e-04, -3.577176e-03,  0.000000e+00};
static const FLOAT m06l_dss[6]  = { 4.650534e-01,  1.617589e-01,  1.833657e-01,  4.692100e-04, -4.990573e-03,  0.000000e+00};

static const FLOAT m06hf_cab[5] = { 1.674634e+00,  5.732017e+01,  5.955416e+01, -2.311007e+02,  1.255199e+02};
static const FLOAT m06hf_css[5] = { 1.023254e-01, -2.453783e+00,  2.913180e+01, -3.494358e+01,  2.315955e+01};
static const FLOAT m06hf_dab[6] = {-6.746338e-01, -1.534002e-01, -9.021521e-02, -1.292037e-03, -2.352983e-04,  0.000000e+00};
static const FLOAT m06hf_dss[6] = { 8.976746e-01, -2.345830e-01,  2.368173e-01, -9.913890e-04, -1.146165e-02,  0.000000e+00};

static const FLOAT m06_cab[5]   = { 3.741539e+00,  2.187098e+02, -4.531252e+02,  2.936479e+02, -6.287470e+01};
static const FLOAT m06_css[5]   = { 5.094055e-01, -1.491085e+00,  1.723922e+01, -3.859018e+01,  2.845044e+01};
static const FLOAT m06_dab[6]   = {-2.741539e+00, -6.720113e-01, -7.932688e-02,  1.918681e-03, -2.032902e-03,  0.000000e+00};
static const FLOAT m06_dss[6]   = { 4.905945e-01, -1.437348e-01,  2.357824e-01,  1.871015e-03, -3.788963e-03,  0.000000e+00};

static const FLOAT m062x_cab[5] = { 8.833596e-01,  3.357972e+01, -7.043548e+01,  4.978271e+01, -1.852891e+01};
static const FLOAT m062x_css[5] = { 3.097855e-01, -5.528642e+00,  1.347420e+01, -3.213623e+01,  2.846742e+01};
static const FLOAT m062x_dab[6] = { 1.166404e-01, -9.120847e-02, -6.726189e-02,  6.720580e-05,  8.448011e-04,  0.000000e+00};
static const FLOAT m062x_dss[6] = { 6.902145e-01,  9.847204e-02,  2.214797e-01, -1.968264e-03, -6.775479e-03,  0.000000e+00};

typedef struct{
  const FLOAT *cab, *css, *dab, *dss;
  FLOAT alpha_ab, alpha_ss, gamma_ab, gamma_ss;
} mgga_c_m06l_params;


static void 
mgga_c_vsxc_init(XC(func_type) *p)
{
  mgga_c_m06l_params *params;

  assert(p != NULL);

  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_LDA_C_PW_MOD, XC_POLARIZED);

  assert(p!=NULL && p->params == NULL);
  p->params = malloc(sizeof(mgga_c_m06l_params));
  params = (mgga_c_m06l_params *)p->params;

  switch(p->info->number){
  case XC_MGGA_C_VSXC:
    params->cab = params->css = NULL;
    params->alpha_ab = 0.00304966;
    params->dab = vsxc_dab;
    params->alpha_ss = 0.00515088;
    params->dss = vsxc_dss;
    break;
  case XC_MGGA_C_M05:
    params->gamma_ab = 0.0031;
    params->cab = m05_cab;
    params->gamma_ss = 0.06;
    params->css = m05_css;
    params->dab = params->dss = NULL;
    break;
  case XC_MGGA_C_M05_2X:
    params->gamma_ab = 0.0031;
    params->cab = m052x_cab;
    params->gamma_ss = 0.06;
    params->css = m052x_css;
    params->dab = params->dss = NULL;
    break;
  case XC_MGGA_C_M06_L:
    params->gamma_ab = 0.0031;
    params->cab = m06l_cab;
    params->gamma_ss = 0.06;
    params->css = m06l_css;
    params->alpha_ab = 0.00304966;
    params->dab = m06l_dab;
    params->alpha_ss = 0.00515088;
    params->dss = m06l_dss;
    break;
  case XC_MGGA_C_M06_HF:
    params->gamma_ab = 0.0031;
    params->cab = m06hf_cab;
    params->gamma_ss = 0.06;
    params->css = m06hf_css;
    params->alpha_ab = 0.00304966;
    params->dab = m06hf_dab;
    params->alpha_ss = 0.00515088;
    params->dss = m06hf_dss;
    break;
  case XC_MGGA_C_M06:
    params->gamma_ab = 0.0031;
    params->cab = m06_cab;
    params->gamma_ss = 0.06;
    params->css = m06_css;
    params->alpha_ab = 0.00304966;
    params->dab = m06_dab;
    params->alpha_ss = 0.00515088;
    params->dss = m06_dss;
    break;
  case XC_MGGA_C_M06_2X:
    params->gamma_ab = 0.0031;
    params->cab = m062x_cab;
    params->gamma_ss = 0.06;
    params->css = m062x_css;
    params->alpha_ab = 0.00304966;
    params->dab = m062x_dab;
    params->alpha_ss = 0.00515088;
    params->dss = m062x_dss;
    break;    
  default:
    fprintf(stderr, "Internal error in mgga_c_vsxc\n");
    exit(1);
  }  
}


static void 
func(const XC(func_type) *pt, XC(mgga_work_c_t) *r)
{
  static const FLOAT tmin = 0.5e-10;
  static const FLOAT sign[2] = {1.0, -1.0};

  mgga_c_m06l_params *params;
  XC(lda_work_t) LDA[3];
  FLOAT opz, dd, g, dgdx, d2gdx2, h, dhdx, dhdt, aux, x_tot, dx_totdxs[2], ddddxs, ddddts;
  int is;

  assert(pt!=NULL && pt->params != NULL);
  params = (mgga_c_m06l_params *)pt->params;

  /* first we get the parallel and perpendicular LDAS */
  XC(lda_stoll) (pt->func_aux[0], r->dens, r->zeta, r->order, LDA);

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

    if(r->dens*opz < 2.0*pt->info->min_dens) continue;

    if(params->dss == NULL){
      h = dhdx = dhdt = 0.0;
    }else{
      XC(mgga_x_gvt4_func)(r->order, r->xs[is], 2.0*(r->ts[is] - K_FACTOR_C), 
			   params->alpha_ss, params->dss, &h, &dhdx, &dhdt);
    }

    if(params->css == NULL){
      g = dgdx = 0.0;
    }else{
       XC(mgga_b97_func_g)(params->css, params->gamma_ss, r->xs[is], r->order, &g, &dgdx, &d2gdx2);
    }

    dd = (r->ts[is] > tmin) ? 1.0 - r->xs[is]*r->xs[is]/(8.0*r->ts[is]) : 0.0;

    r->f += LDA[is].zk*dd*(g + h);

    if(r->order < 1) continue;

    if(r->ts[is] > tmin){
      ddddxs = -2.0*r->xs[is]/(8.0*r->ts[is]);
      ddddts = r->xs[is]*r->xs[is]/(8.0*r->ts[is]*r->ts[is]);
    }else
      ddddxs = ddddts = 0.0;

    r->dfdrs     += LDA[is].dedrs*dd*(g + h);
    r->dfdz      += LDA[is].dedz *dd*(g + h);
    r->dfdxs[is] += LDA[is].zk*(ddddxs*(g + h) + dd*(dgdx + dhdx));
    r->dfdts[is] += LDA[is].zk*(ddddts*(g + h) + 2.0*dd*dhdt);
  }

  /* and now we add the opposite-spin contribution */
  aux   = r->xs[0]*r->xs[0] + r->xs[1]*r->xs[1];
  x_tot = SQRT(aux);

  if(params->dab == NULL){
    h = dhdx = dhdt = 0.0;
  }else{
    XC(mgga_x_gvt4_func)(r->order, x_tot, 2.0*(r->ts[0] + r->ts[1] - 2.0*K_FACTOR_C), 
			 params->alpha_ab, params->dab, &h, &dhdx, &dhdt);
  }

  if(params->cab == NULL){
    g = dgdx = 0.0;
  }else{
    XC(mgga_b97_func_g)(params->cab, params->gamma_ab, x_tot, r->order, &g, &dgdx, &d2gdx2);
  }

  r->f += LDA[2].zk*(g + h);

  if(r->order < 1) return;

  dx_totdxs[0] = r->xs[0]/x_tot;
  dx_totdxs[1] = r->xs[1]/x_tot;

  r->dfdrs    += LDA[2].dedrs*(g + h);
  r->dfdz     += LDA[2].dedz *(g + h);
  r->dfdxs[0] += LDA[2].zk*(dgdx + dhdx)*dx_totdxs[0];
  r->dfdxs[1] += LDA[2].zk*(dgdx + dhdx)*dx_totdxs[1];
  r->dfdts[0] += LDA[2].zk*dhdt*2.0;
  r->dfdts[1] += LDA[2].zk*dhdt*2.0;
}

#include "work_mgga_c.c"

XC(func_info_type) XC(func_info_mgga_c_m05) = {
  XC_MGGA_C_M05,
  XC_CORRELATION,
  "M05 functional of Minnesota",
  XC_FAMILY_MGGA,
  "Y Zhao, NE Schultz, and DG Truhlar, J. Chem. Phys. 123, 161103 (2005)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_c_vsxc_init,
  NULL, NULL, NULL,
  work_mgga_c,
};


XC(func_info_type) XC(func_info_mgga_c_m05_2x) = {
  XC_MGGA_C_M05_2X,
  XC_CORRELATION,
  "M05-2X functional of Minnesota",
  XC_FAMILY_MGGA,
  "Y Zhao, NE Schultz, and DG Truhlar, J. Chem. Theory Comput. 2, 364 (2006)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  mgga_c_vsxc_init,
  NULL, NULL, NULL,
  work_mgga_c,
};

const XC(func_info_type) XC(func_info_mgga_c_vsxc) = {
  XC_MGGA_C_VSXC,
  XC_CORRELATION,
  "VSXC (correlation part)",
  XC_FAMILY_MGGA,
  "T Van Voorhis and GE Scuseria, JCP 109, 400 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_c_vsxc_init,
  NULL,
  NULL, NULL,
  work_mgga_c,
};

const XC(func_info_type) XC(func_info_mgga_c_m06_l) = {
  XC_MGGA_C_M06_L,
  XC_CORRELATION,
  "M06-Local functional of Minnesota",
  XC_FAMILY_MGGA,
  "Y Zhao and DG Truhlar, JCP 125, 194101 (2006)\n"
  "Y Zhao and DG Truhlar, Theor. Chem. Account 120, 215 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_c_vsxc_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_c,
};

const XC(func_info_type) XC(func_info_mgga_c_m06_hf) = {
  XC_MGGA_C_M06_HF,
  XC_CORRELATION,
  "M06-HF functional of Minnesota",
  XC_FAMILY_MGGA,
  "Y Zhao and DG Truhlar, J. Phys. Chem. A 110, 13126 (2006)\n",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_c_vsxc_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_c,
};

const XC(func_info_type) XC(func_info_mgga_c_m06) = {
  XC_MGGA_C_M06,
  XC_CORRELATION,
  "M06 functional of Minnesota",
  XC_FAMILY_MGGA,
  "Theor. Chem. Acc. 120, 215 (2008)\n",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_c_vsxc_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_c,
};

const XC(func_info_type) XC(func_info_mgga_c_m06_2x) = {
  XC_MGGA_C_M06_2X,
  XC_CORRELATION,
  "M06-2X functional of Minnesota",
  XC_FAMILY_MGGA,
  "Theor. Chem. Acc. 120, 215 (2008)\n",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_c_vsxc_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_c,
};
