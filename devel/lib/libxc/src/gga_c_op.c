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

#define XC_GGA_C_OP_B88      87 /* one-parameter progressive functional (B88 version)     */
#define XC_GGA_C_OP_PBE      86 /* one-parameter progressive functional (PBE version)     */
#define XC_GGA_C_OP_G96      85 /* one-parameter progressive functional (G96 version)     */
#define XC_GGA_C_OP_XALPHA   84 /* one-parameter progressive functional (XALPHA version)  */

typedef struct{
  FLOAT qOPab;
  void (*enhancement_factor)
    (const XC(func_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);
} gga_c_op_params;


static void gga_c_op_init(XC(func_type) *p)
{
  gga_c_op_params *params;

  assert(p != NULL && p->params == NULL);
  p->params = malloc(sizeof(gga_c_op_params));
  params = (gga_c_op_params *) (p->params);

  if(p->info->number != XC_GGA_C_OP_XALPHA){
    p->n_func_aux  = 1;
    p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
    p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));
  }

  switch(p->info->number){
  case XC_GGA_C_OP_B88:
    XC(func_init) (p->func_aux[0], XC_GGA_X_B88, XC_POLARIZED);
    params->enhancement_factor = XC(gga_x_b88_enhance);
    params->qOPab = 2.3670;
    break;
  case XC_GGA_C_OP_PBE:
    XC(func_init) (p->func_aux[0], XC_GGA_X_PBE, XC_POLARIZED);
    params->enhancement_factor = XC(gga_x_pbe_enhance);
    params->qOPab = 2.3789;
    break;
  case XC_GGA_C_OP_G96:
    XC(func_init) (p->func_aux[0], XC_GGA_X_G96, XC_POLARIZED);
    params->enhancement_factor = XC(gga_x_g96_enhance);
    params->qOPab = 2.3638;
    break;
  case XC_GGA_C_OP_XALPHA:
    params->enhancement_factor = NULL;
    params->qOPab = 2.5654;
    break;
  default:
    fprintf(stderr, "Internal error in gga_c_op\n");
    exit(1);
  }
}

static inline void 
func(const XC(func_type) *p, XC(gga_work_c_t) *r)
{
  static const FLOAT a1 = 1.5214, a2 = 0.5764, b1 = 1.1284, b2 = 0.3183;

  gga_c_op_params *params;
  FLOAT eu_f, eu_dfdx, eu_d2fdx2, ed_f, ed_dfdx, ed_d2fdx2;
  FLOAT cnst, cnst13, pref, opz, omz, opz13, omz13, beta_num, beta_den, beta, beta2, f_num, f_den;
  FLOAT dbeta_numdz, dbeta_numdxs[2], dbeta_dendz, dbeta_dendxs[2], dbetadz, dbetadxs[2];
  FLOAT df_numdz, df_numdrs, df_numdbeta, df_dendrs, df_dendbeta, dfdbeta;

  assert(p != NULL && p->params != NULL);
  params = (gga_c_op_params *) (p->params);

  if(ABS(r->zeta) > 1.0 - p->info->min_zeta){
    r->f = 0.0;
  }else{
    /* call enhancement factor */
    if(p->info->number != XC_GGA_C_OP_XALPHA){
      params->enhancement_factor(p->func_aux[0], r->order, r->xs[0], &eu_f, &eu_dfdx, &eu_d2fdx2);
      params->enhancement_factor(p->func_aux[0], r->order, r->xs[1], &ed_f, &ed_dfdx, &ed_d2fdx2);
    }else{
      eu_f = ed_f = 1.0;
      eu_dfdx = ed_dfdx = 0.0;
      eu_d2fdx2 = ed_d2fdx2 = 0.0;
    }
    
    cnst   = 4.0*M_PI/3.0;
    cnst13 = CBRT(cnst);
    pref   = params->qOPab*2.0*X_FACTOR_C/(M_CBRT2*cnst13);
    
    opz = 1.0 + r->zeta;
    omz = 1.0 - r->zeta;

    opz13 = POW(opz, 1.0/3.0);
    omz13 = POW(omz, 1.0/3.0);
    
    beta_num = pref * opz13*omz13 * eu_f*ed_f;
    beta_den = opz13*eu_f + omz13*ed_f;
    beta     = beta_num/beta_den;
    beta2    = beta*beta;
    
    f_num = -opz*omz*(a1*beta + a2*r->rs);
    f_den = 4.0*cnst*beta2*(beta2 + b1*r->rs*beta + b2*r->rs*r->rs);
    
    r->f = f_num/f_den;
  }

  if(r->order < 1) return;

  if(ABS(r->zeta) > 1.0 - p->info->min_zeta){
    r->dfdrs = r->dfdz = r->dfdxt = r->dfdxs[0] = r->dfdxs[1] = 0.0;
  }else{
    dbeta_numdz     = pref * (-2.0*r->zeta/(3.0*opz13*opz13*omz13*omz13)) * eu_f*ed_f;
    dbeta_numdxs[0] = pref * opz13*omz13 * eu_dfdx*ed_f;
    dbeta_numdxs[1] = pref * opz13*omz13 * eu_f*ed_dfdx;

    dbeta_dendz     = eu_f/(3.0*opz13*opz13) - ed_f/(3.0*omz13*omz13);
    dbeta_dendxs[0] = opz13*eu_dfdx;
    dbeta_dendxs[1] = omz13*ed_dfdx;

    dbetadz     = (    dbeta_numdz*beta_den - beta_num*dbeta_dendz    )/(beta_den*beta_den);
    dbetadxs[0] = (dbeta_numdxs[0]*beta_den - beta_num*dbeta_dendxs[0])/(beta_den*beta_den);
    dbetadxs[1] = (dbeta_numdxs[1]*beta_den - beta_num*dbeta_dendxs[1])/(beta_den*beta_den);
    
    df_numdz    = 2.0*r->zeta*(a1*beta + a2*r->rs);
    df_numdrs   = -opz*omz*a2;
    df_numdbeta = -opz*omz*a1;

    df_dendrs   = 4.0*cnst*beta2*(b1*beta + 2.0*b2*r->rs);
    df_dendbeta = 4.0*cnst*beta*(4.0*beta2 + 3.0*b1*r->rs*beta + 2.0*b2*r->rs*r->rs);

    dfdbeta     = (df_numdbeta*f_den - f_num*df_dendbeta)/(f_den*f_den);

    r->dfdrs    = (df_numdrs*f_den - f_num*df_dendrs)/(f_den*f_den);
    r->dfdz     = df_numdz/f_den + dfdbeta*dbetadz;
    r->dfdxt    = 0.0;
    r->dfdxs[0] = dfdbeta*dbetadxs[0];
    r->dfdxs[1] = dfdbeta*dbetadxs[1];
  }
  
  if(r->order < 2) return;

}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_op_b88) = {
  XC_GGA_C_OP_B88,
  XC_CORRELATION,
  "one-parameter progressive functional (B88 version)",
  XC_FAMILY_GGA,
  "T Tsuneda, T Suzumura, and K Hirao, J. Chem. Phys. 110, 10664 (1999)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-12, 1e-32, 0.0, 1e-10,
  gga_c_op_init,
  NULL, NULL,
  work_gga_c,
};

const XC(func_info_type) XC(func_info_gga_c_op_pbe) = {
  XC_GGA_C_OP_PBE,
  XC_CORRELATION,
  "one-parameter progressive functional (PBE version)",
  XC_FAMILY_GGA,
  "T Tsuneda, T Suzumura, and K Hirao, J. Chem. Phys. 111, 5656 (1999)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-12, 1e-32, 0.0, 1e-10,
  gga_c_op_init,
  NULL, NULL,
  work_gga_c,
};

const XC(func_info_type) XC(func_info_gga_c_op_g96) = {
  XC_GGA_C_OP_G96,
  XC_CORRELATION,
  "one-parameter progressive functional (G96 version)",
  XC_FAMILY_GGA,
  "T Tsuneda, T Suzumura, and K Hirao, J. Chem. Phys. 111, 5656 (1999)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-12, 1e-32, 0.0, 1e-10,
  gga_c_op_init,
  NULL, NULL,
  work_gga_c,
};

const XC(func_info_type) XC(func_info_gga_c_op_xalpha) = {
  XC_GGA_C_OP_XALPHA,
  XC_CORRELATION,
  "one-parameter progressive functional (Xalpha version)",
  XC_FAMILY_GGA,
  "T Tsuneda, T Suzumura, and K Hirao, J. Chem. Phys. 111, 5656 (1999)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-12, 1e-32, 0.0, 1e-10,
  gga_c_op_init,
  NULL, NULL,
  work_gga_c,
};
