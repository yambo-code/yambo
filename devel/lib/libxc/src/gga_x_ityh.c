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

#define XC_GGA_X_ITYH 529 /* short-range recipe for exchange GGA functionals */

typedef struct{
  int func_id;
  void (*enhancement_factor)
    (const XC(func_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);
} gga_x_ityh_params;

static void
gga_x_ityh_init(XC(func_type) *p)
{
  assert(p->params == NULL);
  p->params = malloc(sizeof(gga_x_ityh_params));

  /* random functional, mainly intended for testing */
  ((gga_x_ityh_params *) (p->params))->func_id = -1;
  XC(gga_x_ityh_set_params)(p, XC_GGA_X_B88, 0.2);
}

void 
XC(gga_x_ityh_set_params)(XC(func_type) *p, int func_id, FLOAT omega)
{
  gga_x_ityh_params *params;

  assert(p != NULL && p->params != NULL);
  params = (gga_x_ityh_params *) (p->params);

  p->cam_omega = omega;

  /* if func_id == -1 do nothing */
  if(func_id != -1 && params->func_id == -1){ /* intialize stuff */
    p->n_func_aux  = 1;
    p->func_aux    = (XC(func_type) **) malloc(sizeof(XC(func_type) *));
    p->func_aux[0] = (XC(func_type)  *) malloc(sizeof(XC(func_type)  ));
  }

  if(func_id != -1 && params->func_id != func_id){
    if(params->func_id != -1)
      XC(func_end) (p->func_aux[0]);

    params->func_id = func_id;
    XC(func_init) (p->func_aux[0], params->func_id, p->nspin);

    switch(params->func_id){
    case XC_GGA_X_WC:
      params->enhancement_factor = XC(gga_x_wc_enhance);
      break;
    case XC_GGA_X_PBE:
    case XC_GGA_X_PBE_R:
    case XC_GGA_X_PBE_SOL:
    case XC_GGA_X_XPBE:
    case XC_GGA_X_PBE_JSJR:
    case XC_GGA_X_PBEK1_VDW:
    case XC_GGA_X_RGE2:
    case XC_GGA_X_APBE:
      params->enhancement_factor = XC(gga_x_pbe_enhance);
      break;
    case XC_GGA_X_PW91:
    case XC_GGA_X_MPW91:
      params->enhancement_factor = XC(gga_x_pw91_enhance);
      break;
    case XC_GGA_X_RPBE:
      params->enhancement_factor = XC(gga_x_rpbe_enhance);
      break;
    case XC_GGA_X_HTBS:
      params->enhancement_factor = XC(gga_x_htbs_enhance);
      break;
    case XC_GGA_X_B88:
    case XC_GGA_X_OPTB88_VDW:
    case XC_GGA_X_MB88:
      params->enhancement_factor = XC(gga_x_b88_enhance);
      break;
    case XC_GGA_X_G96:
      params->enhancement_factor = XC(gga_x_g96_enhance);
      break;
    default:
      fprintf(stderr, "Internal error in gga_x_ityh\n");
      exit(1);
    }
  }
}


#define HEADER 3

static inline void 
func(const XC(func_type) *p, int order, FLOAT x, FLOAT ds,
     FLOAT *f, FLOAT *dfdx, FLOAT *lvrho)
{
  gga_x_ityh_params *params;
  FLOAT e_f, e_dfdx, e_d2fdx2;
  FLOAT k_GGA, K_GGA, aa, f_aa, df_aa, d2f_aa, d3f_aa;
  FLOAT dk_GGAdr, dk_GGAdx, daadr, daadx;

  assert(p != NULL && p->params != NULL);
  params = (gga_x_ityh_params *) (p->params);

  /* call enhancement factor */
  params->enhancement_factor(p->func_aux[0], order, x, &e_f, &e_dfdx, &e_d2fdx2);

  K_GGA = 2.0*X_FACTOR_C*e_f;
  k_GGA = SQRT(9.0*M_PI/K_GGA)*CBRT(ds);

  aa = p->cam_omega/(2.0*k_GGA);

  XC(lda_x_attenuation_function)(0, order, aa, &f_aa, &df_aa, &d2f_aa, &d3f_aa);

  *f = e_f*f_aa;

  if(order < 1) return;

  dk_GGAdr =  k_GGA/(3.0*ds);
  dk_GGAdx = -k_GGA*e_dfdx/(2.0*e_f);

  daadr   = -aa*dk_GGAdr/k_GGA;
  daadx   = -aa*dk_GGAdx/k_GGA;

  *dfdx   = e_dfdx*f_aa + e_f*df_aa*daadx;
  *lvrho  = e_f*df_aa*daadr; 
}

#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_x_ityh) = {
  XC_GGA_X_ITYH,
  XC_EXCHANGE,
  "Short-range recipe for exchange GGA functionals",
  XC_FAMILY_GGA,
  "H Iikura, T Tsuneda, T Yanai, and K Hirao, J. Chem. Phys. 115, 3540 (2001)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_ityh_init,
  NULL, NULL, 
  work_gga_x
};
