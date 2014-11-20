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

#define XC_GGA_X_PBE          101 /* Perdew, Burke & Ernzerhof exchange             */
#define XC_GGA_X_PBE_R        102 /* Perdew, Burke & Ernzerhof exchange (revised)   */
#define XC_GGA_X_PBE_SOL      116 /* Perdew, Burke & Ernzerhof exchange (solids)    */
#define XC_GGA_X_XPBE         123 /* xPBE reparametrization by Xu & Goddard         */
#define XC_GGA_X_PBE_JSJR     126 /* JSJR reparametrization by Pedroza, Silva & Capelle */
#define XC_GGA_X_PBEK1_VDW    140 /* PBE reparametrization for vdW */
#define XC_GGA_X_RGE2         142 /* Regularized PBE */
#define XC_GGA_X_APBE         184 /* mu fixed from the semiclassical neutral atom   */
#define XC_GGA_K_APBE         185 /* mu fixed from the semiclassical neutral atom   */
#define XC_GGA_K_TW1          187 /* Tran and Wesolowski set 1 (Table II)           */
#define XC_GGA_K_TW2          188 /* Tran and Wesolowski set 2 (Table II)           */
#define XC_GGA_K_TW3          189 /* Tran and Wesolowski set 3 (Table II)           */
#define XC_GGA_K_TW4          190 /* Tran and Wesolowski set 4 (Table II)           */


typedef struct{
  FLOAT kappa, mu;
} gga_x_pbe_params;


static void 
gga_x_pbe_init(XC(func_type) *p)
{
  static const FLOAT kappa[13] = {
    0.8040,  /* original PBE */
    1.245,   /* PBE R     */
    0.8040,  /* PBE sol   */
    0.91954, /* xPBE      */
    0.8040,  /* PBE_JSJR  */
    1.0,     /* PBEK1_VDW */
    0.8040,  /* RGE2      */
    0.8040,  /* APBE (X)  */
    0.8040,  /* APBE (K)  */
    0.8209,  /* TW1       */
    0.6774,  /* TW2       */
    0.8438,  /* TW3       */
    0.8589   /* TW4       */
  };

  static const FLOAT mu[13] = {
    0.2195149727645171,     /* PBE: mu = beta*pi^2/3, beta = 0.06672455060314922 */
    0.2195149727645171,     /* PBE rev: as PBE */
    10.0/81.0,              /* PBE sol */
    0.23214,                /* xPBE */
    0.046*M_PI*M_PI/3.0,    /* PBE_JSJR */
    0.2195149727645171,     /* PBEK1_VDW: as PBE */
    10.0/81.0,              /* RGE2      */
    0.260,                  /* APBE (X)  */
    0.23889,                /* APBE (K)  */
    0.2335,                 /* TW1       */
    0.2371,                 /* TW2       */
    0.2319,                 /* TW3       */
    0.2309                  /* TW4       */
  };

  assert(p!=NULL && p->params == NULL);
  p->params = malloc(sizeof(gga_x_pbe_params));
 
  switch(p->info->number){
  case XC_GGA_X_PBE:        p->func = 0;  break;
  case XC_GGA_X_PBE_R:      p->func = 1;  break;
  case XC_GGA_X_PBE_SOL:    p->func = 2;  break;
  case XC_GGA_X_XPBE:       p->func = 3;  break;
  case XC_GGA_X_PBE_JSJR:   p->func = 4;  break;
  case XC_GGA_X_PBEK1_VDW:  p->func = 5;  break;
  case XC_GGA_X_RGE2:       p->func = 6;  break;
  case XC_GGA_X_APBE:       p->func = 7;  break;
  case XC_GGA_K_APBE:       p->func = 8;  break;
  case XC_GGA_K_TW1:        p->func = 9;  break;
  case XC_GGA_K_TW2:        p->func = 10; break;
  case XC_GGA_K_TW3:        p->func = 11; break;
  case XC_GGA_K_TW4:        p->func = 12; break;
  default:
    fprintf(stderr, "Internal error in gga_x_pbe\n");
    exit(1);
  }

  XC(gga_x_pbe_set_params)(p, kappa[p->func], mu[p->func]);
}


void 
XC(gga_x_pbe_set_params)(XC(func_type) *p, FLOAT kappa, FLOAT mu)
{
  gga_x_pbe_params *params;

  assert(p != NULL && p->params != NULL);
  params = (gga_x_pbe_params *) (p->params);

  params->kappa = kappa;
  params->mu    = mu;
}


void XC(gga_x_pbe_enhance) 
  (const XC(func_type) *p, int order, FLOAT x, 
   FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  FLOAT kappa, mu, ss, ss2, f0, df0, d2f0;

  assert(p->params != NULL);
  kappa = ((gga_x_pbe_params *) (p->params))->kappa;
  mu    = ((gga_x_pbe_params *) (p->params))->mu;

  ss  = X2S*x;
  ss2 = ss*ss;
 
  f0 = kappa + mu*ss2;
  if(p->info->number == XC_GGA_X_RGE2)
    f0 += mu*mu*ss2*ss2/kappa;

  *f = 1.0 + kappa*(1.0 - kappa/f0);

  if(order < 1) return;

  df0 = 2.0*mu*ss;
  if(p->info->number == XC_GGA_X_RGE2)
    df0 += 4.0*mu*mu*ss2*ss/kappa;

  *dfdx  = X2S*kappa*kappa*df0/(f0*f0);

  if(order < 2) return;

  d2f0 = 2.0*mu;
  if(p->info->number == XC_GGA_X_RGE2)
    d2f0 += 4.0*3.0*mu*mu*ss2/kappa;

  *d2fdx2 = X2S*X2S*kappa*kappa/(f0*f0)*(d2f0 - 2.0*df0*df0/f0);
}


#define func XC(gga_x_pbe_enhance)
#include "work_gga_x.c"


const XC(func_info_type) XC(func_info_gga_x_pbe) = {
  XC_GGA_X_PBE,
  XC_EXCHANGE,
  "Perdew, Burke & Ernzerhof",
  XC_FAMILY_GGA,
  "JP Perdew, K Burke, and M Ernzerhof, Phys. Rev. Lett. 77, 3865 (1996)\n"
  "JP Perdew, K Burke, and M Ernzerhof, Phys. Rev. Lett. 78, 1396(E) (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init, 
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_pbe_r) = {
  XC_GGA_X_PBE_R,
  XC_EXCHANGE,
  "Revised PBE from Zhang & Yang",
  XC_FAMILY_GGA,
  "Y Zhang and W Yang, Phys. Rev. Lett 80, 890 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init, 
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_pbe_sol) = {
  XC_GGA_X_PBE_SOL,
  XC_EXCHANGE,
  "Perdew, Burke & Ernzerhof SOL",
  XC_FAMILY_GGA,
  "JP Perdew, et al, Phys. Rev. Lett. 100, 136406 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init, 
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_xpbe) = {
  XC_GGA_X_XPBE,
  XC_EXCHANGE,
  "Extended PBE by Xu & Goddard III",
  XC_FAMILY_GGA,
  "X Xu and WA Goddard III, J. Chem. Phys. 121, 4068 (2004)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init, 
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_pbe_jsjr) = {
  XC_GGA_X_PBE_JSJR,
  XC_EXCHANGE,
  "Reparametrized PBE by Pedroza, Silva & Capelle",
  XC_FAMILY_GGA,
  "LS Pedroza, AJR da Silva, and K. Capelle, Phys. Rev. B 79, 201106(R) (2009)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init, 
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_pbek1_vdw) = {
  XC_GGA_X_PBEK1_VDW,
  XC_EXCHANGE,
  "Reparametrized PBE for vdW",
  XC_FAMILY_GGA,
  "J Klimes, DR Bowler, and A Michaelides, J. Phys.: Condens. Matter 22, 022201 (2010)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init, 
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_rge2) = {
  XC_GGA_X_RGE2,
  XC_EXCHANGE,
  "Regularized PBE",
  XC_FAMILY_GGA,
  "A Ruzsinszky, GI Csonka, and G Scuseria, J. Chem. Theory Comput. 5, 763 (2009)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init,
  NULL, NULL,
  work_gga_x
};

const XC(func_info_type) XC(func_info_gga_x_apbe) = {
  XC_GGA_X_APBE,
  XC_EXCHANGE,
  "mu fixed from the semiclassical neutral atom",
  XC_FAMILY_GGA,
  "LA Constantin, E Fabiano, S Laricchia, and F Della Sala, Phys. Rev. Lett. 106, 186406 (2011)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init,
  NULL, NULL,
  work_gga_x
};

#define XC_KINETIC_FUNCTIONAL
#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_k_apbe) = {
  XC_GGA_K_APBE,
  XC_KINETIC,
  "mu fixed from the semiclassical neutral atom",
  XC_FAMILY_GGA,
  "LA Constantin, E Fabiano, S Laricchia, and F Della Sala, Phys. Rev. Lett. 106, 186406 (2011)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_tw1) = {
  XC_GGA_K_TW1,
  XC_KINETIC,
  "Tran and Wesolowski set 1 (Table II)",
  XC_FAMILY_GGA,
  "F Tran and TA Wesolowski, Int. J. Quant. Chem. 89, 441-446 (2002)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_tw2) = {
  XC_GGA_K_TW2,
  XC_KINETIC,
  "Tran and Wesolowski set 1 (Table II)",
  XC_FAMILY_GGA,
  "F Tran and TA Wesolowski, Int. J. Quant. Chem. 89, 441-446 (2002)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_tw3) = {
  XC_GGA_K_TW3,
  XC_KINETIC,
  "Tran and Wesolowski set 1 (Table II)",
  XC_FAMILY_GGA,
  "F Tran and TA Wesolowski, Int. J. Quant. Chem. 89, 441-446 (2002)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_tw4) = {
  XC_GGA_K_TW4,
  XC_KINETIC,
  "Tran and Wesolowski set 1 (Table II)",
  XC_FAMILY_GGA,
  "F Tran and TA Wesolowski, Int. J. Quant. Chem. 89, 441-446 (2002)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  gga_x_pbe_init,
  NULL, NULL,
  work_gga_k
};
