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

#define XC_GGA_XC_TH_FL        196 /* Tozer and Handy v. FL  */
#define XC_GGA_XC_TH_FC        197 /* Tozer and Handy v. FC  */
#define XC_GGA_XC_TH_FCFO      198 /* Tozer and Handy v. FCFO */
#define XC_GGA_XC_TH_FCO       199 /* Tozer and Handy v. FCO */
#define XC_GGA_XC_TH1          154 /* Tozer and Handy v. 1 */
#define XC_GGA_XC_TH2          155 /* Tozer and Handy v. 2 */
#define XC_GGA_XC_TH3          156 /* Tozer and Handy v. 3 */
#define XC_GGA_XC_TH4          157 /* Tozer and Handy v. 4 */

typedef struct{
  int n, *b, *c, *d;
  FLOAT *a, *omega;
} gga_xc_th_params;


/* parameters for TH_FL */
static int n_TH_FL = 4;
static FLOAT omega_TH_FL[] = 
  {-0.106141e01, +0.898203e00, -0.134439e01, +0.302369e00};

/* parameters for TH_FC */
static int n_TH_FC = 12;

/* parameters for TH_FCFO */
static int n_TH_FCFO = 20;
static FLOAT omega_TH_FCFO[] = 
  {-0.864448e+00, +0.565130e+00, -0.127306e+01, +0.309681e+00, -0.287658e+00, +0.588767e+00,
   -0.252700e+00, +0.223563e-01, +0.140131e-01, -0.826608e-01, +0.556080e-01, -0.936227e-02,
   -0.677146e-02, +0.515199e-01, -0.874213e-01, +0.423827e-01, +0.431940e+00, -0.691153e+00,
   -0.637866e+00, +0.107565e+01};

/* parameters for TH_FCO */
static int n_TH_FCO = 20;
static FLOAT omega_TH_FCO[] = 
  {-0.962998e+00, +0.860233e+00, -0.154092e+01, +0.381602e+00, -0.210208e+00, +0.391496e+00,
   -0.107660e+00, -0.105324e-01, +0.837384e-02, -0.617859e-01, +0.383072e-01, -0.526905e-02,
   -0.381514e-02, +0.321541e-01, -0.568280e-01, +0.288585e-01, +0.368326e+00, -0.328799e+00,
   -0.122595e+01, +0.136412e+01};

/* parameters for TH1 */
static int n_TH1 = 21;
static FLOAT a_TH1[] = 
  {7.0/6.0, 8.0/6.0, 9.0/6.0, 10.0/6.0, 8.0/6.0, 9.0/6.0, 10.0/6.0, 
   11.0/6.0, 9.0/6.0, 10.0/6.0, 11.0/6.0, 12.0/6.0, 9.0/6.0, 10.0/6.0, 
   11.0/6.0, 12.0/6.0, 7.0/6.0, 8.0/6.0, 9.0/6.0, 10.0/6.0, 1.0};
static int   b_TH1[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0};
static int   c_TH1[] = {0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0};
static int   d_TH1[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0};
static FLOAT omega_TH1[] = 
  {-0.728255e+00, +0.331699e+00, -0.102946e+01, +0.235703e+00, -0.876221e-01, +0.140854e+00,
   +0.336982e-01, -0.353615e-01, +0.497930e-02, -0.645900e-01, +0.461795e-01, -0.757191e-02,
   -0.242717e-02, +0.428140e-01, -0.744891e-01, +0.386577e-01, -0.352519e+00, +0.219805e+01
   -0.372927e+01, +0.194441e+01, +0.128877e+00};

/* parameters for TH2 */
static int n_TH2 = 19;
static FLOAT a_TH2[] =
  {13.0/12.0, 7.0/6.0, 8.0/6.0, 9.0/6.0, 10.0/6.0, 17.0/12.0, 9.0/6.0, 10.0/6.0, 
   11.0/6.0, 10.0/6.0, 11.0/6.0, 12.0/6.0, 10.0/6.0, 11.0/6.0, 12.0/6.0, 7.0/6.0,
   8.0/6.0, 9.0/6.0, 10.0/6.0};
static int   b_TH2[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1};
static int   c_TH2[] = {0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0};
static int   d_TH2[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0};
static FLOAT omega_TH2[] = 
  {+0.678831e+00, -0.175821e+01, +0.127676e+01, -0.160789e+01, +0.365610e+00, -0.181327e+00,
   +0.146973e+00, +0.147141e+00, -0.716917e-01, -0.407167e-01, +0.214625e-01, -0.768156e-03,
   +0.310377e-01, -0.720326e-01, +0.446562e-01, -0.266802e+00, +0.150822e+01, -0.194515e+01,
   +0.679078e+00};

/* parameters for TH3 */
static int n_TH3 = 19;
static FLOAT a_TH3[] =
  {7.0/6.0, 8.0/6.0, 9.0/6.0, 10.0/6.0, 17.0/12.0, 9.0/6.0, 10.0/6.0, 11.0/6.0,
   10.0/6.0, 11.0/6.0, 12.0/6.0, 10.0/6.0, 11.0/6.0, 12.0/6.0, 7.0/6.0, 8.0/6.0, 
   9.0/6.0, 10.0/6.0, 13.0/12.0};
static int   b_TH3[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0};
static int   c_TH3[] = {0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0};
static int   d_TH3[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0};
static FLOAT omega_TH3[] = 
  {-0.142542e+00, -0.783603e+00, -0.188875e+00, +0.426830e-01, -0.304953e+00, +0.430407e+00, 
   -0.997699e-01, +0.355789e-02, -0.344374e-01, +0.192108e-01, -0.230906e-02, +0.235189e-01, 
   -0.331157e-01, +0.121316e-01, +0.441190e+00, -0.227167e+01, +0.403051e+01, -0.228074e+01,
   +0.360204e-01};

/* parameters for TH4 */
static int n_TH4 = 19;
static FLOAT omega_TH4[] = 
  {+0.677353e-01, -0.106763e+01, -0.419018e-01, +0.226313e-01, -0.222478e+00, +0.283432e+00,
   -0.165089e-01, -0.167204e-01, -0.332362e-01, +0.162254e-01, -0.984119e-03, +0.376713e-01,
   -0.653419e-01, +0.222835e-01, +0.375782e+00, -0.190675e+01, +0.322494e+01, -0.168698e+01,
   -0.235810e-01};


static void 
gga_xc_th_init(void *p_)
{
  XC(gga_type) *p = (XC(gga_type) *)p_;
  gga_xc_th_params *params;

  assert(p->params == NULL);
  p->params = (gga_xc_th_params *)malloc(sizeof(gga_xc_th_params));
  params = (gga_xc_th_params *)p->params;

  /* most functionals share the same a, b, c, d */
  params->a = a_TH1;
  params->b = b_TH1;
  params->c = c_TH1;
  params->d = d_TH1;

  switch(p->info->number){
  case XC_GGA_XC_TH_FL:
    p->func = 0;
    params->n = n_TH_FL;
    params->omega = omega_TH_FL;
    break;

  case XC_GGA_XC_TH_FC:
    p->func = 1;
    params->n = n_TH_FC;
    params->omega = omega_TH_FCFO;
    break;

  case XC_GGA_XC_TH_FCFO:
    p->func = 2;
    params->n = n_TH_FCFO;
    params->omega = omega_TH_FCFO;
    break;

  case XC_GGA_XC_TH_FCO:
    p->func = 3;
    params->n = n_TH_FCO;
    params->omega = omega_TH_FCO; 
    break;

  case XC_GGA_XC_TH1:
    p->func = 4;
    params->n = n_TH1;
    params->omega = omega_TH1;
    break;

  case XC_GGA_XC_TH2:
    p->func = 5;
    params->n = n_TH2;
    params->a = a_TH2;
    params->b = b_TH2;
    params->c = c_TH2;
    params->d = d_TH2;
    params->omega = omega_TH2;
    break;

  case XC_GGA_XC_TH3:
    p->func = 6;
    params->n = n_TH3;
    params->a = a_TH3;
    params->b = b_TH3;
    params->c = c_TH3;
    params->d = d_TH3;
    params->omega = omega_TH3;
    break;

  case XC_GGA_XC_TH4:
    p->func = 7;
    params->n = n_TH3;
    params->a = a_TH3;
    params->b = b_TH3;
    params->c = c_TH3;
    params->d = d_TH3;
    params->omega = omega_TH4;
    break;

  default:
    fprintf(stderr, "Internal error in gga_xc_th\n");
    exit(1);
  }
}

static inline void 
func(const XC(gga_type) *p, int order, FLOAT rs, FLOAT zeta, FLOAT xt, FLOAT *xs,
     FLOAT *f, FLOAT *dfdrs, FLOAT *dfdz, FLOAT *dfdxt, FLOAT *dfdxs,
     FLOAT *d2fdrs2, FLOAT *d2fdrsz, FLOAT *d2fdrsxt, FLOAT *d2fdrsxs, FLOAT *d2fdz2, 
     FLOAT *d2fdzxt, FLOAT *d2fdzxs, FLOAT *d2fdxt2, FLOAT *d2fdxtxs, FLOAT *d2fdxs2)
{
  gga_xc_th_params *params;
  int ii;
  FLOAT dens, opz, omz, XX[2], YY;
  FLOAT ddens, dXXdxs[2], dXXdz[2], dYYdxt;
  FLOAT d2dens, d2XXdxs2[2], d2XXdzxs[2], d2YYdxt2;

  assert(p->params != NULL);
  params = (gga_xc_th_params *) p->params;

  dens  = 3.0/(4.0*M_PI*rs*rs*rs);
  opz   = 1.0 + zeta;
  omz   = 1.0 - zeta;
  XX[0] = 0.5*xs[0]*opz;
  XX[1] = 0.5*xs[1]*omz;
  YY    = 2.0*(XX[0] + XX[1]) - xt*xt;

  *f = 0.0;

  if(order >= 1){
    ddens     = -3.0*dens/rs;
    dXXdxs[0] =  0.5*opz;
    dXXdxs[1] =  0.5*omz;
    dXXdz[0]  =  0.5*xs[0];
    dXXdz[1]  = -0.5*xs[1];
    dYYdxt    = -2.0*xt;

    *dfdrs = *dfdz = *dfdxt = dfdxs[0] = dfdxs[1] = 0.0;
  }

  if(order >= 2){
    d2dens      = -4.0*ddens/rs;
    d2XXdzxs[0] =  0.5;
    d2XXdzxs[1] = -0.5;
    d2YYdxt2    = -2.0;

    *d2fdrs2 = *d2fdrsz = *d2fdrsxt = d2fdrsxs[0] = d2fdrsxs[1] = 0.0;
    *d2fdz2 = *d2fdzxt = d2fdzxs[0] = d2fdzxs[1] = 0.0;
    *d2fdxt2 = d2fdxtxs[0] = d2fdxtxs[1] = 0.0;
    d2fdxs2[0] = d2fdxs2[1] = d2fdxs2[2] = 0.0;
  }

  for(ii=0; ii<params->n; ii++){
    FLOAT fz[2], Rid, Ri, Si, Xi, Yi, XXC[2];
    FLOAT dfz[2], dRidrs, dRidz, dSidz, dXidz, dXidxs[2], dYidz, dYidxs[2], dYidxt;
    FLOAT d2fz[2], d2Ridrs2, d2Ridrsz, d2Ridz2, d2Sidz2, d2Xidz2, d2Xidxs2[2], d2Xidzxs[2], d2Yidxt2, d2Yidxs2[2], d2Yidzxs[2];

    fz[0] = POW(opz, params->a[ii]);
    fz[1] = POW(omz, params->a[ii]);
    Rid   = POW(dens/2.0, params->a[ii]);
    Ri    = Rid*(fz[0] + fz[1]);

    /* b = 0 || 1 */
    Si = (params->b[ii] == 0) ? 1.0 : zeta*zeta;

    /* c = 0 || 1 || 2 */
    switch(params->c[ii]){
    case 0: 
      Xi = 1.0;
      break;
    case 1: 
      Xi = 0.5*(XX[0] + XX[1]);
      break;
    case 2:
      Xi = 0.5*(XX[0]*XX[0] + XX[1]*XX[1]);
      break;
    }

    /* d = 0 || 1 */
    Yi = (params->d[ii] == 0) ? 1.0 : YY;

    /* the parametrization in the paper is for the energy per volume */
    *f += params->omega[ii]*Ri*Si*Xi*Yi/dens;

    if(order < 1) continue;

    if(params->a[ii] == 1.0){
      dfz[0] =  1.0;
      dfz[1] = -1.0;
    }else{
      dfz[0] = (ABS(opz) < p->info->min_zeta) ? 0.0 :  params->a[ii]*fz[0]/opz;
      dfz[1] = (ABS(omz) < p->info->min_zeta) ? 0.0 : -params->a[ii]*fz[1]/omz;
    }
    dRidrs   = params->a[ii]*Ri*ddens/dens;
    dRidz    = Rid*(dfz[0] + dfz[1]);
    
    /* b = 0 || 1 */
    dSidz    = (params->b[ii] == 0) ? 0.0 : 2.0*zeta;

    /* c = 0 || 1 || 2 */
    switch(params->c[ii]){
    case 0:
      dXidz = dXidxs[0] = dXidxs[1] = 0.0;
      break;
    case 1:
      dXidz     = 0.5*(dXXdz[0] + dXXdz[1]);
      dXidxs[0] = 0.5*dXXdxs[0];
      dXidxs[1] = 0.5*dXXdxs[1];
      break;
    case 2:
      dXidz     = (XX[0]*dXXdz[0] + XX[1]*dXXdz[1]);
      dXidxs[0] = XX[0]*dXXdxs[0];
      dXidxs[1] = XX[1]*dXXdxs[1];
      break;
    }

    /* d = 0 || 1 */
    if(params->d[ii] == 0){
      dYidz = dYidxt = dYidxs[0] = dYidxs[1] = 0.0;
    }else{
      dYidz     = 2.0*(dXXdz[0] + dXXdz[1]);
      dYidxt    = dYYdxt;
      dYidxs[0] = 2.0*dXXdxs[0];
      dYidxs[1] = 2.0*dXXdxs[1];
    }

    *dfdrs   += params->omega[ii]*(dRidrs - Ri*ddens/dens)*Si*Xi*Yi/dens;
    *dfdz    += params->omega[ii]*(dRidz*Si*Xi*Yi + Ri*dSidz*Xi*Yi + Ri*Si*dXidz*Yi + Ri*Si*Xi*dYidz)/dens;
    *dfdxt   += params->omega[ii]*Ri*Si*Xi*dYidxt/dens;
    dfdxs[0] += params->omega[ii]*Ri*Si*(dXidxs[0]*Yi + Xi*dYidxs[0])/dens;
    dfdxs[1] += params->omega[ii]*Ri*Si*(dXidxs[1]*Yi + Xi*dYidxs[1])/dens;

    if(order < 2) continue;

    if(params->a[ii] == 1.0){
      d2fz[0] = d2fz[1] = 0.0;
    }else{
      d2fz[0] = (ABS(opz) < p->info->min_zeta) ? 0.0 :  (params->a[ii] - 1.0)*dfz[0]/opz;
      d2fz[1] = (ABS(omz) < p->info->min_zeta) ? 0.0 : -(params->a[ii] - 1.0)*dfz[1]/omz;
    }
    d2Ridrs2  = params->a[ii]/dens*(dRidrs*ddens + Ri*(d2dens - ddens*ddens/dens));
    d2Ridrsz  = params->a[ii]*Rid*(ddens/dens)*(dfz[0] + dfz[1]);
    d2Ridz2   = Rid*(d2fz[0] + d2fz[1]);
    
    /* b = 0 || 1 */
    d2Sidz2   = (params->b[ii] == 0) ? 0.0 : 2.0;

    /* c = 0 || 1 || 2 */
    switch(params->c[ii]){
    case 0:
      d2Xidz2 = d2Xidxs2[0] = d2Xidxs2[1] = d2Xidzxs[0] = d2Xidzxs[1] = 0.0;
      break;
    case 1:
      d2Xidz2 = d2Xidxs2[0] = d2Xidxs2[1] = 0.0;
      d2Xidzxs[0] = 0.5*d2XXdzxs[0];
      d2Xidzxs[1] = 0.5*d2XXdzxs[1];
      d2Xidxs2[0] = 0.5*d2XXdxs2[0];
      d2Xidxs2[1] = 0.5*d2XXdxs2[1];
      break;
    case 2:
      d2Xidz2     = (dXXdz[0]*dXXdz[0] + dXXdz[1]*dXXdz[1]);
      d2Xidxs2[0] = dXXdxs[0]*dXXdxs[0];
      d2Xidxs2[1] = dXXdxs[1]*dXXdxs[1];
      d2Xidzxs[0] = dXXdz[0]*dXXdxs[0] + XX[0]*d2XXdzxs[0];
      d2Xidzxs[1] = dXXdz[1]*dXXdxs[1] + XX[1]*d2XXdzxs[1];
      d2Xidxs2[0] = dXXdxs[0]*dXXdxs[0];
      d2Xidxs2[1] = dXXdxs[1]*dXXdxs[1];
      break;
    }    

    /* d = 0 || 1 */
    if(params->d[ii] == 0){
      d2Yidxt2 = d2Yidxs2[0] = d2Yidxs2[1] = d2Yidzxs[0] = d2Yidzxs[1] = 0.0;
    }else{
      d2Yidxt2    = d2YYdxt2;
      d2Yidxs2[0] = 2.0*d2XXdxs2[0];
      d2Yidxs2[1] = 2.0*d2XXdxs2[1];
      d2Yidzxs[0] = 2.0*d2XXdzxs[0];
      d2Yidzxs[1] = 2.0*d2XXdzxs[1];
    }

    *d2fdrs2    +=  params->omega[ii]*(d2Ridrs2 - 2.0*dRidrs*ddens/dens - Ri*(d2dens - 2.0*ddens*ddens/dens)/dens)*Si*Xi*Yi/dens;
    *d2fdrsz    +=  params->omega[ii]*
      (- (dRidz*Si*Xi*Yi + Ri*dSidz*Xi*Yi + Ri*Si*dXidz*Yi + Ri*Si*Xi*dYidz)*ddens/dens
       + (d2Ridrsz*Si*Xi*Yi + dRidrs*dSidz*Xi*Yi + dRidrs*Si*dXidz*Yi + dRidrs*Si*Xi*dYidz))/dens;
    *d2fdrsxt   +=  params->omega[ii]*(dRidrs - Ri*ddens/dens)*Si*Xi*dYidxt/dens;;
    d2fdrsxs[0] +=  params->omega[ii]*(dRidrs - Ri*ddens/dens)*Si*(dXidxs[0]*Yi + Xi*dYidxs[0])/dens;
    d2fdrsxs[1] +=  params->omega[ii]*(dRidrs - Ri*ddens/dens)*Si*(dXidxs[1]*Yi + Xi*dYidxs[1])/dens;;
    *d2fdz2     +=  params->omega[ii]*
      (2.0*(dRidz*dSidz*Xi*Yi + dRidz*Si*dXidz*Yi + dRidz*Si*Xi*dYidz + Ri*dSidz*dXidz*Yi + Ri*dSidz*Xi*dYidz + Ri*Si*dXidz*dYidz) +
       d2Ridz2*Si*Xi*Yi + Ri*d2Sidz2*Xi*Yi + Ri*Si*d2Xidz2*Yi)/dens;
    *d2fdzxt    +=  params->omega[ii]*(dRidz*Si*Xi*dYidxt + Ri*dSidz*Xi*dYidxt + Ri*Si*dXidz*dYidxt)/dens;
    d2fdzxs[0]  +=  params->omega[ii]*
      ((dRidz*Si + Ri*dSidz)*(dXidxs[0]*Yi + Xi*dYidxs[0]) + 
       Ri*Si*(d2Xidzxs[0]*Yi + dXidz*dYidxs[0] + dXidxs[0]*dYidz + Xi*d2Yidzxs[0]))/dens;
    d2fdzxs[1]  +=  params->omega[ii]*
      ((dRidz*Si + Ri*dSidz)*(dXidxs[1]*Yi + Xi*dYidxs[1]) + 
       Ri*Si*(d2Xidzxs[1]*Yi + dXidz*dYidxs[1] + dXidxs[1]*dYidz + Xi*d2Yidzxs[1]))/dens;
    *d2fdxt2    +=  params->omega[ii]*Ri*Si*Xi*d2Yidxt2/dens;
    d2fdxtxs[0] +=  params->omega[ii]*Ri*Si*dXidxs[0]*dYidxt/dens;
    d2fdxtxs[1] +=  params->omega[ii]*Ri*Si*dXidxs[1]*dYidxt/dens;
    d2fdxs2[0]  +=  params->omega[ii]*Ri*Si*(d2Xidxs2[0]*Yi + 2.0*dXidxs[0]*dYidxs[0])/dens;
    d2fdxs2[1]  +=  0.0;
    d2fdxs2[2]  +=  params->omega[ii]*Ri*Si*(d2Xidxs2[1]*Yi + 2.0*dXidxs[1]*dYidxs[1])/dens;;

  }
}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_xc_th_fl) = {
  XC_GGA_XC_TH_FL,
  XC_EXCHANGE_CORRELATION,
  "Tozer and Handy v. FL",
  XC_FAMILY_GGA,
  "DJ Tozer, NC Handy, amd WH Green, Chem. Phys. Lett. 273, 183-194 (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_xc_th_init, 
  NULL, NULL,
  work_gga_c
};

const XC(func_info_type) XC(func_info_gga_xc_th_fc) = {
  XC_GGA_XC_TH_FC,
  XC_EXCHANGE_CORRELATION,
  "Tozer and Handy v. FC",
  XC_FAMILY_GGA,
  "DJ Tozer, NC Handy, amd WH Green, Chem. Phys. Lett. 273, 183-194 (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_xc_th_init, 
  NULL, NULL,
  work_gga_c
};

const XC(func_info_type) XC(func_info_gga_xc_th_fcfo) = {
  XC_GGA_XC_TH_FCFO,
  XC_EXCHANGE_CORRELATION,
  "Tozer and Handy v. FCFO",
  XC_FAMILY_GGA,
  "DJ Tozer, NC Handy, amd WH Green, Chem. Phys. Lett. 273, 183-194 (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_xc_th_init, 
  NULL, NULL,
  work_gga_c
};

const XC(func_info_type) XC(func_info_gga_xc_th_fco) = {
  XC_GGA_XC_TH_FCO,
  XC_EXCHANGE_CORRELATION,
  "Tozer and Handy v. FCO",
  XC_FAMILY_GGA,
  "DJ Tozer, NC Handy, amd WH Green, Chem. Phys. Lett. 273, 183-194 (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_xc_th_init, 
  NULL, NULL,
  work_gga_c
};

const XC(func_info_type) XC(func_info_gga_xc_th1) = {
  XC_GGA_XC_TH1,
  XC_EXCHANGE_CORRELATION,
  "Tozer and Handy v. 1",
  XC_FAMILY_GGA,
  "DJ Tozer and NC Handy, J. Chem. Phys. 108, 2545 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_xc_th_init, 
  NULL, NULL,
  work_gga_c
};

const XC(func_info_type) XC(func_info_gga_xc_th2) = {
  XC_GGA_XC_TH2,
  XC_EXCHANGE_CORRELATION,
  "Tozer and Handy v. 2",
  XC_FAMILY_GGA,
  "DJ Tozer and NC Handy, J. Phys. Chem. A 102, 3162 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_xc_th_init, 
  NULL, NULL,
  work_gga_c
};

const XC(func_info_type) XC(func_info_gga_xc_th3) = {
  XC_GGA_XC_TH3,
  XC_EXCHANGE_CORRELATION,
  "Tozer and Handy v. 3",
  XC_FAMILY_GGA,
  "DJ Tozer and NC Handy, Mol. Phys. 94, 707 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_xc_th_init, 
  NULL, NULL,
  work_gga_c
};

const XC(func_info_type) XC(func_info_gga_xc_th4) = {
  XC_GGA_XC_TH4,
  XC_EXCHANGE_CORRELATION,
  "Tozer and Handy v. 4",
  XC_FAMILY_GGA,
  "DJ Tozer and NC Handy, Mol. Phys. 94, 707 (1998)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_xc_th_init, 
  NULL, NULL,
  work_gga_c
};
