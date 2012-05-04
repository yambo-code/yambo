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

#include <stdlib.h>
#include <assert.h>

#include "util.h"

/************************************************************************
 Correlation energy per particle and potentials for a homogeneous electron
 gas in 2D, as parametrized by Attacalite et al.
************************************************************************/

#define XC_LDA_C_2D_AMGB  15   /* Attacalite et al             */


/* parameters necessary to the calculation */
static FLOAT a[3] = { -0.1925,     0.117331,    0.0234188 };
static FLOAT b[3] = {  0.0863136, -3.394e-2,   -0.037093  };
static FLOAT c[3] = {  0.0572384, -7.66765e-3,  0.0163618 };
static FLOAT d[3] = {  0.0,        0.0,         0.0       };
static FLOAT e[3] = {  1.0022,     0.4133,      1.424301  };
static FLOAT f[3] = { -0.02069,    0.0,         0.0       };
static FLOAT g[3] = {  0.33997,    6.68467e-2,  0.0       };
static FLOAT h[3] = {  1.747e-2,   7.799e-4,    1.163099  };
static FLOAT beta = 1.3386, ax = 0.0;


/* Initialization */
static void
lda_c_2d_amgb_init(void *p)
{
  int i;
  
  /* initialize a couple of constants */
  for(i=0; i<3; i++) d[i] = -a[i]*h[i];
  ax = -4.0/(3.0*M_PI*M_SQRT2);
}

static void
malpha(int order, int i, FLOAT *rs,
       FLOAT *alpha, FLOAT *dalpha, FLOAT *d2alpha, FLOAT *d3alpha)
{
  FLOAT p1, dp1, d2p1, d3p1, p2, dp2, d2p2, d3p2;
  FLOAT rs3, aux2, logp2;

  rs3 = rs[2]*rs[1];

  p1  = b[i]*rs[1] + c[i]*rs[2] + d[i]*rs3;
  p2  = e[i]*rs[1] + f[i]*rs[0]*rs[1] + g[i]*rs[2] + h[i]*rs3;

  aux2  = 1.0 + p2;
  logp2 = LOG(1.0 + 1.0/p2);

  *alpha = a[i] + p1*logp2;

  if(order < 1) return;

  dp1 = b[i] + 2.0*c[i]*rs[1] + 3.0*d[i]*rs[2];
  dp2 = e[i] + 1.5*f[i]*rs[0] + 2.0*g[i]*rs[1] + 3.0*h[i]*rs[2];
  
  *dalpha = dp1*logp2 - p1*dp2/(p2*aux2);

  if(order < 2) return;

  d2p1 = 2.0*c[i] + 6.0*d[i]*rs[1];
  d2p2 = 1.5*0.5*f[i]/rs[0] + 2.0*g[i] + 6.0*h[i]*rs[1];

  *d2alpha = d2p1*logp2 + 
    (-2.0*p2*aux2*dp1*dp2 + p1*(1.0 + 2.0*p2)*dp2*dp2 - p1*p2*aux2*d2p2)/(p2*p2*aux2*aux2);

  if(order < 3) return;

  d3p1 = 6.0*d[i];
  d3p2 = -1.5*0.5*0.5*f[i]/(rs[0]*rs[1]) + 6.0*h[i];

  *d3alpha = d3p1*logp2 + 
    (2*p1*(2.0 + 3.0*p2)*dp2*dp2*dp2 - 3.0*p2*aux2*dp2*(dp1*dp2 + p1*d2p2) +
     aux2*aux2*(-6.0*p1*dp2*dp2*dp2 + 6.0*p2*dp2*(dp1*dp2 + p1*d2p2)
		-p2*p2*(3.0*dp2*d2p1 + 3.0*dp1*d2p2 + p1*d3p2)))/
    (p2*p2*p2*aux2*aux2*aux2);
}


static void
func(const XC(lda_type) *p, XC(lda_rs_zeta) *r)
{
  FLOAT ecp, vcp, fcp, kcp;
  FLOAT ecf, vcf, fcf, kcf;
  FLOAT alpha, dalpha, d2alpha, d3alpha;
  FLOAT z2, z3, z4, fz, dfz, d2fz, d3fz;
  FLOAT ex, dex, d2ex, d3ex;
  FLOAT ex6, dex6drs, dex6dz, d2ex6drs2, d2ex6drsz, d2ex6dz2, d3ex6drs3, d3ex6drs2z, d3ex6drsz2, d3ex6dz3;

  malpha(r->order, 0, r->rs, &ecp, &vcp, &fcp, &kcp);
  
  if(p->nspin == XC_UNPOLARIZED)
    r->zk = ecp;
  else{
    /* get ferromagnetic values */
    malpha(r->order, 1, r->rs, &ecf, &vcf, &fcf, &kcf);

    /* get alpha_c */
    malpha(r->order, 2, r->rs, &alpha, &dalpha, &d2alpha, &d3alpha);

    z2  = r->zeta*r->zeta;
    z3  = r->zeta*z2;
    z4  = r->zeta*z3;

    ex  = -4.0*M_SQRT2/(3.0*M_PI*r->rs[1]) ;
    fz  = 0.5*(POW(1.0 + r->zeta, 3.0/2.0) + POW(1.0 - r->zeta, 3.0/2.0));
    ex6 = ex*(fz - 1.0 - 3.0/8.0*z2 - 3.0/128.0*z4);

    r->zk = ecp + ecf*z2 + alpha*z4 + (exp(-beta*r->rs[1]) - 1.0)*ex6;
  }

  if(r->order < 1) return;

  if(p->nspin == XC_UNPOLARIZED)
    r->dedrs = vcp;
  else{
    dex = -ex/r->rs[1];

    dfz = 3.0/4.0*(SQRT(1.0 + r->zeta) - SQRT(1.0 - r->zeta));

    dex6drs = dex*(fz - 1.0 - (3.0/8.0)*z2 - (3.0/128.0)*z4);
    dex6dz  =  ex*(dfz - 2.0*(3.0/8.0)*r->zeta - 4.0*(3.0/128.0)*z3);

    r->dedrs = vcp + vcf*z2 + dalpha*z4 + exp(-beta*r->rs[1])*(dex6drs - beta*ex6) - dex6drs;
    r->dedz  = 2.0*ecf*r->zeta + 4.0*alpha*z3 + (exp(-beta*r->rs[1]) - 1.0)*dex6dz;
  }

  if(r->order < 2) return;

  if(p->nspin == XC_UNPOLARIZED)
    r->d2edrs2 = fcp;
  else{
    d2ex = -2.0*dex/r->rs[1];

    d2fz = 3.0/8.0*(1.0 / SQRT(1.0 + r->zeta) + 1.0 / SQRT(1.0 - r->zeta));
    
    d2ex6drs2 = d2ex*(  fz - 1.0 - (3.0/8.0)*z2          - (3.0/128.0)*z4);
    d2ex6drsz =  dex*( dfz   - 2.0*(3.0/8.0)*r->zeta - 4.0*(3.0/128.0)*z3);
    d2ex6dz2  =   ex*(d2fz   - 2.0*(3.0/8.0)        - 12.0*(3.0/128.0)*z2);

    r->d2edrs2 = fcp + fcf*z2 + d2alpha*z4 + 
      exp(-beta*r->rs[1])*(d2ex6drs2 - 2.0*beta*dex6drs + beta*beta*ex6) - d2ex6drs2;
    r->d2edrsz = 2.0*vcf*r->zeta + 4.0*dalpha*z3 + exp(-beta*r->rs[1])*(d2ex6drsz - beta*dex6dz) - d2ex6drsz;
    r->d2edz2  = 2.0*ecf + 12.0*alpha*z2 + (exp(-beta*r->rs[1]) - 1.0)*d2ex6dz2;
  }

  if(r->order < 3) return;

  if(p->nspin == XC_UNPOLARIZED)
    r->d3edrs3 = kcp;
  else{
    d3ex = -3.0*d2ex/r->rs[1];

    d3fz = -3.0/16.0*(POW(1.0 + r->zeta, -3.0/2.0) - POW(1.0 - r->zeta, -3.0/2.0));

    d3ex6drs3  = d3ex*(  fz - 1.0 - (3.0/8.0)*z2          - (3.0/128.0)*z4);
    d3ex6drs2z = d2ex*( dfz   - 2.0*(3.0/8.0)*r->zeta - 4.0*(3.0/128.0)*z3);
    d3ex6drsz2 =  dex*(d2fz   - 2.0*(3.0/8.0)        - 12.0*(3.0/128.0)*z2);
    d3ex6dz3   =   ex*(d3fz                          - 24.0*(3.0/128.0)*r->zeta);

    r->d3edrs3  = kcp + kcf*z2 + d3alpha*z4 + 
      exp(-beta*r->rs[1])*(d3ex6drs3 - 3.0*beta*d2ex6drs2 + 3.0*beta*beta*dex6drs - beta*beta*beta*ex6) - d3ex6drs3;
    r->d3edrs2z = 2.0*fcf*r->zeta + 4.0*d2alpha*z3 + 
      exp(-beta*r->rs[1])*(d3ex6drs2z - 2.0*beta*d2ex6drsz + beta*beta*dex6dz) - d3ex6drs2z;
    r->d3edrsz2 = 2.0*vcf + 12.0*dalpha*z2 + exp(-beta*r->rs[1])*(d3ex6drsz2 - beta*d2ex6dz2) - d3ex6drsz2;
    r->d3edz3   = 24.0*alpha*r->zeta + (exp(-beta*r->rs[1]) - 1.0)*d3ex6dz3;
  }
}

#define XC_DIMENSIONS 2
#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_c_2d_amgb) = {
  XC_LDA_C_2D_AMGB,
  XC_CORRELATION,
  "AMGB (for 2D systems)",
  XC_FAMILY_LDA,
  "C Attacalite et al, Phys. Rev. Lett. 88, 256601 (2002)\n"
  "C Attacalite, PhD thesis",
  XC_FLAGS_2D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  lda_c_2d_amgb_init,
  NULL,
  work_lda
};
