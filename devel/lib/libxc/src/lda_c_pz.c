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
 Correlation energy per particle and potential of a HEG as parametrized 
 by 
   Perdew & Zunger
   Ortiz & Ballone
************************************************************************/

#define XC_LDA_C_PZ       9   /* Perdew & Zunger              */
#define XC_LDA_C_PZ_MOD  10   /* Perdew & Zunger (Modified)   */
#define XC_LDA_C_OB_PZ   11   /* Ortiz & Ballone (PZ)         */

typedef struct {
  FLOAT gamma[2];
  FLOAT beta1[2];
  FLOAT beta2[2];
  FLOAT a[2], b[2], c[2], d[2];
} pz_consts_type;

static pz_consts_type
pz_consts[3] = {
  {    /* PZ Original */
    {-0.1423, -0.0843},  /* gamma */
    { 1.0529,  1.3981},  /* beta1 */
    { 0.3334,  0.2611},  /* beta2 */
    { 0.0311,  0.01555}, /*  a    */
    {-0.048,  -0.0269},  /*  b    */
    { 0.0020,  0.0007},  /*  c    */
    {-0.0116, -0.0048}   /*  d    */
  }, { /* PZ Modified */
    {-0.1423, -0.0843},   
    { 1.0529,  1.3981}, 
    { 0.3334,  0.2611}, 
    { 0.0311,  0.01555},
    {-0.048,  -0.0269},   
    { 0.0020191519406228,  0.00069255121311694},
    {-0.0116320663789130, -0.00480126353790614}
  }, { /* OB */
    {-0.103756, -0.065951},
    { 0.56371,   1.11846},
    { 0.27358,   0.18797},
    { 0.031091,  0.015545},
    {-0.046644, -0.025599},
    { 0.00419,   0.00329},  /* the sign of c[0] and c[1] is different from [2], but is consistent
			       with the continuity requirement. There is nothing in [3] about this. */
    {-0.00983,  -0.00300}
  }
};


/* Auxiliary functions to handle parametrizations */
static void
ec_pot_low(pz_consts_type *X, int order, int i, FLOAT *rs, 
	   FLOAT *zk, FLOAT *dedrs, FLOAT *d2edrs2, FLOAT *d3edrs3)
{
  FLOAT f1, f12, beta12, beta22;

  /* Eq. C3 */
  f1  = 1.0 + X->beta1[i]*rs[0] + X->beta2[i]*rs[1];  
  *zk = X->gamma[i]/f1;

  if(order < 1) return;

  *dedrs  = -X->gamma[i];
  *dedrs *= X->beta1[i]/(2.0*rs[0]) + X->beta2[i];
  *dedrs /= f1*f1;

  if(order < 2) return;

  f12    = f1*f1;
  beta12 = X->beta1[i]*X->beta1[i];
  beta22 = X->beta2[i]*X->beta2[i];

  *d2edrs2  = X->gamma[i];
  *d2edrs2 *= X->beta1[i] + 3.0*beta12*rs[0] +
    9.0*X->beta1[i]*X->beta2[i]*rs[1] + 8.0*beta22*rs[0]*rs[1];
  *d2edrs2 /= 4.0*rs[0]*rs[1]*f12*f1;

  if(order < 3) return;

  *d3edrs3  = -3.0*X->gamma[i];
  *d3edrs3 *= 5.0*beta12*X->beta1[i]*rs[1] + 16.0*beta22*X->beta2[i]*rs[0]*rs[2]
    + 4.0*beta12*rs[0]*(1.0 + 5.0*X->beta2[i]*rs[1])
    + X->beta1[i]*(1.0 + X->beta2[i]*rs[1]*(6.0 + 29.0*X->beta2[i]*rs[1]));
  *d3edrs3 /= 8.0*rs[0]*rs[2]*f12*f12;
}


static void 
ec_pot_high(pz_consts_type *X, int order, int i, FLOAT *rs, 
	    FLOAT *zk, FLOAT *dedrs, FLOAT *d2edrs2, FLOAT *d3edrs3)
{
  FLOAT lrs = log(rs[1]);

  /* Eq. [1].C5 */
  *zk  = X->a[i]*lrs + X->b[i] + X->c[i]*rs[1]*lrs + X->d[i]*rs[1];

  if(order < 1) return;

  *dedrs = X->a[i]/rs[1] + (X->c[i] + X->d[i]) + X->c[i]*lrs;

  if(order < 2) return;

  *d2edrs2 = -X->a[i]/rs[2] + X->c[i]/rs[1];

  if(order < 3) return;

  *d3edrs3 = 2.0*X->a[i]/(rs[1]*rs[2]) - X->c[i]/rs[2];
}


/* the functional */
void 
XC(lda_c_pz_func)(const XC(lda_type) *p, XC(lda_rs_zeta) *r)
{
  int func;
  FLOAT ecp, vcp, fcp, kcp;
  FLOAT ecf, vcf, fcf, kcf;
  FLOAT fz, dfz, d2fz, d3fz;

  func= p->info->number - XC_LDA_C_PZ;
  assert(func==0 || func==1 || func==2);
  
  if(r->rs[1] >= 1.0)
    ec_pot_low (&pz_consts[func], r->order, 0, r->rs, &ecp, &vcp, &fcp, &kcp);
  else
    ec_pot_high(&pz_consts[func], r->order, 0, r->rs, &ecp, &vcp, &fcp, &kcp);

  if(p->nspin == XC_UNPOLARIZED)
    r->zk = ecp;
  else{
    fz  =  FZETA(r->zeta);

    /* get ferromagnetic values */
    if(r->rs[1] >= 1.0)
      ec_pot_low (&pz_consts[func], r->order, 1, r->rs, &ecf, &vcf, &fcf, &kcf);
    else
      ec_pot_high(&pz_consts[func], r->order, 1, r->rs, &ecf, &vcf, &fcf, &kcf);

    r->zk = ecp + (ecf - ecp)*fz;
  }

  if(r->order < 1) return;

  if(p->nspin == XC_UNPOLARIZED)
    r->dedrs = vcp;
  else{
    dfz = DFZETA(r->zeta);

    r->dedrs = vcp + (vcf - vcp)*fz;
    r->dedz  = (ecf - ecp)*dfz;
  }
    
  if(r->order < 2) return;

  if(p->nspin == XC_UNPOLARIZED)
    r->d2edrs2 = fcp;
  else{
    d2fz = D2FZETA(r->zeta);

    r->d2edrs2 = fcp + (fcf - fcp)*fz;
    r->d2edrsz =       (vcf - vcp)*dfz;
    r->d2edz2  =       (ecf - ecp)*d2fz;
  }  

  if(r->order < 3) return;

  if(p->nspin == XC_UNPOLARIZED)
    r->d3edrs3 = kcp;
  else{
    d3fz = D3FZETA(r->zeta);

    r->d3edrs3  = kcp + (kcf - kcp)*fz;
    r->d3edrs2z =       (fcf - fcp)*dfz;
    r->d3edrsz2 =       (vcf - vcp)*d2fz;
    r->d3edz3   =       (ecf - ecp)*d3fz;
  }  
}

#define func XC(lda_c_pz_func)
#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_c_pz) = {
  XC_LDA_C_PZ,
  XC_CORRELATION,
  "Perdew & Zunger",
  XC_FAMILY_LDA,
  "Perdew and Zunger, Phys. Rev. B 23, 5048 (1981)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  NULL,     /* init */
  NULL,     /* end  */
  work_lda, /* lda  */
};

const XC(func_info_type) XC(func_info_lda_c_pz_mod) = {
  XC_LDA_C_PZ_MOD,
  XC_CORRELATION,
  "Perdew & Zunger (Modified)",
  XC_FAMILY_LDA,
  "Perdew and Zunger, Phys. Rev. B 23, 5048 (1981)\n"
  "Modified to improve the matching between the low- and high-rs parts",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  NULL,     /* init */
  NULL,     /* end  */
  work_lda, /* lda  */
};

const XC(func_info_type) XC(func_info_lda_c_ob_pz) = {
  XC_LDA_C_OB_PZ,
  XC_CORRELATION,
  "Ortiz & Ballone (PZ parametrization)",
  XC_FAMILY_LDA,
  "G Ortiz and P Ballone, Phys. Rev. B 50, 1391 (1994)\n"
  "G Ortiz and P Ballone, Phys. Rev. B 56, 9970(E) (1997)\n"
  "Perdew and Zunger, Phys. Rev. B 23, 5048 (1981)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  NULL,     /* init */
  NULL,     /* end  */
  work_lda, /* lda  */
};
