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
#include <assert.h>
#include "util.h"


/************************************************************************
   L. Hedin and  B.I. Lundqvist
   O. Gunnarsson and B. I. Lundqvist
************************************************************************/

#define XC_LDA_C_HL   4   /* Hedin & Lundqvist            */
#define XC_LDA_C_GL   5   /* Gunnarson & Lundqvist        */
#define XC_LDA_C_vBH 17   /* von Barth & Hedin            */

static void 
hl_f(int func, int order, int i, FLOAT rs, FLOAT *zk, FLOAT *drs, FLOAT *d2rs, FLOAT *d3rs)
{
  static const 
    FLOAT r[3][2] = {{21.0,   21.0},     /* HL unpolarized only*/
		     {11.4,   15.9},     /* GL */
                     {30,     75 }};     /* vBH */
  static const 
    FLOAT c[3][2] = {{0.0225, 0.0225},  /* HL unpolarized only */
		     {0.0333, 0.0203},  /* GL */
		     {0.0252, 0.0127}}; /* vBH */
  
  FLOAT a, x, x2, x3;
  
  x   = rs/r[func][i];
  x2  = x*x;
  x3  = x2*x;
  
  a   = log(1.0 + 1.0/x);
  *zk = -c[func][i]*((1.0 + x3)*a - x2 + 0.5*x - 1.0/3.0);
  
  if(order < 1) return;

  *drs  = 3.0*x*(x*a - 1) - 1/x + 3.0/2.0;
  *drs *= -c[func][i]/r[func][i];

  if(order < 2) return;

  *d2rs  = -3.0 + 1.0/x2 - 3.0*x/(1.0 + x) + 6.0*x*a;
  *d2rs *= -c[func][i]/(r[func][i]*r[func][i]);

  if(order < 3) return;

  *d3rs = -2.0/x3 + 3.0*x/((1.0 + x)*(1.0 + x)) - 9.0/(1.0 + x) + 6.0*a;
  *d3rs *= -c[func][i]/(r[func][i]*r[func][i]*r[func][i]);
}


void 
XC(lda_c_hl_func)(const XC(lda_type) *p, XC(lda_rs_zeta) *r)
{
  int func;
  FLOAT ecp, vcp, fcp, kcp;
  FLOAT ecf, vcf, fcf, kcf;
  FLOAT fz, dfz, d2fz, d3fz;

  switch(p->info->number){
  case XC_LDA_C_GL:  func = 1; break;
  case XC_LDA_C_vBH: func = 2; break;
  default:           func = 0; /* original HL */
  }

  hl_f(func, r->order, 0, r->rs[1], &ecp, &vcp, &fcp, &kcp);

  if(p->nspin == XC_UNPOLARIZED)
    r->zk = ecp;
  else{
    /* get ferromagnetic values */
    hl_f(func, r->order, 1, r->rs[1], &ecf, &vcf, &fcf, &kcf);

    fz = FZETA(r->zeta);
    r->zk = ecp + (ecf - ecp)*fz;
  }

  if(r->order < 1) return;

  if(p->nspin == XC_UNPOLARIZED)
    r->dedrs = vcp;
  else{
    dfz = DFZETA(r->zeta);
    r->dedrs = vcp + (vcf - vcp)*fz;
    r->dedz  =       (ecf - ecp)*dfz;
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

#define func XC(lda_c_hl_func)
#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_c_hl) = {
  XC_LDA_C_HL,
  XC_CORRELATION,
  "Hedin & Lundqvist",
  XC_FAMILY_LDA,
  /* can someone get me this paper, so I can find all coefficients? */
  "L Hedin and BI Lundqvist, J. Phys. C 4, 2064 (1971)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  NULL,     /* init */
  NULL,     /* end  */
  work_lda, /* lda  */
};

const XC(func_info_type) XC(func_info_lda_c_gl) = {
  XC_LDA_C_GL,
  XC_CORRELATION,
  "Gunnarson & Lundqvist",
  XC_FAMILY_LDA,
  "O Gunnarsson and BI Lundqvist, PRB 13, 4274 (1976)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  NULL,     /* init */
  NULL,     /* end  */
  work_lda, /* lda  */
};

const XC(func_info_type) XC(func_info_lda_c_vbh) = {
  XC_LDA_C_vBH,
  XC_CORRELATION,
  "von Barth & Hedin",
  XC_FAMILY_LDA,
  "U von Barth and L Hedin, J. Phys. C: Solid State Phys. 5, 1629 (1972)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  NULL,     /* init */
  NULL,     /* end  */
  work_lda, /* lda  */
};
