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

#define XC_LDA_K_TF      50   /* Thomas-Fermi kinetic energy functional */
#define XC_LDA_K_LP      51   /* Lee and Parr Gaussian ansatz           */

/* do not forget that our definition of the kinetic energy is |nabla n|^2,
   therefore, all formulas must multiplied by a factor of 2 */

static inline void 
func(const XC(lda_type) *p, XC(lda_rs_zeta) *r)
{
  FLOAT ax, fz, dfz, d2fz, d3fz;

  switch(p->info->number){
  case XC_LDA_K_LP:\
    /* 2 * 3*M_PI/2^(5/3) * (3/4 pi)^(2/3) = 3*M_PI*POW(3/(8*M_PI), 2/3)*/
    ax = 2.284855419517333351288618503355783851342;
    break;
  case XC_LDA_K_TF:
    /* 2 * 3/10*(3*M_PI^2)^(2/3) * (3/4 pi)^(2/3) = 3/5*POW(9*M_PI/4, 2/3) */
    ax = 2.209901131411720004197664159039271385884;
    break;
  }

  r->zk = ax/r->rs[2];

  if(p->nspin == XC_POLARIZED){
    fz  = 0.5*(POW(1.0 + r->zeta,  5.0/3.0) + POW(1.0 - r->zeta,  5.0/3.0));
    r->zk *= fz;
  }

  if(r->order < 1) return;
  
  r->dedrs = -2.0*ax/(r->rs[1]*r->rs[2]);

  if(p->nspin == XC_POLARIZED){
    dfz = 5.0/(2.0*3.0)*(POW(1.0 + r->zeta,  2.0/3.0) - POW(1.0 - r->zeta,  2.0/3.0));

    r->dedrs *=             fz;
    r->dedz   = ax/r->rs[2]*dfz;
  }

  if(r->order < 2) return;
    
  r->d2edrs2 = 2.0*3.0*ax/(r->rs[2]*r->rs[2]);

  if(p->nspin == XC_POLARIZED){
    if(ABS(r->zeta) == 1.0)
      d2fz = FLT_MAX;
    else
      d2fz = 10.0/(2.0*9.0)*(1.0/CBRT(1.0 + r->zeta) + 1.0/CBRT(1.0 - r->zeta));
    
    r->d2edrs2 *=                             fz;
    r->d2edrsz  = -2.0*ax/(r->rs[1]*r->rs[2])*dfz;
    r->d2edz2   =      ax/r->rs[2]           *d2fz;
  }

  if(r->order < 3) return;

  r->d3edrs3 = -2.0*3.0*4.0*ax/(r->rs[1]*r->rs[2]*r->rs[2]);

  if(p->nspin == XC_POLARIZED){
    if(ABS(r->zeta) == 1.0)
      d3fz = FLT_MAX;
    else
      d3fz = -10.0/(2.0*27.0)*(POW(1.0 + r->zeta,  -4.0/3.0) - POW(1.0 - r->zeta,  -4.0/3.0));

    r->d3edrs3 *= fz;
    r->d3edrs2z = 2.0*3.0*ax/(r->rs[2]*r->rs[2])*dfz;
    r->d3edrsz2 =    -2.0*ax/(r->rs[1]*r->rs[2])*d2fz;
    r->d3edz3   =         ax/r->rs[2]           *d3fz;
  }

}

#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_k_tf) = {
  XC_LDA_K_TF,
  XC_KINETIC,
  "Thomas-Fermi kinetic energy",
  XC_FAMILY_LDA,
  "LH Thomas, Proc. Cambridge Phil. Soc. 23,  542-548 (1927)\n"
  "E Fermi. Rend. Accad. Naz. Lincei 6, 602-607 (1927)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  NULL,
  NULL,
  work_lda
};

const XC(func_info_type) XC(func_info_lda_k_lp) = {
  XC_LDA_K_LP,
  XC_KINETIC,
  "Lee and Parr Gaussian ansatz for the kinetic energy",
  XC_FAMILY_LDA,
  "CL and RG Parr, Phys. Rev. A 35, 2377-2383 (1987)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  NULL,
  NULL,
  work_lda
};

