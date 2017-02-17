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

#define XC_LDA_X_2D  19 /* Exchange in 2D */

static inline void 
func(const XC(lda_type) *p, XC(lda_rs_zeta) *r)
{
  const FLOAT ax = -0.600210877438070713036799460671; /* -4/3*SQRT(2)/M_PI */
  FLOAT fz, dfz, d2fz, d3fz;

  r->zk = ax/r->rs[1];
  if(p->nspin == XC_POLARIZED){
    fz  = 0.5*(POW(1.0 + r->zeta,  3.0/2.0) + POW(1.0 - r->zeta,  3.0/2.0));
    r->zk *= fz;
  }

  if(r->order < 1) return;
  
  r->dedrs = -ax/r->rs[2];
  if(p->nspin == XC_POLARIZED){
    dfz = 3.0/4.0*(SQRT(1.0 + r->zeta) - SQRT(1.0 - r->zeta));

    r->dedrs *= fz;
    r->dedz   = ax/r->rs[1]*dfz;
  }

  if(r->order < 2) return;

  r->d2edrs2 = 2.0*ax/(r->rs[1]*r->rs[2]);
  if(p->nspin == XC_POLARIZED){
    d2fz = 3.0/8.0*(1.0/SQRT(1.0 + r->zeta) + 1.0/SQRT(1.0 - r->zeta));

    r->d2edrs2 *=                fz;
    r->d2edrsz  = -ax/r->rs[2]* dfz;
    r->d2edz2   =  ax/r->rs[1]*d2fz;
  }

  if(r->order < 3) return;

  r->d3edrs3 = -6.0*ax/(r->rs[2]*r->rs[2]);
  if(p->nspin == XC_POLARIZED){
    d3fz = -3.0/16.0*(POW(1.0 + r->zeta,  -3.0/2.0) - POW(1.0 - r->zeta, -3.0/2.0));

    r->d3edrs3 *=                             fz;
    r->d3edrs2z = 2.0*ax/(r->rs[1]*r->rs[2])*dfz;
    r->d3edrsz2 =    -ax/r->rs[2]*          d2fz;
    r->d3edz3   =     ax/r->rs[1]*          d3fz;
  }   
}

#define XC_DIMENSIONS 2
#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_x_2d) = {
  XC_LDA_X_2D,
  XC_EXCHANGE,
  "Slater exchange",
  XC_FAMILY_LDA,
  "PAM Dirac, Proceedings of the Cambridge Philosophical Society 26, 376 (1930)\n"
  "F Bloch, Zeitschrift fuer Physik 57, 545 (1929)",
  XC_FLAGS_2D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  NULL, NULL,
  work_lda
};

