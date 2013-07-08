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
#include "util.h"

/************************************************************************
 Random Phase Approximation (RPA)
************************************************************************/

#define XC_LDA_C_RPA  3   /* Random Phase Approximation   */

static inline void 
func(const XC(lda_type) *p, XC(lda_rs_zeta) *r)
{
  static FLOAT a = 0.0311, b = -0.048, c = 0.009, d = -0.017;
  FLOAT lrs;

  lrs = log(r->rs[1]);
  r->zk = a*lrs + b + c*r->rs[1]*lrs + d*r->rs[1];

  if(r->order < 1) return;

  r->dedrs = a/r->rs[1] + c*(lrs + 1.0) + d;
  /* no spin polarization for the moment */
  r->dedz  = 0.0;

  if(r->order < 2) return;

  r->d2edrs2 = -a/r->rs[2] + c/r->rs[1];
  r->d2edrsz = r->d2edz2 = 0.0;

  if(r->order < 3) return;

  r->d3edrs3 = 2.0*a/(r->rs[1]*r->rs[2]) - c/r->rs[2];
  r->d3edrs2z = r->d3edrsz2 = r->d3edz3 = 0.0;
}

#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_c_rpa) = {
  XC_LDA_C_RPA,
  XC_CORRELATION,
  "Random Phase Approximation (RPA)",
  XC_FAMILY_LDA,
  "M Gell-Mann and KA Brueckner, Phys. Rev. 106, 364 (1957)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  NULL,     /* init */
  NULL,     /* end  */
  work_lda, /* lda  */
};
