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
 Wigner's parametrization from the low density limit
************************************************************************/

#define XC_LDA_C_GOMBAS  24   /* Gombas parametrization       */

static inline void 
func(const XC(func_type) *p, XC(lda_work_t) *r)
{
  static FLOAT a1=-0.0357, a2=0.0562, b1=-0.0311, b2=2.39;
  FLOAT t1, t2, cnst_rs, x;
  
  cnst_rs = CBRT(4.0*M_PI/3.0);
  x = cnst_rs*r->rs[1];

  t1 = 1.0 + a2*x;
  t2 = x + b2;

  r->zk = a1/t1 + b1*log(t2/x);

  if(r->order < 1) return;

  r->dedrs = -a1*a2/(t1*t1) - b1*b2/(x*t2);
  r->dedrs*= cnst_rs;
  r->dedz  = 0.0;

  if(r->order < 2) return;

  r->d2edrs2 = 2.0*a1*a2*a2/(t1*t1*t1) + b1*(1.0/(x*x) - 1.0/(t2*t2));
  r->d2edrs2*= cnst_rs*cnst_rs;
  r->d2edrsz = r->d2edz2 = 0.0;

  if(r->order < 3) return;

  r->d3edrs3 = -6*a1*a2*a2*a2/(t1*t1*t1*t1) - 2.0*b1/(x*x*x) + 2.0*b1/(t2*t2*t2);
  r->d3edrs3*= cnst_rs*cnst_rs*cnst_rs;
  r->d3edrs2z = r->d3edrsz2 = r->d3edz3 = 0.0;
}

#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_c_gombas) = {
  XC_LDA_C_GOMBAS,
  XC_CORRELATION,
  "Gombas",
  XC_FAMILY_LDA,
  "P. Gombas, Pseudopotentiale (Springer-Verlag, Wien, New York, 1967)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  1e-32, 0.0, 0.0, 1e-32,
  NULL,     /* init */
  NULL,     /* end  */
  work_lda, /* lda  */
};
