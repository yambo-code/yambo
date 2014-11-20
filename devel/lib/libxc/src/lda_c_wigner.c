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

#define XC_LDA_C_WIGNER  2   /* Wigner parametrization       */

static inline void 
func(const XC(func_type) *p, XC(lda_work_t) *r)
{
  static FLOAT a = -0.44, b = 7.8;
  FLOAT t, t2;
  
  t   =  b + r->rs[1];
  r->zk =  a/t;

  if(r->order < 1) return;

  t2       = t*t;
  r->dedrs = -a/t2;
  r->dedz  = 0.0;

  if(r->order < 2) return;

  r->d2edrs2 = 2.0*a/(t2*t);
  r->d2edrsz = r->d2edz2 = 0.0;

  if(r->order < 3) return;

  r->d3edrs3 = -2.0*3.0*a/(t2*t2);
  r->d3edrs2z = r->d3edrsz2 = r->d3edz3 = 0.0;
}

#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_c_wigner) = {
  XC_LDA_C_WIGNER,
  XC_CORRELATION,
  "Wigner",
  XC_FAMILY_LDA,
  "EP Wigner, Trans. Faraday Soc. 34, 678 (1938)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  1e-32, 0.0, 0.0, 1e-32,
  NULL,     /* init */
  NULL,     /* end  */
  work_lda, /* lda  */
};
