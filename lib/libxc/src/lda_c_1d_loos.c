/*
 Copyright (C) 2006-2009 M.A.L. Marques

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

#define XC_LDA_C_1D_LOOS          26 /* P-F Loos correlation LDA     */

static FLOAT kappa, eta0, eta1, eps0, eps1;
static FLOAT c0, c1, c2, c3;

static void 
lda_c_1d_loos_init(XC(func_type) *p)
{
  eta0 = -LOG(SQRT(2.0*M_PI)) - 3.0/4.0;
  eta1 = 0.359933;
  eps0 = -M_PI*M_PI/360.0;
  eps1 = 0.00714;

  kappa = 0.3083;

  c0 = kappa*eta0;
  c1 = 4.0*kappa*eta0 + kappa*SQRT(kappa)*eta1;
  c2 = 5.0*eps0 + eps1/kappa;
  c3 = eps1;
}

static inline void
func(const XC(func_type) *p, XC(lda_work_t) *r)
{
  FLOAT aux, tt, tt2, tt3, omtt, omtt2, omtt3;
  FLOAT dttdrs, dfdtt, d2ttdrs2, d2fdtt2;

  aux = SQRT(1.0 + 4.0*kappa*r->rs[1]);
  tt  = (aux - 1.0)/(2.0*kappa*r->rs[1]);

  tt2   = tt*tt;
  tt3   = tt*tt2;
  omtt  = 1.0 - tt;
  omtt2 = omtt*omtt;
  omtt3 = omtt*omtt2;

  r->zk = tt2*(c0*omtt3 + c1*tt*omtt2 + c2*tt2*omtt + c3*tt3);
  
  if(r->order < 1) return;

  dttdrs = 1.0/(r->rs[1]*aux) + (1.0 - aux)/(2.0*kappa*r->rs[2]);
  dfdtt  = 2.0*c0*omtt3*tt + 3.0*(c1 - c0)*omtt2*tt2 + (4.0*c2 - 2.0*c1)*omtt*tt3 + (5.0*c3 - c2)*tt*tt3;

  r->dedrs = dfdtt*dttdrs;
  r->dedz  = 0.0;

  if(r->order < 2) return;

  d2ttdrs2 = -2.0*kappa/(r->rs[1]*aux*aux*aux) - 2.0/(r->rs[2]*aux) - (1.0 - aux)/(kappa*r->rs[1]*r->rs[2]);
  d2fdtt2  = 2.0*c0*omtt3 + 6.0*(c1 - 2.0*c0)*omtt2*tt + 6.0*(2.0*c2 - 2.0*c1 + c0)*omtt*tt2 + 2.0*(c1 - 4.0*c2)*tt3 + 20.0*c3*tt3;

  r->d2edrs2 = d2fdtt2*dttdrs*dttdrs + dfdtt*d2ttdrs2;
  r->d2edrsz = 0.0;
  r->d2edz2  = 0.0;
}

#define XC_DIMENSIONS 1
#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_c_1d_loos) = {
  XC_LDA_C_1D_LOOS,
  XC_CORRELATION,
  "P-F Loos correlation LDA",
  XC_FAMILY_LDA,
  "P-F Loos, arXiv:1207.6849v1 [cond-mat.str-el] (2012)",
  XC_FLAGS_1D |  XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 0.0, 0.0, 1e-32,
  lda_c_1d_loos_init,    /* init */
  NULL,                 /* end  */
  work_lda,             /* lda  */
};
