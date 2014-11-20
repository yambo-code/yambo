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

#define XC_MGGA_X_PKZB          213 /* Perdew, Kurth, Zupan, and Blaha */

static const FLOAT kappa = 0.804;

static void Eq_14(FLOAT pp, FLOAT qt, int order, FLOAT *xx, FLOAT *dxxdpp, FLOAT *dxxdqt)
{
  static const FLOAT 
    DD = 0.113,
    a1 = 10.0/81.0, 
    a2 = 146.0/2025.0, 
    a3 = -73.0/405.0,
    a4 = 0.131957187845257783631757384393; /* DD + 100.0/(81.0*81.0*kappa); */

  *xx = a1*pp + a2*qt*qt + a3*qt*pp + a4*pp*pp;

  if(order < 1) return;

  *dxxdpp = a1 + a3*qt + 2.0*a4*pp;
  *dxxdqt = 2.0*a2*qt + a3*pp;
}

static void 
func(const XC(func_type) *pt, XC(mgga_work_x_t) *r)
{
  FLOAT x2s2, pp, rr, qt, xx, dxxdpp, dxxdqt;
  FLOAT kpxx, k2, dqtdrr, dqtdpp, dfdxx;

  x2s2 = X2S*X2S;
  k2   = kappa*kappa;

  pp = x2s2*r->x*r->x;
  rr = x2s2*r->t;

  qt = 6.0*rr - 9.0/20.0 - pp/12.0;

  Eq_14(pp, qt, r->order, &xx, &dxxdpp, &dxxdqt);

  kpxx = kappa + xx;
  r->f = 1.0 + kappa - k2/kpxx;

  if(r->order < 1) return;

  dqtdrr = 6.0;
  dqtdpp = -1.0/12.0;

  dfdxx = k2/(kpxx*kpxx);

  r->dfdx = dfdxx*(dxxdpp + dxxdqt*dqtdpp)*2.0*x2s2*r->x;
  r->dfdt = dfdxx*dxxdqt*dqtdrr*x2s2;
  r->dfdu = 0.0;

  if(r->order < 2) return;

}


#include "work_mgga_x.c"


XC(func_info_type) XC(func_info_mgga_x_pkzb) = {
  XC_MGGA_X_PKZB,
  XC_EXCHANGE,
  "Perdew, Kurth, Zupan, and Blaha",
  XC_FAMILY_MGGA,
  "JP Perdew, S Kurth, A Zupan, and P. Blaha, Phys. Rev. Lett. 82, 2544-2547 (1999)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 1e-32, 1e-32, 1e-32,
  NULL,
  NULL, NULL, NULL,
  work_mgga_x,
};
