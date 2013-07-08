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

#define XC_GGA_X_HTBS         191 /* Haas, Tran, Blaha, and Schwarz  */

/* The equations to solve in order to obtain the coeficients cc are

  G(s1) = 0
  G(s2) = 1
 G'(s1) = 0
 G'(s2) = 0
G''(s1) = 0
G''(s2) = 0
*/

static FLOAT s1 = 0.6, s2 = 2.6;
static FLOAT cc[6];

static void 
gga_x_htbs_init(void *p_)
{
  FLOAT s12, s22, s1_s2, aux;

  XC(gga_type) *p = (XC(gga_type) *)p_;

  p->n_func_aux  = 2;
  p->func_aux    = (XC(func_type) **) malloc(2*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));
  p->func_aux[1] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_GGA_X_RPBE, p->nspin);
  XC(func_init)(p->func_aux[1], XC_GGA_X_WC,   p->nspin);

  s12 = s1*s1;
  s22 = s2*s2;
  s1_s2 = s1 - s2;

  aux = s1_s2*s1_s2;
  aux = 1.0/(aux*aux*s1_s2);

  cc[0] =  aux*s12*s1*(s12 - 5.0*s1*s2 + 10*s22);
  cc[1] = -aux*30.0*s12*s22;
  cc[2] =  aux*30*s1*s2*(s1 + s2);
  cc[3] = -aux*10*(s12 + 4.0*s1*s2 + s22);
  cc[4] =  aux*15.0*(s1 + s2);
  cc[5] = -aux*6.0;
}


void XC(gga_x_htbs_enhance)
  (const XC(gga_type) *p, int order, FLOAT x, 
   FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  FLOAT s, g, dg, d2g, a, da, d2a, b, db, d2b;

  s  = X2S*x;
  
  if(s > s1)
    XC(gga_x_rpbe_enhance)(p->func_aux[0]->gga, order, x, &a, &da, &d2a);
  if(s < s2)
    XC(gga_x_wc_enhance)  (p->func_aux[1]->gga, order, x, &b, &db, &d2b);

  if(s < s1)
    *f = b;
  else if(s > s2)
    *f = a;
  else{
    g  = cc[0] + s*(cc[1] + s*(cc[2] + s*(cc[3] + s*(cc[4] + s*cc[5]))));
    *f = g*a + (1.0 - g)*b;
  }

  if(order < 1) return;

  if(s < s1)
    *dfdx = db;
  else if(s > s2)
    *dfdx = da;
  else{
    dg  = cc[1] + s*(2.0*cc[2] + s*(3.0*cc[3] + s*(4.0*cc[4] + s*5.0*cc[5])));
    dg *= X2S;

    *dfdx = dg*(a - b) + g*(da - db) + db;
  }

  if(order < 2) return;
  
   if(s < s1)
    *d2fdx2 = d2b;
  else if(s > s2)
    *d2fdx2 = d2a;
  else{ 
    d2g  = 2.0*cc[2] + s*(6.0*cc[3] + s*(12.0*cc[4] + s*20.0*cc[5]));
    d2g *= X2S*X2S;

    *d2fdx2  = d2g*(a - b) + 2.0*dg*(da - db) + g*(d2a - d2b) + d2b;
  }
}


#define func XC(gga_x_htbs_enhance)
#include "work_gga_x.c"


const XC(func_info_type) XC(func_info_gga_x_htbs) = {
  XC_GGA_X_HTBS,
  XC_EXCHANGE,
  "Haas, Tran, Blaha, and Schwarz",
  XC_FAMILY_GGA,
  "P Haas, F Tran, P Blaha, and K Schwarz, Phys. Rev. B 83, 205117 (2011)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_x_htbs_init, 
  NULL, NULL,
  work_gga_x
};
