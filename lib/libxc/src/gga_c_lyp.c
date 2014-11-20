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

#define XC_GGA_C_LYP  131 /* Lee, Yang & Parr */

typedef struct{
  FLOAT A, B, c, d;
} gga_c_lyp_params;

void XC(gga_c_lyp_init)(XC(func_type) *p)
{
  assert(p->params == NULL);

  p->params = malloc(sizeof(gga_c_lyp_params));

  /* values of constants in standard LYP functional */
  XC(gga_c_lyp_set_params)(p, 0.04918, 0.132, 0.2533, 0.349);
}


void XC(gga_c_lyp_set_params)(XC(func_type) *p, FLOAT A, FLOAT B, FLOAT c, FLOAT d)
{
  gga_c_lyp_params *params;

  assert(p != NULL && p->params != NULL);
  params = (gga_c_lyp_params *) (p->params);

  params->A = A;
  params->B = B;
  params->c = c;
  params->d = d;
}


static inline void 
func(const XC(func_type) *p, XC(gga_work_c_t) *r)
{
  gga_c_lyp_params *params;

  FLOAT AA, BB, cc, dd, Cf; /* sortcuts for parameters */
  FLOAT cnst_rs, xt2, opz, omz, opz23, omz23, opz53, omz53, opz83, omz83, zeta2, opdrs;
  FLOAT omega, delta, domega, ddelta, d2omega, d2delta;
  FLOAT aux4, aux5, aux6, xs02, xs12;
  FLOAT t1, t2, t3, t4, t5, t6;
  FLOAT dt1drs, dt2drs, dt4drs, dt5drs;
  FLOAT dt1dz, dt2dz, dt3dz, dt4dz, dt5dz, dt6dz;
  FLOAT d2t1drs2, d2t2drs2, d2t4drs2, d2t5drs2;
  FLOAT d2t1drsz, d2t2drsz, d2t4drsz, d2t5drsz;
  FLOAT d2t1dz2, d2t2dz2, d2t3dz2, d2t4dz2, d2t5dz2, d2t6dz2;

  assert(p->params != NULL);
  params = (gga_c_lyp_params *)(p->params);

  cnst_rs = CBRT(4.0*M_PI/3.0);
  Cf      = 3.0*POW(3.0*M_PI*M_PI, 2.0/3.0)/10.0;
  xt2     = r->xt*r->xt;
  xs02    = r->xs[0]*r->xs[0];
  xs12    = r->xs[1]*r->xs[1];

  AA = params->A;
  BB = params->B;
  cc = params->c*cnst_rs;
  dd = params->d*cnst_rs;

  zeta2 = r->zeta*r->zeta;
  opz = 1.0 + r->zeta;
  omz = 1.0 - r->zeta;
  opz23 = POW(opz, 2.0/3.0);
  omz23 = POW(omz, 2.0/3.0);
  opz53 = opz*opz23;
  omz53 = omz*omz23;
  opz83 = opz*opz53;
  omz83 = omz*omz53;

  opdrs = 1.0/(1.0 + dd*r->rs);
  omega = BB*exp(-cc*r->rs)*opdrs;
  delta = (cc + dd*opdrs)*r->rs;

  aux6 = 1.0/POW(2.0, 8.0/3.0);
  aux4 = aux6/4.0;
  aux5 = aux4/(9.0*2.0);

  t1 = -(1.0 - zeta2)/(1.0 + dd*r->rs);
  t2 = -xt2*((1.0 - zeta2)*(47.0 - 7.0*delta)/(4.0*18.0) - 2.0/3.0);
  t3 = -Cf/2.0*(1.0 - zeta2)*(opz83 + omz83);
  t4 =  aux4*(1.0 - zeta2)*(5.0/2.0 - delta/18.0)*(xs02*opz83 + xs12*omz83);
  t5 =  aux5*(1.0 - zeta2)*(delta - 11.0)*(xs02*opz*opz83 + xs12*omz*omz83);
  t6 = -aux6*(2.0/3.0*(xs02*opz83 + xs12*omz83) -
	      opz*opz*xs12*omz83/4.0 - omz*omz*xs02*opz83/4.0);

  r->f  = AA*(t1 + omega*(t2 + t3 + t4 + t5 + t6));

  if(r->order < 1) return;

  domega = -omega*(cc + dd*opdrs);
  ddelta = cc + dd*opdrs*opdrs;

  dt1drs = -dd*t1/(1.0 + dd*r->rs);
  dt2drs =  xt2 *(1.0 - zeta2)*ddelta*7.0/(4.0*18.0);
  dt4drs = -aux4*(1.0 - zeta2)*ddelta/18.0*(xs02*opz83 + xs12*omz83);
  dt5drs =  aux5*(1.0 - zeta2)*ddelta*(xs02*opz*opz83 + xs12*omz*omz83);
  r->dfdrs =  AA*(dt1drs + domega*(t2 + t3 + t4 + t5 + t6) + omega*(dt2drs + dt4drs + dt5drs));
  
  dt1dz =  2.0*r->zeta/(1.0 + dd*r->rs);
  dt2dz =  xt2*2.0*r->zeta*(47.0 - 7.0*delta)/(4.0*18.0);
  dt3dz = -Cf/2.0*(-2.0*r->zeta*(opz83 + omz83) + (1.0 - zeta2)*8.0/3.0*(opz53 - omz53));
  dt4dz =  aux4*(5.0/2.0 - delta/18.0)*
    (-2.0*r->zeta*(xs02*opz83 + xs12*omz83) + 
     (1.0 - zeta2)*8.0/3.0*(xs02*opz53 - xs12*omz53));
  dt5dz =  aux5*(delta - 11.0)*
    (-2.0*r->zeta*(xs02*opz*opz83 + xs12*omz*omz83) + 
     (1.0 - zeta2)*11/3.0*(xs02*opz83 - xs12*omz83));
  dt6dz = -aux6*(16.0/9.0*(xs02*opz53 - xs12*omz53) - 
		 1.0/2.0*(opz*xs12*omz83 - omz*xs02*opz83) +
		 2.0/3.0*(opz*opz*xs12*omz53 - omz*omz*xs02*opz53));
  r->dfdz = AA*(dt1dz + omega*(dt2dz + dt3dz + dt4dz + dt5dz + dt6dz));

  r->dfdxt = -2.0*AA*omega*r->xt*((1.0 - zeta2)*(47.0 - 7.0*delta)/(4.0*18.0) - 2.0/3.0);

  r->dfdxs[0] = AA*omega*2.0*r->xs[0]*
    (aux4*(1.0 - zeta2)*(5.0/2.0 - delta/18.0)*opz83 +
     aux5*(1.0 - zeta2)*(delta - 11.0)*opz*opz83 -
     aux6*(2.0/3.0*opz83 - omz*omz*opz83/4.0)
     );
  r->dfdxs[1] = AA*omega*2.0*r->xs[1]*
    (aux4*(1.0 - zeta2)*(5.0/2.0 - delta/18.0)*omz83 +
     aux5*(1.0 - zeta2)*(delta - 11.0)*omz*omz83 -
     aux6*(2.0/3.0*omz83 - opz*opz*omz83/4.0)
     );

  if(r->order < 2) return;

  d2omega = -domega*(cc + dd*opdrs) + dd*dd*omega*opdrs*opdrs;
  d2delta = -2.0*dd*dd*opdrs*opdrs*opdrs;

  d2t1drs2 = -2.0*dd*dt1drs/(1.0 + dd*r->rs);
  d2t2drs2 =  xt2 *(1.0 - zeta2)*d2delta*7.0/(4.0*18.0);
  d2t4drs2 = -aux4*(1.0 - zeta2)*d2delta/18.0*(xs02*opz83 + xs12*omz83);
  d2t5drs2 =  aux5*(1.0 - zeta2)*d2delta*(xs02*opz*opz83 + xs12*omz*omz83);
  r->d2fdrs2 = AA*(d2t1drs2 + d2omega*(t2 + t3 + t4 + t5 + t6) + 
		 2.0*domega*(dt2drs + dt4drs + dt5drs) + omega*(d2t2drs2 + d2t4drs2 + d2t5drs2));
  
  d2t1drsz = -dd*dt1dz/(1.0 + dd*r->rs); 
  d2t2drsz = -xt2*2.0*r->zeta*7.0*ddelta/(4.0*18.0);
  d2t4drsz = -aux4*ddelta/18.0*
    (-2.0*r->zeta*(xs02*opz83 + xs12*omz83) + (1.0 - zeta2)*8.0/3.0*(xs02*opz53 - xs12*omz53));
  d2t5drsz =  aux5*ddelta*
    (-2.0*r->zeta*(xs02*opz*opz83 + xs12*omz*omz83) + (1.0 - zeta2)*11/3.0*(xs02*opz83 - xs12*omz83));

  r->d2fdrsz = AA*(d2t1drsz + domega*(dt2dz + dt3dz + dt4dz + dt5dz + dt6dz) + omega*(d2t2drsz + d2t4drsz + d2t5drsz));

  r->d2fdrsxt = -2.0*AA*r->xt*(domega*((1.0 - zeta2)*(47.0 - 7.0*delta)/(4.0*18.0) - 2.0/3.0) -
			  omega*(1.0 - zeta2)*7.0*ddelta/(4.0*18.0));

  r->d2fdrsxs[0] = r->dfdxs[0]*domega/omega + AA*omega*2.0*r->xs[0]*ddelta*
    (-aux4*(1.0 - zeta2)/18.0*opz83 + aux5*(1.0 - zeta2)*opz*opz83);
  r->d2fdrsxs[1] = r->dfdxs[1]*domega/omega + AA*omega*2.0*r->xs[1]*ddelta*
    (-aux4*(1.0 - zeta2)/18.0*omz83 + aux5*(1.0 - zeta2)*omz*omz83);

  d2t1dz2 =  2.0/(1.0 + dd*r->rs);
  d2t2dz2 =  xt2*2.0*(47.0 - 7.0*delta)/(4.0*18.0);
  d2t3dz2 = -Cf/2.0*(-2.0*(opz83 + omz83) - 4.0*r->zeta*8.0/3.0*(opz53 - omz53) + (1.0 - zeta2)*40.0/9.0*(opz23 + omz23));
  d2t4dz2 =  aux4*(5.0/2.0 - delta/18.0)*
    (-2.0*(xs02*opz83 + xs12*omz83) - 4.0*r->zeta*8.0/3.0*(xs02*opz53 - xs12*omz53) +
     (1.0 - zeta2)*40.0/9.0*(xs02*opz23 + xs12*omz23));
  d2t5dz2 =  aux5*(delta - 11.0)*
    (-2.0*(xs02*opz*opz83 + xs12*omz*omz83) - 4.0*r->zeta*11.0/3.0*(xs02*opz83 - xs12*omz83) +
     (1.0 - zeta2)*88.0/9.0*(xs02*opz53 + xs12*omz53));
  d2t6dz2 = -aux6*(80.0/27.0*(xs02*opz23 + xs12*omz23) - 
		   1.0/2.0*(xs12*omz83 + xs02*opz83) +
		   8.0/3.0*(opz*xs12*omz53 + omz*xs02*opz53) -
		   10.0/9.0*(opz*opz*xs12*omz23 + omz*omz*xs02*opz23));
  r->d2fdz2 = AA*(d2t1dz2 + omega*(d2t2dz2 + d2t3dz2 + d2t4dz2 + d2t5dz2 + d2t6dz2));
  
  r->d2fdzxt = 4.0*AA*omega*r->xt*r->zeta*(47.0 - 7.0*delta)/(4.0*18.0);

  r->d2fdzxs[0]  = 2.0*AA*omega*r->xs[0]*
    (aux4*(5.0/2.0 - delta/18.0)*(-2.0*r->zeta*opz83 + (1.0 - zeta2)*8.0/3.0*opz53) +
     aux5*(delta - 11.0)*(-2.0*r->zeta*opz*opz83 + (1.0 - zeta2)*11.0/3.0*opz83) -
     aux6*(16.0/9.0*opz53 + 1.0/2.0*omz*opz83 - 2.0/3.0*omz*omz*opz53));
  r->d2fdzxs[1]  = 2.0*AA*omega*r->xs[1]*
    (aux4*(5.0/2.0 - delta/18.0)*(-2.0*r->zeta*omz83 - (1.0 - zeta2)*8.0/3.0*omz53) +
     aux5*(delta - 11.0)*(-2.0*r->zeta*omz*omz83 - (1.0 - zeta2)*11/3.0*omz83) +
     aux6*(16.0/9.0*omz53 + 1.0/2.0*opz*omz83 - 2.0/3.0*opz*opz*omz53));

  r->d2fdxt2 = r->dfdxt/r->xt;

  r->d2fdxtxs[0] = 0.0;
  r->d2fdxtxs[1] = 0.0;

  r->d2fdxs2[0]  = r->dfdxs[0]/r->xs[0];
  r->d2fdxs2[1]  = 0.0;
  r->d2fdxs2[2]  = r->dfdxs[1]/r->xs[1];
}

#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_lyp) = {
  XC_GGA_C_LYP,
  XC_CORRELATION,
  "Lee, Yang & Parr",
  XC_FAMILY_GGA,
  "C Lee, W Yang and RG Parr, Phys. Rev. B 37, 785 (1988)\n"
  "B Miehlich, A Savin, H Stoll and H Preuss, Chem. Phys. Lett. 157, 200 (1989)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 1e-32, 0.0, 1e-32,
  XC(gga_c_lyp_init), 
  NULL,
  NULL,
  work_gga_c
};
