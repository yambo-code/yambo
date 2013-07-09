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

#define XC_LDA_XC_TETER93     20   /* Teter 93 parametrization                */

static FLOAT teter_a [4] = {0.4581652932831429, 2.217058676663745,  0.7405551735357053, 0.01968227878617998 };
static FLOAT teter_ap[4] = {0.119086804055547,  0.6157402568883345, 0.1574201515892867, 0.003532336663397157};
static FLOAT teter_b [4] = {1.0000000000000000, 4.504130959426697,  1.110667363742916,  0.02359291751427506 };
static FLOAT teter_bp[4] = {0.000000000000000,  0.2673612973836267, 0.2052004607777787, 0.004200005045691381};
  

/* the functional */
static inline void 
func(const XC(lda_type) *p, XC(lda_rs_zeta) *r)
{
  FLOAT mrs0, mrs1, mrs2, mrs3, mrs4;
  FLOAT aa[4], bb[4];
  FLOAT fz[4];

  FLOAT nn, dd, dd2, dd3, invdd;
  FLOAT DnnDrs, DddDrs, DnnDz, DddDz;
  FLOAT D2nnDrs2, D2nnDz2, D2nnDrsz, D2ddDrs2, D2ddDz2, D2ddDrsz;
  FLOAT D3nnDrs3, D3nnDrs2z, D3nnDrsz2, D3nnDz3, D3ddDrs3, D3ddDrs2z, D3ddDrsz2, D3ddDz3;
  int ii;

  /* Wigner radius */
  mrs0 = 1.0;
  mrs1 = r->rs[1];
  mrs2 = r->rs[2];
  mrs3 = mrs1*mrs2;
  mrs4 = mrs1*mrs3;
  
  if(p->nspin != XC_UNPOLARIZED){
    XC(fast_fzeta)(r->zeta, p->nspin, r->order, fz);

    for(ii=0; ii < 4; ii++){
      aa[ii] = teter_a[ii] + teter_ap[ii]*fz[0];
      bb[ii] = teter_b[ii] + teter_bp[ii]*fz[0];
    }

  } else {

   fz[0] = 0.0;
   fz[1] = 0.0;
   fz[2] = (8.0/9.0)/FZETAFACTOR;
   fz[3] = 0.0;

   for(ii=0; ii < 4; ii++){
     aa[ii] = teter_a[ii];
     bb[ii] = teter_b[ii];
   }
  }

  nn = aa[0]*mrs0 + aa[1]*mrs1 + aa[2]*mrs2 + aa[3]*mrs3;
  dd = bb[0]*mrs1 + bb[1]*mrs2 + bb[2]*mrs3 + bb[3]*mrs4;

  invdd = 1.0/dd;

  r->zk = -nn*invdd;

  if(r->order < 1) return; /* nothing else to do */

  DnnDrs = aa[1] + aa[2]*2.0*mrs1 + aa[3]*3.0*mrs2;
  DddDrs = bb[0] + bb[1]*2.0*mrs1 + bb[2]*3.0*mrs2 + 4.0*bb[3]*mrs3;

  r->dedrs = -(DnnDrs - DddDrs*nn*invdd)*invdd;

  if(p->nspin == XC_POLARIZED){

    DnnDz  = (teter_ap[0]*mrs0 + teter_ap[1]*mrs1 + teter_ap[2]*mrs2 + teter_ap[3]*mrs3)*fz[1];
    DddDz  = (teter_bp[0]*mrs1 + teter_bp[1]*mrs2 + teter_bp[2]*mrs3 + teter_bp[3]*mrs4)*fz[1];
    
    r->dedz  = -(DnnDz - DddDz*nn*invdd)*invdd;
  }

  if(r->order < 2) return; /* nothing else to do */

  D2nnDrs2 = 2*aa[2] + 3*2*aa[3]*mrs1;
  D2ddDrs2 = 2*bb[1] + 3*2*bb[2]*mrs1 + 4*3*bb[3]*mrs2;

  D2nnDrsz = (teter_ap[1] + 2*teter_ap[2]*mrs1 + 3*teter_ap[3]*mrs2)*fz[1];
  D2ddDrsz = (teter_bp[0] + 2*teter_bp[1]*mrs1 + 3*teter_bp[2]*mrs2 + 4*teter_bp[3]*mrs3)*fz[1];

  D2nnDz2  = (teter_ap[0]*mrs0 + teter_ap[1]*mrs1 + teter_ap[2]*mrs2 + teter_ap[3]*mrs3)*fz[2];
  D2ddDz2  = (teter_bp[0]*mrs1 + teter_bp[1]*mrs2 + teter_bp[2]*mrs3 + teter_bp[3]*mrs4)*fz[2];

  dd2      = dd*dd;
  dd3      = dd*dd2;

  r->d2edrs2  = -((D2nnDrs2*dd - D2ddDrs2*nn)*dd -
		  2*DddDrs*(DnnDrs*dd - DddDrs*nn))/dd3;
  r->d2edz2   = -((D2nnDz2*dd  - D2ddDz2*nn)*dd -
		  2*DddDz* (DnnDz*dd  - DddDz*nn)) /dd3;
  r->d2edrsz  = -((D2nnDrsz*dd + DnnDrs*DddDz - D2ddDrsz*nn - DddDrs*DnnDz)*dd -
		  2*DddDz* (DnnDrs*dd - DddDrs*nn))/dd3;

  if(r->order < 3) return; /* nothing else to do */

  D3nnDrs3  = 3*2*aa[3];
  D3ddDrs3  = 3*2*bb[2] + 4*3*2*bb[3]*mrs1;
  
  D3nnDrs2z = (2*teter_ap[2] + 3*2*teter_ap[3]*mrs1)*fz[1];
  D3ddDrs2z = (2*teter_bp[1] + 3*2*teter_bp[2]*mrs1 + 4*3*teter_bp[3]*mrs2)*fz[1];

  D3nnDrsz2 = (teter_ap[1] + 2*teter_ap[2]*mrs1 + 3*teter_ap[3]*mrs2)*fz[2];
  D3ddDrsz2 = (teter_bp[0] + 2*teter_bp[1]*mrs1 + 3*teter_bp[2]*mrs2 + 4*teter_bp[3]*mrs3)*fz[2];

  D3nnDz3   = (teter_ap[0]*mrs0 + teter_ap[1]*mrs1 + teter_ap[2]*mrs2 + teter_ap[3]*mrs3)*fz[3];
  D3ddDz3   = (teter_bp[0]*mrs1 + teter_bp[1]*mrs2 + teter_bp[2]*mrs3 + teter_bp[3]*mrs4)*fz[3];

  r->d3edrs3   = (- nn*(6.0*DddDrs*DddDrs*DddDrs - 6.0*dd*DddDrs*D2ddDrs2 + dd2*D3ddDrs3)
		  + dd*(6.0*DddDrs*DddDrs*DnnDrs - 3.0*dd*DddDrs*D2nnDrs2 +
			dd*(-3.0*DnnDrs*D2ddDrs2 + dd*D3nnDrs3)));
  r->d3edrs3  /= -dd3*dd;

  r->d3edz3    = (- nn*(6.0*DddDz*DddDz*DddDz - 6.0*dd*DddDz*D2ddDz2 + dd2*D3ddDz3)
		  + dd*(6.0*DddDz*DddDz*DnnDz - 3.0*dd*DddDz*D2nnDz2 +
			dd*(-3.0*DnnDz*D2ddDz2 + dd*D3nnDz3)));
  r->d3edz3   /= -dd3*dd;
  
  r->d3edrs2z  = -(nn*(DddDz*(6.0*DddDrs*DddDrs - 2.0*dd*D2ddDrs2) + dd*(-4.0*DddDrs*D2ddDrsz + dd*D3ddDrs2z))
		   + dd*(DnnDz*(-2.0*DddDrs*DddDrs + dd*D2ddDrs2) + DddDz*(-4.0*DddDrs*DnnDrs + dd*D2nnDrs2) 
			 + dd*(2.0*DnnDrs*D2ddDrsz + 2.0*DddDrs*D2nnDrsz - dd*D3nnDrs2z)));
  r->d3edrs2z /= -dd3*dd;
  
  r->d3edrsz2  = -(nn*(DddDrs*(6.0*DddDz*DddDz - 2.0*dd*D2ddDz2) + dd*(-4.0*DddDz*D2ddDrsz + dd*D3ddDrsz2))
		   + dd*(DnnDrs*(-2.0*DddDz*DddDz + dd*D2ddDz2) + DddDrs*(-4.0*DddDz*DnnDz + dd*D2nnDz2) 
			 + dd*(2.0*DnnDz*D2ddDrsz + 2.0*DddDz*D2nnDrsz - dd*D3nnDrsz2)));
  r->d3edrsz2 /= -dd3*dd;
}

#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_xc_teter93) = {
  XC_LDA_XC_TETER93,
  XC_EXCHANGE_CORRELATION,
  "Teter 93",
  XC_FAMILY_LDA,
  "S Goedecker, M Teter, J Hutter, PRB 54, 1703 (1996)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  NULL,     /* init */
  NULL,     /* end  */
  work_lda, /* lda  */
};
