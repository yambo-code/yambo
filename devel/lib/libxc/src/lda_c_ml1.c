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

#include <stdlib.h>
#include <assert.h>

#include "util.h"

/************************************************************************
 Correlation energy of Proynov and Salahub
************************************************************************/

#define XC_LDA_C_ML1    22   /* Modified LSD (version 1) of Proynov and Salahub */
#define XC_LDA_C_ML2    23   /* Modified LSD (version 2) of Proynov and Salahub */

static void 
lda_c_ml1_init(XC(func_type) *p)
{
  switch(p->info->number){
  case XC_LDA_C_ML2:
    p->func = 1; break;
  default:
    p->func = 0; break;
  }
}


/* the functional */
static inline void 
func(const XC(func_type) *p, XC(lda_work_t) *r)
{
  static FLOAT fc[2] = {0.2026, 0.266}, q[2] = {0.084, 0.5}, C = 6.187335;
  static FLOAT b[6] = {2.763169, 1.757515, 1.741397, 0.568985, 1.572202, 1.885389};

  FLOAT cnst_rs, nn, zp3, zm3, alpha, beta, gamma, k, Q;
  FLOAT dalpha, dbeta, dQ, dkdrs, dkdz;

  cnst_rs = CBRT(3.0/(4*M_PI));

  alpha = fc[p->func]*(POW(1 + r->zeta, q[p->func]) + POW(1.0 - r->zeta, q[p->func]));

  zp3   = CBRT(1.0 + r->zeta);
  zm3   = CBRT(1.0 - r->zeta);
  beta  = zp3*zm3/(zp3 + zm3);

  k     = C*alpha*beta*cnst_rs/r->rs[1];

  Q = (k == 0.0) ? -FLT_MAX : -b[0]/(1.0 + b[1]*k) + b[2]/k*log(1.0 + b[3]/k) + b[4]/k - b[5]/(k*k);

  gamma = (1 - r->zeta*r->zeta)/4.0;
  nn    = POW(cnst_rs/r->rs[1], 3);
  r->zk = 0.5*nn*gamma*Q;

  if(r->order < 1) return;

  dQ = (k == 0.0) ? FLT_MAX : b[0]*b[1]/((1.0 + b[1]*k)*(1.0 + b[1]*k)) - b[2]*b[3]/((b[3] + k)*(k*k))
    - b[2]*log(1.0 + b[3]/k)/(k*k) - b[4]/(k*k) + 2.0*b[5]/(k*k*k);

  dkdrs = -k/r->rs[1];

  if(ABS(r->zeta) == 1.0)
    dalpha = dbeta = 0.0;
  else{
    dalpha = fc[p->func]*q[p->func]*(POW(1 + r->zeta, q[p->func] - 1.0) - POW(1.0 - r->zeta, q[p->func] - 1.0));
    dbeta  = (-2.0*r->zeta - zm3*zm3*zp3 + zm3*zp3*zp3)/(3.0*zm3*zm3*zp3*zp3*(zp3 + zm3));
  }
  dkdz   = C*(dalpha*beta + alpha*dbeta)*cnst_rs/r->rs[1];

  r->dedrs = 0.5*nn*gamma*(dQ*dkdrs - 3.0*Q/r->rs[1]);
  r->dedz  = 0.5*nn*(-r->zeta*Q/2.0 + gamma*dQ*dkdz);
}

#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_c_ml1) = {
  XC_LDA_C_ML1,
  XC_CORRELATION,
  "Modified LSD (version 1) of Proynov and Salahub",
  XC_FAMILY_LDA,
  "EI Proynov and D Salahub, Phys. Rev. B 49, 7874 (1994)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 0.0, 0.0, 1e-32,
  lda_c_ml1_init,
  NULL,
  work_lda,
};

const XC(func_info_type) XC(func_info_lda_c_ml2) = {
  XC_LDA_C_ML2,
  XC_CORRELATION,
  "Modified LSD (version 2) of Proynov and Salahub",
  XC_FAMILY_LDA,
  "EI Proynov and D Salahub, Phys. Rev. B 49, 7874 (1994)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-32, 0.0, 0.0, 1e-32,
  lda_c_ml1_init,
  NULL,
  work_lda,
};
