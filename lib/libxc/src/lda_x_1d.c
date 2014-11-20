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

#define XC_LDA_X_1D          21 /* Exchange in 1D     */

typedef struct{
  int interaction;  /* 0: exponentially screened; 1: soft-Coulomb */
  FLOAT bb;         /* screening parameter beta */
} lda_x_1d_params;

static void 
lda_x_1d_init(XC(func_type) *p)
{
  assert(p->params == NULL);
  p->params = malloc(sizeof(lda_x_1d_params));

  /* default value is soft-Coulomb with beta=1.0 */
  XC(lda_x_1d_set_params)(p, 1, 1.0);
}


void 
XC(lda_x_1d_set_params)(XC(func_type) *p, int interaction, FLOAT bb)
{
  lda_x_1d_params *params;

  assert(p != NULL && p->params != NULL);
  params = (lda_x_1d_params *)(p->params);

  assert(interaction == 0 || interaction == 1);

  params->interaction = interaction;
  params->bb          = bb;
}


static inline FLOAT FT_inter(FLOAT x, int interaction)
{
  assert(interaction == 0 || interaction == 1);

  if(interaction == 0){
    FLOAT x2 = x*x;
    return expint_e1(x2)*exp(x2);
  }else
    return 2.0*bessel_K0(x); 
}


static void func1(FLOAT *x, int n, void *ex)
{
  int interaction = *(int *)ex;
  int ii;
  
  for(ii=0; ii<n; ii++)
    x[ii] = FT_inter(x[ii], interaction);
}


static void func2(FLOAT *x, int n, void *ex)
{
  int interaction = *(int *)ex;
  int ii;
  
  for(ii=0; ii<n; ii++)
    x[ii] = x[ii]*FT_inter(x[ii], interaction);
}


static inline void
func(const XC(func_type) *p, XC(lda_work_t) *r)
{
  static int spin_sign[2] = {+1, -1};
  static int spin_fact[2] = { 2,  1};

  int interaction, is;
  FLOAT bb, R, int1[2], int2[2];

  assert(p->params != NULL);
  interaction = ((lda_x_1d_params *)p->params)->interaction;
  bb  =         ((lda_x_1d_params *)p->params)->bb;

  r->zk = 0.0;
  for(is=0; is<p->nspin; is++){
    R = M_PI*bb*(1.0 + spin_sign[is]*r->zeta)/(2.0*r->rs[1]);

    if(R == 0.0) continue;

    int1[is] = integrate(func1, (void *)(&interaction), 0.0, R);
    int2[is] = integrate(func2, (void *)(&interaction), 0.0, R);

    r->zk -= (1.0 + spin_sign[is]*r->zeta) *
      (int1[is] - int2[is]/R);
  }
  r->zk *= spin_fact[p->nspin-1]/(4.0*M_PI*bb);

  if(r->order < 1) return;
  
  r->dedrs = 0.0;
  r->dedz  = 0.0;
  for(is=0; is<p->nspin; is++){
    if(1.0 + spin_sign[is]*r->zeta == 0.0) continue;

    r->dedrs +=               int2[is];
    r->dedz  -= spin_sign[is]*int1[is];
  }
  r->dedrs *= spin_fact[p->nspin-1]/(2.0*M_PI*M_PI*bb*bb);
  r->dedz  *= spin_fact[p->nspin-1]/(4.0*M_PI*bb);

  if(r->order < 2) return;

  r->d2edrs2 = r->d2edrsz = r->d2edz2  = 0.0;
  for(is=0; is<p->nspin; is++){
    FLOAT ft, aux = 1.0 + spin_sign[is]*r->zeta;

    if(aux == 0.0) continue;

    R  = M_PI*bb*aux/(2.0*r->rs[1]);
    ft = FT_inter(R, interaction);
 
    r->d2edrs2 -= aux*aux*ft;
    r->d2edrsz += spin_sign[is]*aux*ft;
    r->d2edz2  -= ft;
  }
  r->d2edrs2 *= spin_fact[p->nspin-1]/(8.0*r->rs[2]*r->rs[1]);
  r->d2edrsz *= spin_fact[p->nspin-1]/(8.0*r->rs[2]);
  r->d2edz2  *= spin_fact[p->nspin-1]/(8.0*r->rs[1]);

  if(r->order < 3) return;

  /* TODO : third derivatives */
}

#define XC_DIMENSIONS 1
#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_x_1d) = {
  XC_LDA_X_1D,
  XC_EXCHANGE,
  "Exchange in 1D",
  XC_FAMILY_LDA,
  "N. Helbig, J. I. Fuks, M. Casula, M. J. Verstraete, M. A. L. Marques, I. V. Tokatly and A. Rubio, Phys. Rev. A 83, 032503 (2011)",
  XC_FLAGS_1D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-32, 0.0, 0.0, 1e-32,
  lda_x_1d_init,    /* init */
  NULL,             /* end  */
  work_lda,         /* lda  */
};
