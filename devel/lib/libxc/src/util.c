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


/* this function converts the spin-density into total density and
	 relative magnetization */
/* inline */ void
XC(rho2dzeta)(int nspin, const FLOAT *rho, FLOAT *d, FLOAT *zeta)
{
  if(nspin==XC_UNPOLARIZED){
    *d    = max(rho[0], 0.0);
    *zeta = 0.0;
  }else{
    *d = rho[0] + rho[1];
    if(*d > 0.0){
      *zeta = (rho[0] - rho[1])/(*d);
      *zeta = min(*zeta,  1.0);
      *zeta = max(*zeta, -1.0);
    }else{
      *d    = 0.0;
      *zeta = 0.0;
    }
  }
}

/* inline */ void
XC(fast_fzeta)(const FLOAT x, const int nspin, const int order, FLOAT * fz){

  FLOAT aa, bb, aa2, bb2;

  if(nspin != XC_UNPOLARIZED){
    aa = CBRT(1.0 + x);
    bb = CBRT(1.0 - x);
    
    aa2 = aa*aa;
    bb2 = bb*bb;
    
    fz[0] = (aa2*aa2 + bb2*bb2 - 2.0)/FZETAFACTOR;
    if(order < 1) return;
    fz[1] = (aa - bb)*(4.0/3.0)/FZETAFACTOR;
    if(order < 2) return;
    fz[2] = ((4.0/9.0)/FZETAFACTOR)*(ABS(x)==1.0 ? (FLT_MAX) : (pow(1.0 + (x), -2.0/3.0) + pow(1.0 - (x), -2.0/3.0)));
    if(order < 3) return;
    fz[3] = (-(8.0/27.0)/FZETAFACTOR)*(ABS(x)==1.0 ? (FLT_MAX) : (pow(1.0 + (x), -5.0/3.0) - pow(1.0 - (x), -5.0/3.0)));
  } else {
    fz[0] = 0.0;
    fz[1] = 0.0;
    fz[2] = (8.0/9.0)/FZETAFACTOR;
    fz[3] = 0.0;
  }
}

/* initializes the mixing */
void 
XC(mix_init)(XC(func_type) *p, int n_funcs, const int *funcs_id, const FLOAT *mix_coef)
{
  int ii;

  assert(p != NULL);
  assert(p->func_aux == NULL && p->mix_coef == NULL);

  /* allocate structures needed for */
  p->n_func_aux = n_funcs;
  p->mix_coef   = (FLOAT *) malloc(n_funcs*sizeof(FLOAT));
  p->func_aux   = (XC(func_type) **) malloc(n_funcs*sizeof(XC(func_type) *));

  for(ii=0; ii<n_funcs; ii++){
    p->mix_coef[ii] = mix_coef[ii];
    p->func_aux[ii] = (XC(func_type) *) malloc(sizeof(XC(func_type)));
    XC(func_init) (p->func_aux[ii], funcs_id[ii], p->nspin);
  }
  
}

