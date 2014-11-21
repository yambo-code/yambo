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
#include <string.h>

#include "util.h"
#include "funcs_lda.c"


/* initialization */
int 
XC(lda_init)(XC(func_type) *func, const XC(func_info_type) *info, int nspin)
{
  assert(func != NULL);

  /* initialize structure */
  func->info   = info;
  func->nspin  = nspin;
  func->params = NULL;
  func->func   = 0;

  /* initialize spin counters */
  func->n_rho = func->n_vrho = func->nspin;
  func->n_zk  = 1;
  if(func->nspin == XC_UNPOLARIZED){
    func->n_v2rho2 = func->n_v3rho3 = 1;
  }else{
    func->n_v2rho2 = 3;
    func->n_v3rho3 = 4;
  }

  /* see if we need to initialize the functional */
  if(func->info->init != NULL)
    func->info->init(func);
  return 0;
}


/* termination */
void 
XC(lda_end)(XC(func_type) *func)
{
  assert(func != NULL);

  if(func->info->end != NULL)
    func->info->end(func);

  /* deallocate any used parameter */
  if(func->params != NULL){
    free(func->params);
    func->params = NULL;
  }
}


/* get the lda functional */
void 
XC(lda)(const XC(func_type) *func, int np, const FLOAT *rho, 
	FLOAT *zk, FLOAT *vrho, FLOAT *v2rho2, FLOAT *v3rho3)
{
  assert(func != NULL);

  /* sanity check */
  if(zk != NULL && !(func->info->flags & XC_FLAGS_HAVE_EXC)){
    fprintf(stderr, "Functional '%s' does not provide an implementation of Exc",
	    func->info->name);
    exit(1);
  }

  if(vrho != NULL && !(func->info->flags & XC_FLAGS_HAVE_VXC)){
    fprintf(stderr, "Functional '%s' does not provide an implementation of vxc",
	    func->info->name);
    exit(1);
  }

  if(v2rho2 != NULL && !(func->info->flags & XC_FLAGS_HAVE_FXC)){
    fprintf(stderr, "Functional '%s' does not provide an implementation of fxc",
	    func->info->name);
    exit(1);
  }

  if(v3rho3 != NULL && !(func->info->flags & XC_FLAGS_HAVE_KXC)){
    fprintf(stderr, "Functional '%s' does not provide an implementation of kxc",
	    func->info->name);
    exit(1);
  }

  /* initialize output */
  if(zk != NULL)
    memset(zk,     0, np*sizeof(FLOAT)*func->n_zk);

  if(vrho != NULL)
    memset(vrho,   0, np*sizeof(FLOAT)*func->n_vrho);

  if(v2rho2 != NULL)
    memset(v2rho2, 0, np*sizeof(FLOAT)*func->n_v2rho2);

  if(v3rho3 != NULL)
    memset(v3rho3, 0, np*sizeof(FLOAT)*func->n_v3rho3);


  assert(func->info!=NULL && func->info->lda!=NULL);

  /* call the LDA routines */
  func->info->lda(func, np, rho, zk, vrho, v2rho2, v3rho3);
}


/* specializations */
inline void 
XC(lda_exc)(const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *zk)
{
  XC(lda)(p, np, rho, zk, NULL, NULL, NULL);
}

inline void 
XC(lda_exc_vxc)(const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *zk, FLOAT *vrho)
{
  XC(lda)(p, np, rho, zk, vrho, NULL, NULL);
}

inline void 
XC(lda_vxc)(const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *vrho)
{
  XC(lda)(p, np, rho, NULL, vrho, NULL, NULL);
}

inline void 
XC(lda_fxc)(const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *v2rho2)
{
  XC(lda)(p, np, rho, NULL, NULL, v2rho2, NULL);
}

inline void 
XC(lda_kxc)(const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *v3rho3)
{
  XC(lda)(p, np, rho, NULL, NULL, NULL, v3rho3);
}


#ifdef SINGLE_PRECISION
#  define DELTA_RHO 1e-4
#else
#  define DELTA_RHO 1e-6
#endif

/* get the xc kernel through finite differences */
void 
XC(lda_fxc_fd)(const XC(func_type) *func, int np, const FLOAT *rho, FLOAT *v2rho2)
{
  int i, ip;

  assert(func != NULL);

  for(ip=0; ip<np; ip++){
    for(i=0; i<func->nspin; i++){
      FLOAT rho2[2], vc1[2], vc2[2];
      int j, js;
      
      j  = (i+1) % 2;
      js = (i==0) ? 0 : 2;
      
      rho2[i] = rho[i] + DELTA_RHO;
      rho2[j] = (func->nspin == XC_POLARIZED) ? rho[j] : 0.0;
      XC(lda_vxc)(func, 1, rho2, vc1);
      
      if(rho[i]<2.0*DELTA_RHO){ /* we have to use a forward difference */
	XC(lda_vxc)(func, 1, rho, vc2);
	
	v2rho2[js] = (vc1[i] - vc2[i])/(DELTA_RHO);
	if(func->nspin == XC_POLARIZED && i==0)
	  v2rho2[1] = (vc1[j] - vc2[j])/(DELTA_RHO);
	
      }else{                    /* centered difference (more precise)  */
	rho2[i] = rho[i] - DELTA_RHO;
	XC(lda_vxc)(func, 1, rho2, vc2);
      
	v2rho2[js] = (vc1[i] - vc2[i])/(2.0*DELTA_RHO);
	if(func->nspin == XC_POLARIZED && i==0)
	  v2rho2[1] = (vc1[j] - vc2[j])/(2.0*DELTA_RHO);
      }
    }

    rho    += func->n_rho;
    v2rho2 += func->n_v2rho2;
  } /* for(ip) */
}


void
XC(lda_kxc_fd)(const XC(func_type) *func, int np, const FLOAT *rho, FLOAT *v3rho3)
{
  /* Kxc, this is a third order tensor with respect to the densities */
  int ip, i, j, n;

  assert(func != NULL);

  for(ip=0; ip<np; ip++){
    for(i=0; i<func->nspin; i++){
      FLOAT rho2[2], vc1[2], vc2[2], vc3[2];

      for(n=0; n<func->nspin; n++) rho2[n] = rho[n];
      XC(lda_vxc)(func, 1, rho, vc2);

      rho2[i] += DELTA_RHO;
      XC(lda_vxc)(func, 1, rho2, vc1);
	
      rho2[i] -= 2.0*DELTA_RHO;
      XC(lda_vxc)(func, 1, rho2, vc3);    
    
      for(j=0; j<func->nspin; j++)
	v3rho3[i*func->nspin + j] = (vc1[j] - 2.0*vc2[j] + vc3[j])/(DELTA_RHO*DELTA_RHO);
    }
    
    rho    += func->n_rho;
    v3rho3 += func->n_v3rho3;
  } /* for(ip) */
}
