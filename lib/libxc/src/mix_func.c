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

#define is_mgga(id)   ((id) == XC_FAMILY_MGGA || (id) == XC_FAMILY_HYB_MGGA)
#define is_gga(id)    ((id) == XC_FAMILY_GGA  || (id) == XC_FAMILY_HYB_GGA || is_mgga(id))
#define is_lda(id)    ((id) == XC_FAMILY_LDA  || is_gga(is))
#define safe_free(pt) if(pt != NULL) free(pt)

void XC(mix_func)
  (const XC(func_type) *func, int np,
   const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl, const FLOAT *tau,
   FLOAT *zk, FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau,
   FLOAT *v2rho2, FLOAT *v2sigma2, FLOAT *v2lapl2, FLOAT *v2tau2,
   FLOAT *v2rhosigma, FLOAT *v2rholapl, FLOAT *v2rhotau, 
   FLOAT *v2sigmalapl, FLOAT *v2sigmatau, FLOAT *v2lapltau)
{
  const XC(func_type) *aux;
  FLOAT *zk_, *vrho_, *vsigma_, *vlapl_, *vtau_;
  FLOAT *v2rho2_, *v2sigma2_, *v2lapl2_, *v2tau2_;
  FLOAT *v2rhosigma_, *v2rholapl_, *v2rhotau_;
  FLOAT *v2sigmalapl_, *v2sigmatau_, *v2lapltau_;

  int ip, ii;

  /* prepare buffers that will hold the results from the individual functionals */
  zk_ = NULL;
  vrho_ = vsigma_ = vlapl_ = vtau_ = NULL;
  v2rho2_ = v2sigma2_ = v2lapl2_ = v2tau2_ = NULL;
  v2rhosigma_ = v2rholapl_ = v2rhotau_ = NULL;
  v2sigmalapl_ = v2sigmatau_ = v2lapltau_ = NULL;

  if(zk != NULL)
    zk_ = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_zk);

  if(vrho != NULL){
    vrho_ = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_vrho);
    if(is_gga(func->info->family)){
      vsigma_ = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_vsigma);
    }
    if(is_mgga(func->info->family)){
      vlapl_ = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_vlapl);
      vtau_  = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_vtau);
    }
  }

  if(v2rho2 != NULL){
    v2rho2_ = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_v2rho2);
    if(is_gga(func->info->family)){
      v2sigma2_    = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_v2sigma2);
      v2rhosigma_  = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_v2rhosigma);
    }
    if(is_mgga(func->info->family)){
      v2lapl2_     = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_v2lapl2);
      v2tau2_      = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_v2tau2);
      v2rholapl_   = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_v2rholapl);
      v2rhotau_    = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_v2rhotau);
      v2sigmalapl_ = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_v2sigmalapl);
      v2sigmatau_  = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_v2sigmatau);
      v2lapltau_   = (FLOAT *) malloc(sizeof(FLOAT)*np*func->n_v2lapltau);
    }
  }

  /* we now add the different components */
  for(ii=0; ii<func->n_func_aux; ii++){
    aux = func->func_aux[ii];
    switch(aux->info->family){
    case XC_FAMILY_LDA:
      XC(lda)(aux, np, rho, zk_, vrho_, v2rho2_, NULL);
      break;
    case XC_FAMILY_GGA:
      XC(gga)(aux, np, rho, sigma, zk_, vrho_, vsigma_, v2rho2_, v2rhosigma_, v2sigma2_);
      break;
    case XC_FAMILY_MGGA:
      XC(mgga)(aux, np, rho, sigma, lapl, tau, zk_, vrho_, vsigma_, vlapl_, vtau_,
	       v2rho2_, v2sigma2_, v2lapl2_, v2tau2_, v2rhosigma_, v2rholapl_, v2rhotau_, 
	       v2sigmalapl_, v2sigmatau_, v2lapltau_);
      break;
    }

    if(zk != NULL)
      for(ip = 0; ip < np*func->n_zk; ip++)
	zk[ip] += func->mix_coef[ii] * zk_[ip];

    if(vrho != NULL){
      for(ip = 0; ip < np*func->n_vrho; ip++)
	vrho[ip] += func->mix_coef[ii] * vrho_[ip];

      if(is_gga(func->info->family) && is_gga(aux->info->family))
	for(ip = 0; ip < np*func->n_vsigma; ip++)
	  vsigma[ip] += func->mix_coef[ii] * vsigma_[ip];

      if(is_mgga(func->info->family) && is_mgga(aux->info->family)){
	for(ip = 0; ip < np*func->n_vlapl; ip++)
	  vlapl[ip] += func->mix_coef[ii] * vlapl_[ip];
	for(ip = 0; ip < np*func->n_vtau; ip++)
	  vtau[ip] += func->mix_coef[ii] * vtau_[ip];
      }
    }

    if(v2rho2 != NULL){
      for(ip = 0; ip < np*func->n_v2rho2; ip++)
	v2rho2[ip] += func->mix_coef[ii] * v2rho2_[ip];

      if(is_gga(func->info->family) && is_gga(aux->info->family)){
	for(ip = 0; ip < np*func->n_v2rhosigma; ip++)
	  v2rhosigma[ip] += func->mix_coef[ii] * v2rhosigma_[ip];
	for(ip = 0; ip < np*func->n_v2sigma2; ip++)
	  v2sigma2[ip] += func->mix_coef[ii] * v2sigma2_[ip];
      }

      if(is_mgga(func->info->family) && is_mgga(aux->info->family)){
	for(ip = 0; ip < np*func->n_v2lapl2; ip++)
	  v2lapl2[ip]     += func->mix_coef[ii] * v2lapl2_[ip];
	for(ip = 0; ip < np*func->n_v2tau2; ip++)
	  v2tau2[ip]      += func->mix_coef[ii] * v2tau2_[ip];
	for(ip = 0; ip < np*func->n_v2rholapl; ip++)
	  v2rholapl[ip]   += func->mix_coef[ii] * v2rholapl_[ip];
	for(ip = 0; ip < np*func->n_v2rhotau; ip++)
	  v2rhotau[ip]    += func->mix_coef[ii] * v2rhotau_[ip];
	for(ip = 0; ip < np*func->n_v2sigmalapl; ip++)
	  v2sigmalapl[ip] += func->mix_coef[ii] * v2sigmalapl_[ip];
	for(ip = 0; ip < np*func->n_v2sigmatau; ip++)
	  v2sigmatau[ip]  += func->mix_coef[ii] * v2sigmatau_[ip];
	for(ip = 0; ip < np*func->n_v2lapltau; ip++)
	  v2lapltau[ip]   += func->mix_coef[ii] * v2lapltau_[ip];
      }
    }
  }

  /* deallocate internal buffers */
  safe_free(zk_);
  safe_free(vrho_); safe_free(vsigma_); safe_free(vlapl_); safe_free(vtau_);
  safe_free(v2rho2_); safe_free(v2sigma2_); safe_free(v2lapl2_); safe_free(v2tau2_);
  safe_free(v2rhosigma_); safe_free(v2rholapl_); safe_free(v2rhotau_);
  safe_free(v2sigmalapl_); safe_free(v2sigmatau_); safe_free(v2lapltau_);
}
