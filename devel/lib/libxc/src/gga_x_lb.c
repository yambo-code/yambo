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

/* Note: Do not forget to add a correlation (LDA) functional to the
   LB94.
   
   Note 2: The 160 value is hardcoded in xc.h and libxc_master.F90 to
   define XC_GGA_XC_LB to keep backwards compatibility.

*/
#define XC_GGA_X_LB  160 /* van Leeuwen & Baerends */
#define XC_GGA_X_LBM 182 /* van Leeuwen & Baerends modified*/

typedef struct{
  int    modified; /* shall we use a modified version */
  FLOAT threshold; /* when to start using the analytic form */
  FLOAT ip;        /* ionization potential of the species */
  FLOAT qtot;      /* total charge in the region */

  FLOAT aa;     /* the parameters of LB94 */
  FLOAT gamm;

  FLOAT alpha;
  FLOAT beta;
} XC(gga_x_lb_params);

/************************************************************************
  Calculates van Leeuwen Baerends functional
************************************************************************/

static void
gga_lb_init(void *p_)
{
  XC(gga_type) *p = (XC(gga_type) *)p_;
  XC(gga_x_lb_params) *params;

  assert(p->params == NULL);

  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_LDA_X, p->nspin);

  p->params = malloc(sizeof(XC(gga_x_lb_params)));
  XC(gga_lb_set_params_)(p, 0, 0.0, 0.0, 0.0);

  params = (XC(gga_x_lb_params) *) (p->params);
  switch(p->info->number){
  case XC_GGA_X_LB:
    params->alpha = 1.0;
    params->beta  = 0.05;
    break;
  case XC_GGA_X_LBM:
    params->alpha = 1.19;
    params->beta  = 0.01;
    break;
  }
}


void
XC(gga_lb_set_params)(XC(func_type) *p, int modified, FLOAT threshold, FLOAT ip, FLOAT qtot)
{
  assert(p!=NULL && p->gga!=NULL);
  XC(gga_lb_set_params_)(p->gga, modified, threshold, ip, qtot);
}

void
XC(gga_lb_set_params_)(XC(gga_type) *p, int modified, FLOAT threshold, FLOAT ip, FLOAT qtot)
{
  XC(gga_x_lb_params) *params;

  assert(p->params != NULL);
  params = (XC(gga_x_lb_params) *) (p->params);

  params->modified  = modified;
  params->threshold = threshold;
  params->ip        = ip;
  params->qtot      = qtot;

  if(params->modified){
    params->aa   = (params->ip > 0.0) ? 2.0*SQRT(2.0*params->ip) : 0.5;
    params->gamm = CBRT(params->qtot)/(2.0*params->aa);
  }else{
    params->aa   = 0.5;
    params->gamm = 1.0;
  }
}


void 
XC(gga_lb_modified)(const XC(gga_type) *func, int np, const FLOAT *rho, const FLOAT *sigma, FLOAT r, FLOAT *vrho)
{
  int ip, is;
  FLOAT gdm, x;

  XC(gga_x_lb_params) *params;

  assert(func != NULL);

  assert(func->params != NULL);
  params = (XC(gga_x_lb_params) *) (func->params);

  XC(lda_vxc)(func->func_aux[0], np, rho, vrho);

  for(ip=0; ip<np; ip++){
    for(is=0; is<func->nspin; is++){
      vrho[is] *= params->alpha;

      gdm = SQRT(sigma[(is==0) ? 0 : 2]);

      if(params->modified == 0 || 
	 (rho[is] > params->threshold && gdm > params->threshold)){
	FLOAT f;
      
	if(rho[is] <= func->info->min_dens) continue;
	
	x =  gdm/POW(rho[is], 4.0/3.0);
	
	if(x < 300.0) /* the actual functional */	   
	  f = -params->beta*x*x/(1.0 + 3.0*params->beta*x*asinh(params->gamm*x));
	else          /* asymptotic expansion */
	  f = -x/(3.0*log(2.0*params->gamm*x));

	vrho[is] += f * CBRT(rho[is]);
	
      }else if(r > 0.0){
	/* the aymptotic expansion of LB94 */
	x = r + (3.0/params->aa)*
	  log(2.0*params->gamm * params->aa * 1.0 / CBRT(params->qtot));
	
	/* x = x + POW(qtot*exp(-aa*r), 1.0/3.0)/(beta*aa*aa); */
	
	vrho[is] -= 1.0/x;
      }
    }
    /* increment pointers */
    rho   += func->n_rho;
    sigma += func->n_sigma;
    
    if(vrho != NULL)
      vrho   += func->n_vrho;

  } /* ip loop */
}


static void 
gga_x_lb(const void *p_, int np, const FLOAT *rho, const FLOAT *sigma,
	  FLOAT *zk, FLOAT *vrho, FLOAT *vsigma,
	  FLOAT *v2rho2, FLOAT *v2rhosigma, FLOAT *v2sigma2)
{
  XC(gga_lb_modified)((XC(gga_type) *)p_, np, rho, sigma, 0.0, vrho);
}


XC(func_info_type) XC(func_info_gga_x_lb) = {
  XC_GGA_X_LB,
  XC_EXCHANGE_CORRELATION,
  "van Leeuwen & Baerends",
  XC_FAMILY_GGA,
  "R van Leeuwen and EJ Baerends, Phys. Rev. A. 49, 2421 (1994)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_lb_init,
  NULL,
  NULL,
  gga_x_lb
};

XC(func_info_type) XC(func_info_gga_x_lbm) = {
  XC_GGA_X_LBM,
  XC_EXCHANGE_CORRELATION,
  "van Leeuwen & Baerends modified",
  XC_FAMILY_GGA,
  "PRT Schipper, OV Gritsenko, SJA van Gisbergen, and EJ Baerends, J. Chem. Phys. 112, 1344 (2000)\n"
  "R van Leeuwen and EJ Baerends, Phys. Rev. A. 49, 2421 (1994)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_lb_init,
  NULL,
  NULL,
  gga_x_lb
};

