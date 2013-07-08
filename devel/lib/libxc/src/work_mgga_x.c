/*
 Copyright (C) 2006-2008 M.A.L. Marques

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

/************************************************************************
  This file is to be included in meta GGA exchange functionals. As often these
  functionals are written as a function of s = |grad n|/n^(4/3) and tau, this
  routine performs the necessary conversions between a functional of s and tau
  and of rho.
************************************************************************/

#include <stdio.h>
#include <string.h>

#ifndef XC_DIMENSIONS
#  define XC_DIMENSIONS 3
#endif

static void 
work_mgga_x(const void *p_, int np,
	    const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl, const FLOAT *tau,
	    FLOAT *zk, FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau,
	    FLOAT *v2rho2, FLOAT *v2sigma2, FLOAT *v2lapl2, FLOAT *v2tau2,
	    FLOAT *v2rhosigma, FLOAT *v2rholapl, FLOAT *v2rhotau, 
	    FLOAT *v2sigmalapl, FLOAT *v2sigmatau, FLOAT *v2lapltau)
{
  const XC(mgga_type) *p = (const XC(mgga_type) *) p_;

  XC(work_mgga_x_params) r;
  FLOAT sfact, sfact2, dens, x_factor_c;
  int is, ip;
  int has_tail;

  /* WARNING: derivatives are _not_ OK for 2 dimensions */
  #if XC_DIMENSIONS == 2
  const FLOAT cnst_rs = 0.56418958354775627928; /* = 1.0/sqrt(M_PI) */
  x_factor_c = X_FACTOR_2D_C;
  #else /* three dimensions */
  const FLOAT cnst_rs = 0.6203504908994000866;  /* = POW(3.0/(4*M_PI), 1.0/3.0)*/
  x_factor_c = X_FACTOR_C;
  #endif

  /* initialize everything to zero */
  memset(&r, 0, sizeof(r));

  r.order = -1;
  if(zk     != NULL) r.order = 0;
  if(vrho   != NULL) r.order = 1;
  if(v2rho2 != NULL) r.order = 2;
  if(r.order < 0) return;

  sfact = (p->nspin == XC_POLARIZED) ? 1.0 : 2.0;
  sfact2 = sfact*sfact;

  has_tail = 0;
  switch(p->info->number){
  case XC_MGGA_X_BR89:
  case XC_MGGA_X_BJ06:
  case XC_MGGA_X_TB09:
  case XC_MGGA_X_RPP09:
    has_tail = 1;
    break;
  }
  
  for(ip = 0; ip < np; ip++){
    XC(rho2dzeta)(p->nspin, rho, &dens, &r.zeta);

    if(dens < p->info->min_dens) goto end_ip_loop;

    r.rs = cnst_rs*POW(dens, -1.0/XC_DIMENSIONS);

    for(is=0; is<p->nspin; is++){
      FLOAT lrho, rho1D, rho2pD_D, lsigma, gdm, lnr2, ltau;
      int js = (is == 0) ? 0 : 2;
      int ks = (is == 0) ? 0 : 5;

      if((!has_tail && (rho[is] < p->info->min_dens || tau[is] < p->info->min_tau)) || (rho[is] == 0.0)) continue;

      lsigma= max(sigma[js]/sfact2, p->info->min_grad*p->info->min_grad);
      gdm   = SQRT(lsigma);
      lrho  = rho[is]/sfact;
      rho1D = POW(lrho, 1.0/XC_DIMENSIONS);
      rho2pD_D = lrho*rho1D*rho1D;
      r.x   = gdm/(lrho*rho1D);
    
      ltau  = tau[is]/sfact;
      r.t   = ltau/rho2pD_D;  /* tau/rho^((2+D)/D) */

      lnr2  = lapl[is]/sfact; /* this can be negative */
      r.u   = lnr2/rho2pD_D;  /* lapl/rho^((2+D)/D) */

      func(p, &r);

      if(zk != NULL && (p->info->flags & XC_FLAGS_HAVE_EXC))
	*zk += -sfact*x_factor_c*(lrho*rho1D)*r.f;

      if(vrho != NULL && (p->info->flags & XC_FLAGS_HAVE_VXC)){
	vrho[is]  = -x_factor_c*rho1D*(-r.rs*r.dfdrs + 4.0/3.0*(r.f - r.dfdx*r.x) - 5.0/3.0*(r.dfdt*r.t + r.dfdu*r.u));

	vtau[is]  = -x_factor_c*r.dfdt/rho1D;

	vlapl[is] = -x_factor_c*r.dfdu/rho1D;

	if(gdm>p->info->min_grad)
	  vsigma[js] = -x_factor_c*(rho1D*lrho)*r.dfdx*r.x/(2.0*sfact*lsigma);
      }

      /* WARNING: terms with rs not implemented yet */
      if(v2rho2 != NULL && (p->info->flags & XC_FLAGS_HAVE_FXC)){
	v2rho2[js]    = -x_factor_c/(9.0*sfact*rho1D*rho1D)*
	  (4.0*r.f - 4.0*r.x*r.dfdx + 4.0*4.0*r.x*r.x*r.d2fdx2 + 5.0*5.0*r.t*r.t*r.d2fdt2 + 5.0*5.0*r.u*r.u*r.d2fdu2 +
	   2.0*5.0*(4.0*r.x*r.t*r.d2fdxt + 4.0*r.x*r.u*r.d2fdxu + 5.0*r.t*r.u*r.d2fdtu));

	v2lapl2[js]   = -x_factor_c*r.d2fdu2/(sfact*rho1D*rho2pD_D);

	v2tau2[js]    = -x_factor_c*r.d2fdt2/(sfact*rho1D*rho2pD_D);

	v2rholapl[js] = -x_factor_c*rho1D/(3.0*sfact*rho2pD_D)*
	  (4.0*r.dfdu - 4.0*r.x*r.d2fdxu - 5.0*r.u*r.d2fdtu - 5.0*(r.dfdu + r.u*r.d2fdu2));

	v2rhotau[js]  = -x_factor_c*rho1D/(3.0*sfact*rho2pD_D)*
	  (4.0*r.dfdt - 4.0*r.x*r.d2fdxt - 5.0*r.u*r.d2fdtu - 5.0*(r.dfdt + r.t*r.d2fdt2));

	v2lapltau[js] = -x_factor_c*r.d2fdtu/(rho1D*rho2pD_D);

	if(gdm>p->info->min_grad){
	  v2sigma2[ks]    =  -x_factor_c*(rho1D*lrho)/(4.0*sfact2*sfact*lsigma*lsigma)*
	    (r.d2fdx2*r.x*r.x - r.dfdx*r.x);

	  v2rhosigma[ks]  = -x_factor_c*rho1D*r.x/(3.0*2.0*sfact2*lsigma)*
	    (-4.0*r.x*r.d2fdx2 - 5.0*r.t*r.d2fdxt - 5.0*r.u*r.d2fdxu);

	  v2sigmalapl[ks] = -x_factor_c*r.x/(2.0*sfact2*lsigma*rho1D)*r.d2fdxu;

	  v2sigmatau[ks]  = -x_factor_c*r.x/(2.0*sfact2*lsigma*rho1D)*r.d2fdxt;
	}
      }
    }
    
    if(zk != NULL)
      *zk /= dens; /* we want energy per particle */

  end_ip_loop:
    /* increment pointers */
    rho   += p->n_rho;
    sigma += p->n_sigma;
    tau   += p->n_tau;
    lapl  += p->n_lapl;
    
    if(zk != NULL)
      zk += p->n_zk;
    
    if(vrho != NULL){
      vrho   += p->n_vrho;
      vsigma += p->n_vsigma;
      vtau   += p->n_vtau;
      vlapl  += p->n_vlapl;
    }

    if(v2rho2 != NULL){
      v2rho2      += p->n_v2rho2;
      v2sigma2    += p->n_v2sigma2;
      v2tau2      += p->n_v2tau2;
      v2lapl2     += p->n_v2lapl2;
      v2rhosigma  += p->n_v2rhosigma;
      v2rhotau    += p->n_v2rhotau;
      v2rholapl   += p->n_v2rholapl;
      v2sigmatau  += p->n_v2sigmatau;
      v2sigmalapl += p->n_v2sigmalapl;
      v2lapltau   += p->n_v2lapltau;
    }
  }
}
