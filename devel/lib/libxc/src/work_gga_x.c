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

/************************************************************************
  This file is to be included in GGA exchange functionals. As often these
  functionals are written as a function of s = |grad n|/n^(4/3), this
  routine performs the necessary conversions between a functional of s
  and of rho.
************************************************************************/

#ifndef HEADER
#  define HEADER 1
#endif

#ifndef XC_DIMENSIONS
#  define XC_DIMENSIONS 3
#endif

static void
#ifdef XC_KINETIC_FUNCTIONAL
work_gga_k
#else
work_gga_x
#endif
(const void *p_, int np, const FLOAT *rho, const FLOAT *sigma,
 FLOAT *zk, FLOAT *vrho, FLOAT *vsigma,
 FLOAT *v2rho2, FLOAT *v2rhosigma, FLOAT *v2sigma2)
{
  const XC(gga_type) *p = (const XC(gga_type) *) p_;

  FLOAT sfact, x_factor_c, alpha, beta, dens;
  int is, ip, order;
#if HEADER == 2
  FLOAT sfact2;
#endif
  /* alpha is the power of rho in the corresponding LDA
     beta  is the power of rho in the expression for x */

  beta = 1.0 + 1.0/XC_DIMENSIONS; /* exponent of the density in expression for x */

#ifndef XC_KINETIC_FUNCTIONAL
  alpha = beta;

#  if XC_DIMENSIONS == 2
  x_factor_c = -X_FACTOR_2D_C;
#  else /* three dimensions */
  x_factor_c = -X_FACTOR_C;
#  endif

#else

#  if XC_DIMENSIONS == 2
#  else /* three dimensions */
  alpha = 5.0/3.0;
  x_factor_c = 2.0*K_FACTOR_C; /* the 2.0 is due to our definition of tau */
#  endif

#endif

  sfact = (p->nspin == XC_POLARIZED) ? 1.0 : 2.0;
#if HEADER == 2
  sfact2 = sfact*sfact;
#endif

  order = -1;
  if(zk     != NULL) order = 0;
  if(vrho   != NULL) order = 1;
  if(v2rho2 != NULL) order = 2;
  if(order < 0) return;

  for(ip = 0; ip < np; ip++){
    dens = (p->nspin == XC_UNPOLARIZED) ? rho[0] : rho[0] + rho[1];
    if(dens < p->info->min_dens) goto end_ip_loop;

    for(is=0; is<p->nspin; is++){
      FLOAT gdm, ds, rhoLDA;
      FLOAT x, f, dfdx, d2fdx2, lvsigma, lv2sigma2, lvsigmax, lvrho;
      int js = (is == 0) ? 0 : 2;
      int ks = (is == 0) ? 0 : 5;

      if(rho[is] < p->info->min_dens) continue;

      gdm    = max(SQRT(sigma[js])/sfact, p->info->min_grad);
      ds     = rho[is]/sfact;
      rhoLDA = POW(ds, alpha);
      x      = gdm/POW(ds, beta);
      
      dfdx = d2fdx2 = 0.0;
      lvsigma = lv2sigma2 = lvsigmax = lvrho = 0.0;

#if   HEADER == 1
      func(p, order, x, &f, &dfdx, &d2fdx2);
#elif HEADER == 2
      /* this second header is useful for functionals that depend
	 explicitly both on x and on sigma */
      func(p, order, x, gdm*gdm, &f, &dfdx, &lvsigma, &d2fdx2, &lv2sigma2, &lvsigmax);
      
      lvsigma   /= sfact2;
      lvsigmax  /= sfact2;
      lv2sigma2 /= sfact2*sfact2;
#elif HEADER == 3
      /* this second header is useful for functionals that depend
	 explicitly both on x and on rho*/
      func(p, order, x, ds, &f, &dfdx, &lvrho);
#endif

      if(zk != NULL && (p->info->flags & XC_FLAGS_HAVE_EXC))
	*zk += sfact*x_factor_c*rhoLDA*f;
      
      if(vrho != NULL && (p->info->flags & XC_FLAGS_HAVE_VXC)){
	vrho[is] += x_factor_c*(rhoLDA/ds)*(alpha*f - beta*dfdx*x)
	  + x_factor_c*rhoLDA*lvrho;
	
	if(gdm>p->info->min_grad)
	  vsigma[js] = sfact*x_factor_c*rhoLDA*(lvsigma + dfdx*x/(2.0*sigma[js]));
      }
      
      if(v2rho2 != NULL && (p->info->flags & XC_FLAGS_HAVE_FXC)){
	v2rho2[js] = x_factor_c*rhoLDA/(ds*ds) *
	  ((alpha - 1.0)*alpha*f + beta*(beta - 2.0*alpha + 1.0)*x*dfdx + beta*beta*x*x*d2fdx2)/sfact;
	
	if(gdm>p->info->min_grad){
	  v2rhosigma[ks] = x_factor_c*(rhoLDA/ds) *
	    (alpha*lvsigma - beta*x*lvsigmax + ((alpha - beta)*x*dfdx - beta*x*x*d2fdx2)/(2.0*sigma[js]));
	  v2sigma2  [ks] = sfact*x_factor_c*rhoLDA*
	    (lv2sigma2 + lvsigmax*x/sigma[js] + (d2fdx2*x - dfdx)*x/(4.0*sigma[js]*sigma[js]));
	}
	
      }
    }

    if(zk != NULL && (p->info->flags & XC_FLAGS_HAVE_EXC))
      *zk /= dens; /* we want energy per particle */
    
  end_ip_loop:
    /* increment pointers */
    rho   += p->n_rho;
    sigma += p->n_sigma;
    
    if(zk != NULL)
      zk += p->n_zk;
    
    if(vrho != NULL){
      vrho   += p->n_vrho;
      vsigma += p->n_vsigma;
    }

    if(v2rho2 != NULL){
      v2rho2     += p->n_v2rho2;
      v2rhosigma += p->n_v2rhosigma;
      v2sigma2   += p->n_v2sigma2;
    }
  }
}
