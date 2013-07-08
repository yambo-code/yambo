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

static void
work_mgga_c_init(void *p_)
{
  XC(mgga_type) *p = (XC(mgga_type) *)p_;

  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(sizeof(XC(func_type) *)*p->n_func_aux);
  p->func_aux[0] = (XC(func_type) *)  malloc(sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_LDA_C_PW, XC_POLARIZED);
}


static void 
work_mgga_c(const void *p_, int np, const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl, const FLOAT *tau,
	    FLOAT *zk, FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau,
	    FLOAT *v2rho2, FLOAT *v2sigma2, FLOAT *v2lapl2, FLOAT *v2tau2,
	    FLOAT *v2rhosigma, FLOAT *v2rholapl, FLOAT *v2rhotau, 
	    FLOAT *v2sigmalapl, FLOAT *v2sigmatau, FLOAT *v2lapltau)
{
  const XC(mgga_type) *p = (const XC(mgga_type) *) p_;

  FLOAT sfact, sfact2, dens;
  FLOAT ds[2], sigmas[2], x[2], t[2], u[2], f_LDA[2], vrho_LDA[2];
  int ip, is, order;

  order = -1;
  if(zk     != NULL) order = 0;
  if(vrho   != NULL) order = 1;
  if(v2rho2 != NULL) order = 2;
  if(order < 0) return;

  sfact = (p->nspin == XC_POLARIZED) ? 1.0 : 2.0;
  sfact2 = sfact*sfact;

  for(ip = 0; ip < np; ip++){
    dens = (p->nspin == XC_UNPOLARIZED) ? rho[0] : rho[0] + rho[1];
    if(dens < p->info->min_dens) goto end_ip_loop;

    if(p->nspin == XC_UNPOLARIZED)
      ds[1] = rho[0]/2.0;

    for(is=0; is<p->nspin; is++){
      FLOAT gdm, rho13;
      FLOAT f, ltau, lnr2, dfdx, dfdt, dfdu, d2fdx2, d2fdxt, d2fdt2;
      int js = (is == 0) ? 0 : 2;

      ds[is] = rho[is]/sfact;

      if(rho[is] < p->info->min_dens) continue;

      sigmas[is] = max(p->info->min_grad*p->info->min_grad, sigma[js]/sfact2);
      gdm        = SQRT(sigmas[is]);
  
      rho13  = CBRT(ds[is]);
      x [is] = gdm/(ds[is]*rho13);
    
      ltau   = max(tau[is]/sfact, p->info->min_tau);
      t [is] = ltau/(ds[is]*rho13*rho13);  /* tau/rho^(5/3) */

      lnr2   = max(p->info->min_tau, lapl[is]/sfact);
      u [is] = lnr2/(ds[is]*rho13*rho13);  /* lapl/rho^(5/3) */

      dfdx  = d2fdx2 = 0.0;
      dfdt = dfdu = 0.0;

      func_c_parallel(p, x[is], t[is], u[is], order, 
		      &f, &dfdx, &dfdt, &dfdu, &d2fdx2, &d2fdxt, &d2fdt2);

      { /* get parallel spin LDA energy */
	FLOAT tmp_rho[2], tmp_vrho[2];

	tmp_rho[0] = ds[is];
	tmp_rho[1] = 0.0;

	switch (order){
	case 0:
	  XC(lda_exc)(p->func_aux[0], 1, tmp_rho, &(f_LDA[is]));
	  break;
	case 1:
	  XC(lda_exc_vxc)(p->func_aux[0], 1, tmp_rho, &(f_LDA[is]), tmp_vrho);
	  vrho_LDA[is] = tmp_vrho[0];
	  break;
	case 2: /* to be implemented */
	  break; 
	}
      }

      if(zk != NULL)
	*zk += sfact*ds[is]*f_LDA[is]*f;
 
      if(vrho != NULL){
	vrho[is]   = vrho_LDA[is]*f - f_LDA[is]*
	  (4.0*dfdx*x[is] + 5.0*(dfdt*t[is] + dfdu*u[is]))/3.0;
	vtau[is]   = f_LDA[is]*dfdt/(rho13*rho13);
	vlapl[is]  = f_LDA[is]*dfdu/(rho13*rho13);
	
	vsigma[js] = ds[is]*f_LDA[is]*dfdx*x[is]/(2.0*sfact*sigmas[is]);
      }

      if(v2rho2 != NULL){
	/* Missing terms here */
	exit(1);
      }
    }
    /* *zk /= dens; return; */  /* DEBUG */

    /* We are now missing the opposite-spin part */
    {
      FLOAT f_LDA_opp, vrho_LDA_opp[2];
      FLOAT f, dfdx, dfdt, dfdu, d2fdx2, d2fdxt, d2fdt2;
      FLOAT xt, tt, uu;

      switch (order){
      case 0:
	XC(lda_exc)(p->func_aux[0], 1, ds, &f_LDA_opp);
	break;
      case 1:
	XC(lda_exc_vxc)(p->func_aux[0], 1, ds, &f_LDA_opp, vrho_LDA_opp);
	break;
      case 2: /* to be implemented */
	break; 
      }
      
      if(p->nspin == XC_POLARIZED){
	xt = tt = uu = 0.0;
	for(is=0; is<p->nspin; is++)
	  if(rho[is] > p->info->min_dens){
	    xt += x[is]*x[is];
	    tt += t[is];
	    uu += u[is];
	  }
	xt = SQRT(xt);
      }else{
	xt = M_SQRT2*x[0];
	tt =      2.0 *t[0];
	uu =      2.0 *u[0];
      }

      dfdt = dfdu = 0.0;
      func_c_opposite(p, xt, tt, uu, order, &f, &dfdx, &dfdt, &dfdu, &d2fdx2, &d2fdxt, &d2fdt2);

      if(zk != NULL)
	*zk += dens*f_LDA_opp*f;
 
      if(vrho != NULL){
	for(is=0; is<p->nspin; is++){
	  int js = (is == 0) ? 0 : 2;
	  
	  if(rho[is] < p->info->min_dens) continue;
	  
	  vrho[is]   += vrho_LDA_opp[is]*f - dens*f_LDA_opp*
	    (4.0*dfdx*x[is]*x[is]/xt + 5.0*(dfdt*t[is] + dfdu*u[is]))/(3.0*ds[is]);
	  vtau[is]   += f_LDA_opp*dfdt*dens/POW(ds[is], 5.0/3.0);
	  vlapl[is]  += f_LDA_opp*dfdu*dens/POW(ds[is], 5.0/3.0);
	  vsigma[js] += dens*f_LDA_opp*dfdx*x[is]*x[is]/(2.0*xt*sfact*sigmas[is]);
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
