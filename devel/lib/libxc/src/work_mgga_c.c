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

static void 
work_mgga_c(const XC(func_type) *p, int np, const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl, const FLOAT *tau,
	    FLOAT *zk, FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau,
	    FLOAT *v2rho2, FLOAT *v2sigma2, FLOAT *v2lapl2, FLOAT *v2tau2,
	    FLOAT *v2rhosigma, FLOAT *v2rholapl, FLOAT *v2rhotau, 
	    FLOAT *v2sigmalapl, FLOAT *v2sigmatau, FLOAT *v2lapltau)
{
  XC(mgga_work_c_t) r;
  FLOAT min_grad2 = p->info->min_grad*p->info->min_grad;
  int ip;

  r.order = -1;
  if(zk     != NULL) r.order = 0;
  if(vrho   != NULL) r.order = 1;
  if(v2rho2 != NULL) r.order = 2;

  if(r.order < 0) return;

  for(ip = 0; ip < np; ip++){
    FLOAT rho13[3], drs, dxt;
    FLOAT ndzdn[2], dxsdn[2];
    FLOAT dxtds, dxsds[2];
    FLOAT dusdn[2], dusdlapl[2], dtsdn[2], dtsdtau[2];

    XC(rho2dzeta)(p->nspin, rho, &(r.dens), &(r.zeta));

    if(r.dens < p->info->min_dens) goto end_ip_loop;

    r.rs = RS(r.dens);
    rho13[2] = CBRT(r.dens);

    if(p->nspin == XC_UNPOLARIZED){
      r.ds[0]  = r.dens/2.0;
      r.ds[1]  = r.ds[0];

      rho13[0] = rho13[2]/M_CBRT2;
      rho13[1] = rho13[0];

      /* we already know that dens > min_dens */
      r.sigmat = max(min_grad2, sigma[0]);
      r.xt     = SQRT(r.sigmat)/(r.dens*rho13[2]);

      r.sigmas[0] = r.sigmat/4.0;
      r.sigmas[1] = r.sigmas[0];
      r.sigmas[2] = r.sigmas[0];

      r.xs[0]  = M_CBRT2*r.xt;
      r.xs[1]  = r.xs[0];

      r.us[0]  = lapl[0]/(2.0*r.ds[0]*rho13[0]*rho13[0]); /* lapl/rho^(5/3) */
      r.us[1]  = r.us[0];

      r.ts[0]  = tau[0]/(2.0*r.ds[0]*rho13[0]*rho13[0]);  /* tau/rho^(5/3) */
      r.ts[1]  = r.ts[0];
    }else{
      r.ds[0]  = max(p->info->min_dens, rho[0]);
      r.ds[1]  = max(p->info->min_dens, rho[1]);

      rho13[0] = CBRT(r.ds[0]);
      rho13[1] = CBRT(r.ds[1]);
      
      r.sigmat = max(min_grad2, sigma[0] + 2.0*sigma[1] + sigma[2]);
      r.xt     = SQRT(r.sigmat)/(r.dens*rho13[2]);
      
      r.sigmas[0] = max(min_grad2, sigma[0]);
      r.sigmas[1] = max(min_grad2, sigma[1]);
      r.sigmas[2] = max(min_grad2, sigma[2]);

      r.xs[0] = SQRT(r.sigmas[0])/(r.ds[0]*rho13[0]);
      r.xs[1] = SQRT(r.sigmas[2])/(r.ds[1]*rho13[1]);

      r.us[0]   = lapl[0]/(r.ds[0]*rho13[0]*rho13[0]);
      r.us[1]   = lapl[1]/(r.ds[1]*rho13[1]*rho13[1]);

      r.ts[0]   = tau[0]/(r.ds[0]*rho13[0]*rho13[0]);
      r.ts[1]   = tau[1]/(r.ds[1]*rho13[1]*rho13[1]);
    }
  
    func(p, &r);

    if(zk != NULL && (p->info->flags & XC_FLAGS_HAVE_EXC))
      *zk = r.f;

    if(r.order < 1) goto end_ip_loop;
    
    /* setup auxiliary variables */
    drs   =     -r.rs/(3.0*r.dens);
    dxt   = -4.0*r.xt/(3.0*r.dens);
    dxtds = r.xt/(2.0*r.sigmat);

    if(p->nspin == XC_POLARIZED){
      ndzdn[1]    = -(r.zeta + 1.0);
      ndzdn[0]    = -(r.zeta - 1.0);

      dxsdn[1]    = -4.0*r.xs[1]/(3.0*r.ds[1]);
      dxsdn[0]    = -4.0*r.xs[0]/(3.0*r.ds[0]);

      dxsds[1]    = r.xs[1]/(2.0*r.sigmas[2]);
      dxsds[0]    = r.xs[0]/(2.0*r.sigmas[0]);

      dusdn[1]    = -5.0*r.us[1]/(3.0*r.ds[1]);
      dusdn[0]    = -5.0*r.us[0]/(3.0*r.ds[0]);

      dusdlapl[1] = 1.0/(r.ds[1]*rho13[1]*rho13[1]);
      dusdlapl[0] = 1.0/(r.ds[0]*rho13[0]*rho13[0]);

      dtsdn[1]    = -5.0*r.ts[1]/(3.0*r.ds[1]);
      dtsdn[0]    = -5.0*r.ts[0]/(3.0*r.ds[0]);

      dtsdtau[1]  = dusdlapl[1];
      dtsdtau[0]  = dusdlapl[0];
    }else{
      dxsdn[0]    = M_CBRT2*dxt;
      dxsds[0]    = M_CBRT2*dxtds;

      dusdn[0]    = -5.0*r.us[0]/(6.0*r.ds[0]);
      dusdlapl[0] = 1.0/(2.0*r.ds[0]*rho13[0]*rho13[0]);
      
      dtsdn[0]    = -5.0*r.ts[0]/(6.0*r.ds[0]);
      dtsdtau[0]  = dusdlapl[0];
    }

    if(vrho != NULL && (p->info->flags & XC_FLAGS_HAVE_VXC)){
      vrho[0]   = r.f + r.dens*(r.dfdrs*drs + r.dfdxt*dxt);
      vsigma[0] = r.dens*r.dfdxt*dxtds;

      if(p->nspin == XC_POLARIZED){
	vrho[1]   = vrho[0] + r.dfdz*ndzdn[1] + r.dens*(r.dfdxs[1]*dxsdn[1] + r.dfdus[1]*dusdn[1] + r.dfdts[1]*dtsdn[1]);
	vrho[0]   = vrho[0] + r.dfdz*ndzdn[0] + r.dens*(r.dfdxs[0]*dxsdn[0] + r.dfdus[0]*dusdn[0] + r.dfdts[0]*dtsdn[0]);

	vsigma[2] = vsigma[0] + r.dens*r.dfdxs[1]*dxsds[1];
	vsigma[1] = 2.0*vsigma[0];
	vsigma[0] = vsigma[0] + r.dens*r.dfdxs[0]*dxsds[0];

	vlapl[1]  = r.dens*r.dfdus[1]*dusdlapl[1];
	vlapl[0]  = r.dens*r.dfdus[0]*dusdlapl[0];

	vtau[1]   = r.dens*r.dfdts[1]*dtsdtau[1];
	vtau[0]   = r.dens*r.dfdts[0]*dtsdtau[0];
	
      }else{
	 /* factor of 2 comes from sum over sigma */
	vrho[0]   += 2.0*r.dens*(r.dfdxs[0]*dxsdn[0] + r.dfdus[0]*dusdn[0] + r.dfdts[0]*dtsdn[0]);
	vsigma[0] += 2.0*r.dens*r.dfdxs[0]*dxsds[0];
	vlapl[0]   = 2.0*r.dens*r.dfdus[0]*dusdlapl[0];
	vtau[0]    = 2.0*r.dens*r.dfdts[0]*dtsdtau[0];
      }
    }
    
    if(r.order < 2) goto end_ip_loop;

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
