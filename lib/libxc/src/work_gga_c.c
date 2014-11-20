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
#include <math.h>
#include "util.h"

static void 
work_gga_c(const XC(func_type) *p, int np, const FLOAT *rho, const FLOAT *sigma,
	   FLOAT *zk, FLOAT *vrho, FLOAT *vsigma,
	   FLOAT *v2rho2, FLOAT *v2rhosigma, FLOAT *v2sigma2)
{
  XC(gga_work_c_t) r;
  FLOAT min_grad2 = p->info->min_grad*p->info->min_grad;
  int ip;

  r.order = -1;
  if(zk     != NULL) r.order = 0;
  if(vrho   != NULL) r.order = 1;
  if(v2rho2 != NULL) r.order = 2;

  if(r.order < 0) return;

  for(ip = 0; ip < np; ip++){
    FLOAT drs, dxt;
    FLOAT d2rs, d2xt;
    FLOAT ndzdn[2], dxsdn[2];
    FLOAT dxtds, d2xtds2, d2xtdns, dxsds[2], d2xsdn2[2], d2xsds2[2], d2xsdns[2];

    XC(rho2dzeta)(p->nspin, rho, &(r.dens), &(r.zeta));

    if(r.dens < p->info->min_dens) goto end_ip_loop;

    r.rs = RS(r.dens);
    if(p->nspin == XC_UNPOLARIZED){
      r.ds[0]  = r.dens/2.0;
      r.ds[1]  = r.ds[0];

      r.sigmat = max(min_grad2, sigma[0]);
      r.xt     = SQRT(r.sigmat)/ POW(r.dens, 4.0/3.0);

      r.sigmas[0] = r.sigmat/4.0;
      r.sigmas[1] = r.sigmas[0];
      r.sigmas[2] = r.sigmas[0];

      r.xs[0]  = CBRT(2.0)*r.xt;
      r.xs[1]  = r.xs[0];
    }else{
      r.ds[0]  = max(p->info->min_dens, rho[0]);
      r.ds[1]  = max(p->info->min_dens, rho[1]);
      
      r.sigmat = max(min_grad2, sigma[0] + 2.0*sigma[1] + sigma[2]);
      r.xt     = SQRT(r.sigmat)/ POW(r.dens, 4.0/3.0);
      
      r.sigmas[0] = max(min_grad2, sigma[0]);
      r.sigmas[1] = max(min_grad2, sigma[1]);
      r.sigmas[2] = max(min_grad2, sigma[2]);

      r.xs[0] = SQRT(r.sigmas[0])/POW(r.ds[0], 4.0/3.0);
      r.xs[1] = SQRT(r.sigmas[2])/POW(r.ds[1], 4.0/3.0);
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
      ndzdn[1] = -(r.zeta + 1.0);
      ndzdn[0] = -(r.zeta - 1.0);

      dxsdn[1] = -4.0/3.0*r.xs[1]/r.ds[1];
      dxsdn[0] = -4.0/3.0*r.xs[0]/r.ds[0];

      dxsds[1] = r.xs[1]/(2.0*r.sigmas[2]);
      dxsds[0] = r.xs[0]/(2.0*r.sigmas[0]);
    }else{
      dxsdn[0] = M_CBRT2*dxt;
      dxsds[0] = M_CBRT2*dxtds;
    }

    if(vrho != NULL && (p->info->flags & XC_FLAGS_HAVE_VXC)){
      vrho[0]   = r.f + r.dens*(r.dfdrs*drs + r.dfdxt*dxt);
      vsigma[0] = r.dens*r.dfdxt*dxtds;

      if(p->nspin == XC_POLARIZED){
	vrho[1] = vrho[0] + r.dfdz*ndzdn[1] + r.dens*r.dfdxs[1]*dxsdn[1];
	vrho[0] = vrho[0] + r.dfdz*ndzdn[0] + r.dens*r.dfdxs[0]*dxsdn[0];;

	vsigma[2] = vsigma[0] + r.dens*r.dfdxs[1]*dxsds[1];
	vsigma[1] = 2.0*vsigma[0];
	vsigma[0] = vsigma[0] + r.dens*r.dfdxs[0]*dxsds[0];
	
      }else{
	vrho[0]   += 2.0*r.dens*r.dfdxs[0]*dxsdn[0]; /* factor of 2 comes from sum over sigma */
	vsigma[0] += 2.0*r.dens*r.dfdxs[0]*dxsds[0];
      }
    }

    if(r.order < 2) goto end_ip_loop;
  
    /* setup auxiliary variables */
    d2rs    = -4.0*drs/(3.0*r.dens);
    d2xt    = -7.0*dxt/(3.0*r.dens);
    d2xtds2 = -dxtds/(2.0*r.sigmat);
    d2xtdns =    dxt/(2.0*r.sigmat);
    if(p->nspin == XC_POLARIZED){
      d2xsdn2[0] = -7.0*dxsdn[0]/(3.0*r.ds[0]);
      d2xsdn2[1] = -7.0*dxsdn[1]/(3.0*r.ds[1]);

      d2xsdns[0] = -4.0/3.0*dxsds[0]/r.ds[0];
      d2xsdns[1] = -4.0/3.0*dxsds[1]/r.ds[1];

      d2xsds2[0] = -dxsds[0]/(2.0*r.sigmas[0]);
      d2xsds2[1] = -dxsds[1]/(2.0*r.sigmas[2]);
    }else{
      d2xsdn2[0] = M_CBRT2*d2xt;
      d2xsdns[0] = M_CBRT2*d2xtdns;
      d2xsds2[0] = M_CBRT2*d2xtds2;
    }

    if(v2rho2 != NULL && (p->info->flags & XC_FLAGS_HAVE_FXC)){
      v2rho2[0]     = 2.0*r.dfdrs*drs + 2.0*r.dfdxt*dxt +
	r.dens*(r.d2fdrs2*drs*drs + r.d2fdxt2*dxt*dxt + r.dfdrs*d2rs + r.dfdxt*d2xt + 2.0*r.d2fdrsxt*drs*dxt);

      v2sigma2[0]   = r.dens*(r.d2fdxt2*dxtds*dxtds + r.dfdxt*d2xtds2);

      v2rhosigma[0] = r.dfdxt*dxtds + r.dens*(r.d2fdrsxt*drs*dxtds + r.d2fdxt2*dxt*dxtds + r.dfdxt*d2xtdns);

      if(p->nspin == XC_POLARIZED){
	int is;

        for(is=2; is>=0; is--){
	  int s1 = (is > 1) ?  1 :  0;         /* {0, 0, 1}[is] */
	  int s2 = (is > 0) ?  1 :  0;         /* {0, 1, 1}[is] */

	  v2rho2[is]  = v2rho2[0];

	  v2rho2[is] += r.dfdxs[s1]*dxsdn[s1] + 
	    ndzdn[s1]*(r.d2fdrsz*drs + r.d2fdzxt*dxt + r.d2fdzxs[s2]*dxsdn[s2]) + 
	    r.dens*(r.d2fdrsxs[s1]*drs*dxsdn[s1] + r.d2fdxtxs[s1]*dxt*dxsdn[s1]);

	  v2rho2[is] += r.dfdxs[s2]*dxsdn[s2] + 
	    ndzdn[s2]*(r.d2fdrsz*drs + r.d2fdzxt*dxt + r.d2fdzxs[s1]*dxsdn[s1]) + 
	    r.dens*(r.d2fdrsxs[s2]*drs*dxsdn[s2] + r.d2fdxtxs[s2]*dxt*dxsdn[s2]);

	  v2rho2[is] += r.d2fdz2*ndzdn[s1]*ndzdn[s2]/r.dens + r.dens*r.d2fdxs2[is]*dxsdn[s1]*dxsdn[s2];

	  if(is != 1)
	    v2rho2[is] += r.dens*r.dfdxs[s1]*d2xsdn2[s1];
	}

	/* v2sigma */
	v2sigma2[5] =     v2sigma2[0] + r.dens*
	  (2.0*r.d2fdxtxs[1]*dxtds*dxsds[1] + r.d2fdxs2[2]*dxsds[1]*dxsds[1] + r.dfdxs[1]*d2xsds2[1]);
	v2sigma2[4] = 2.0*v2sigma2[0] + r.dens*
	  (2.0*r.d2fdxtxs[1]*dxtds*dxsds[1]);
	v2sigma2[3] = 4.0*v2sigma2[0];
	v2sigma2[2] =     v2sigma2[0] + r.dens*
	  (    dxtds*(r.d2fdxtxs[0]*dxsds[0] + r.d2fdxtxs[1]*dxsds[1]) + r.d2fdxs2[1]*dxsds[0]*dxsds[1]);
	v2sigma2[1] =  2.0*v2sigma2[0] + r.dens*
	  (2.0*r.d2fdxtxs[0]*dxtds*dxsds[0]);
	v2sigma2[0] =     v2sigma2[0] + r.dens*
	  (2.0*r.d2fdxtxs[0]*dxtds*dxsds[0] + r.d2fdxs2[0]*dxsds[0]*dxsds[0] + r.dfdxs[0]*d2xsds2[0]);

	/* v2rhosigma */
	v2rhosigma[5] =     v2rhosigma[0] + r.dfdxs[1]*dxsds[1] + ndzdn[1]*(r.d2fdzxt*dxtds + r.d2fdzxs[1]*dxsds[1]) + 
	  r.dens*(r.d2fdrsxs[1]*drs*dxsds[1] + r.d2fdxtxs[1]*(dxsdn[1]*dxtds + dxt*dxsds[1]) + r.d2fdxs2[2]*dxsdn[1]*dxsds[1] +
		r.dfdxs[1]*d2xsdns[1]);

	v2rhosigma[4] = 2.0*v2rhosigma[0] + 2.0*ndzdn[1]*r.d2fdzxt*dxtds + 
		  2.0*r.dens*r.d2fdxtxs[1]*dxsdn[1]*dxtds;

	v2rhosigma[3] =     v2rhosigma[0] + r.dfdxs[0]*dxsds[0] + ndzdn[1]*(r.d2fdzxt*dxtds + r.d2fdzxs[0]*dxsds[0]) + 
	  r.dens*(r.d2fdrsxs[0]*drs*dxsds[0] + r.d2fdxtxs[1]*(dxsdn[1]*dxtds + dxt*dxsds[0]) + r.d2fdxs2[1]*dxsdn[1]*dxsds[0]);

	v2rhosigma[2] =     v2rhosigma[0] + r.dfdxs[1]*dxsds[1] + ndzdn[0]*(r.d2fdzxt*dxtds + r.d2fdzxs[1]*dxsds[1]) + 
	  r.dens*(r.d2fdrsxs[1]*drs*dxsds[1] + r.d2fdxtxs[0]*(dxsdn[0]*dxtds + dxt*dxsds[1]) + r.d2fdxs2[1]*dxsdn[0]*dxsds[1]);

	v2rhosigma[1] = 2.0*v2rhosigma[0] + 2.0*ndzdn[0]*r.d2fdzxt*dxtds + 
	  2.0*r.dens*r.d2fdxtxs[0]*dxsdn[0]*dxtds;
	
	v2rhosigma[0] =     v2rhosigma[0] + r.dfdxs[0]*dxsds[0] + ndzdn[0]*(r.d2fdzxt*dxtds + r.d2fdzxs[0]*dxsds[0]) + 
	  r.dens*(r.d2fdrsxs[0]*drs*dxsds[0] + r.d2fdxtxs[0]*(dxsdn[0]*dxtds + dxt*dxsds[0]) + r.d2fdxs2[0]*dxsdn[0]*dxsds[0] + 
		r.dfdxs[0]*d2xsdns[0]);

      }else{
	v2rho2[0]     += 2.0*dxsdn[0]*
	  (2.0*r.dfdxs[0] + r.dens*(2.0*r.d2fdrsxs[0]*drs + 2.0*r.d2fdxtxs[0]*dxt + (r.d2fdxs2[0] + r.d2fdxs2[1])*dxsdn[0]))
	  + 2.0*r.dens*r.dfdxs[0]*d2xsdn2[0];

	v2sigma2[0]   += 2.0*r.dens*((r.d2fdxs2[0] + r.d2fdxs2[1])*dxsds[0]*dxsds[0] + r.dfdxs[0]*d2xsds2[0] + 2.0*r.d2fdxtxs[0]*dxtds*dxsds[0]);

	v2rhosigma[0] += 2.0*r.dens*r.d2fdxtxs[0]*(dxsdn[0]*dxtds + dxt*dxsds[0]) +
	  2.0*(r.dfdxs[0] + r.dens*(r.d2fdrsxs[0]*drs + (r.d2fdxs2[0] + r.d2fdxs2[1])*dxsdn[0]))*dxsds[0]
	  + 2.0*r.dens*r.dfdxs[0]*d2xsdns[0];
      }
    }

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
