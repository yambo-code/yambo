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

#define XC_MGGA_C_TPSS          231 /* Perdew, Tao, Staroverov & Scuseria correlation */

/************************************************************************
 Implements Perdew, Tao, Staroverov & Scuseria 
   meta-Generalized Gradient Approximation.
   Correlation part
************************************************************************/

static void
mgga_c_tpss_init(void *p_)
{
  XC(mgga_type) *p = (XC(mgga_type) *)p_;

  p->n_func_aux  = 2;
  p->func_aux    = (XC(func_type) **) malloc(sizeof(XC(func_type) *)*p->n_func_aux);
  p->func_aux[0] = (XC(func_type) *)  malloc(sizeof(XC(func_type)));
  p->func_aux[1] = (XC(func_type) *)  malloc(sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_GGA_C_PBE, p->nspin);
  XC(func_init)(p->func_aux[1], XC_GGA_C_PBE, XC_POLARIZED);
}


/* some parameters */
static FLOAT param_d = 2.8; /* Hartree^-1 */

static void
eq_13_14(FLOAT zeta, FLOAT csi, int order, FLOAT *C, FLOAT *dCdzeta, FLOAT *dCdcsi)
{
  FLOAT fz, C0, dC0dz, dfzdz, aa, a4;
  FLOAT z2=zeta*zeta, csi2=csi*csi;
  
  if(zeta==1.0 || zeta==-1.0){
    *C = 0.0;
    if(order > 0)
      *dCdcsi = *dCdzeta = 0.0;
    return;
  }

  /* Equation (13) */
  C0    = 0.53 + z2*(0.87 + z2*(0.50 + z2*2.26));
  fz    = 0.5*(POW(1.0 + zeta, -4.0/3.0) + POW(1.0 - zeta, -4.0/3.0));

  /* Equation (14) */
  aa = 1.0 + csi2*fz;
  a4 = POW(aa, 4);
  
  *C =  C0 / a4;

  if(order > 0){
    /* Equation (13) */
    dC0dz = zeta*(2.0*0.87 + z2*(4.0*0.5 + z2*6.0*2.26));
    dfzdz = 0.5*(POW(1.0 + zeta, -7.0/3.0) - POW(1.0 - zeta, -7.0/3.0))*(-4.0/3.0);
  
    /* Equation (14) */
    *dCdcsi = -8.0*C0*csi*fz/(aa*a4);
    *dCdzeta = (dC0dz*aa - C0*4.0*csi2*dfzdz)/(aa*a4);
  }
}


/* Equation 12 */
static void eq_12(const XC(mgga_type) *p, int order, const FLOAT *rho, const FLOAT *sigma, 
		  FLOAT dens, FLOAT zeta, FLOAT z,
		  FLOAT *f_PKZB, FLOAT *vrho_PKZB, FLOAT *vsigma_PKZB, FLOAT *vz_PKZB)
{
  FLOAT f_PBE,    vrho_PBE[2], vsigma_PBE[3];
  FLOAT f_til[2], vrho_til[2][2], vsigma_til[2][3];

  FLOAT C, dCdcsi=0.0, dCdzeta=0.0;
  FLOAT dzetadd[2], dcsidd[2], dcsidsigma[3];
  int is, sigs;

  FLOAT z2 = z*z, f_aux, vrho_aux[2], vsigma_aux[3];

  sigs = (p->nspin == XC_UNPOLARIZED) ? 1 : 3;

  /* let us get the PBE stuff */
  if(order == 0)
    XC(gga_exc)(p->func_aux[0], 1, rho, sigma, &f_PBE);
  else
    XC(gga_exc_vxc)(p->func_aux[0], 1, rho, sigma, &f_PBE, vrho_PBE, vsigma_PBE);
    
  for(is=0; is<p->nspin; is++){
    FLOAT r1[2], sigma1[3], f1, vrho1[2], vsigma1[3];
    FLOAT sfac = (p->nspin == XC_UNPOLARIZED) ? 0.5 : 1.0;
    int js = (is == 0) ? 0 : 2;

    /* build fully polarized density and gradient */
    r1[0] = rho[is] * sfac;
    r1[1] = 0.0;

    sigma1[0] = sigma[js] * sfac*sfac;
    sigma1[1] = 0.0;
    sigma1[2] = 0.0;

    /* call (polarized) PBE */
    if(order == 0)
      XC(gga_exc)(p->func_aux[1], 1, r1, sigma1, &f1);
    else{
      XC(gga_exc_vxc)(p->func_aux[1], 1, r1, sigma1, &f1, vrho1, vsigma1);

      if(f1 > f_PBE){
	if(rho[is] > p->info->min_dens){
	  vrho_til[is][is]   = f1 + (vrho1[0] - f1)*dens/rho[is];
	  vsigma_til[is][js] = vsigma1[0]*dens/rho[is];
	}else{
	  vrho_til[is][is] = 0.0;
	  vsigma_til[is][js] = 0.0;
	}

	if(p->nspin == XC_POLARIZED){
	  int ns;

	  ns = (is == 0) ? 1 : 0;
	  vrho_til[is][ns] = 0.0;

	  ns = (is == 0) ? 2 : 0;
	  vsigma_til[is][ 1] = 0.0;
	  vsigma_til[is][ns] = 0.0;
	}else
	  vsigma_til[is][js] /= 2.0;

      }else{
	int ks;

	for(ks=0; ks<p->nspin; ks++)
	  vrho_til[is][ks] = vrho_PBE[ks];
	for(ks=0; ks<sigs; ks++)
	  vsigma_til[is][ks] = vsigma_PBE[ks];
      }
    }
    f_til[is] = (f1 > f_PBE) ? f1 : f_PBE;
  }

  if(p->nspin == XC_UNPOLARIZED){
    C             = 0.53;
    dzetadd[0]    = 0.0;
    dcsidd [0]    = 0.0;
    dcsidsigma[0] = 0.0;

  }else{ /* get C(csi, zeta) */
    FLOAT gzeta, gzeta2, csi, aa;
    
    gzeta2 = 
      +     sigma[0]*(1.0 - zeta)*(1.0 - zeta)
      - 2.0*sigma[1]*(1.0 + zeta)*(1.0 - zeta)
      +     sigma[2]*(1.0 + zeta)*(1.0 + zeta);

    gzeta2 = max(gzeta2, p->info->min_grad*p->info->min_grad);
    gzeta  = SQRT(gzeta2);

    aa  = 2.0*CBRT(3.0*M_PI*M_PI*dens);
    csi = gzeta/aa;
  
    eq_13_14(zeta, csi, order, &C, &dCdzeta, &dCdcsi);
    
    if(order > 0){
      FLOAT bb;
      
      dzetadd[0] =  (1.0 - zeta)/dens;
      dzetadd[1] = -(1.0 + zeta)/dens;
      
      bb = 2.0*sigma[0]*(zeta - 1.0) + 
	4.0*sigma[1]*zeta + 2.0*sigma[2]*(zeta + 1.0);

      dcsidd[0] = -1.0*csi/(3.0*dens) + bb/(2.0*aa*gzeta)*dzetadd[0];
      dcsidd[1] = -1.0*csi/(3.0*dens) + bb/(2.0*aa*gzeta)*dzetadd[1];

      dcsidsigma[0] =      (1.0 - zeta)*(1.0 - zeta)/(2.0*aa*gzeta);
      dcsidsigma[1] = -2.0*(1.0 - zeta)*(1.0 + zeta)/(2.0*aa*gzeta);
      dcsidsigma[2] =      (1.0 + zeta)*(1.0 + zeta)/(2.0*aa*gzeta);
    }
  } /* get C(csi, zeta) */

    /* f_aux = sum_sigma n_sigma/n e_til */
  f_aux = 0.0;
  for(is=0; is<p->nspin; is++)
    f_aux += rho[is] * f_til[is];
  f_aux /= dens;

  if(order > 0){
    for(is=0; is<p->nspin; is++)
      vrho_aux[is]  = (dens - rho[is])*f_til[is]/dens;

    for(is=0; is<p->nspin; is++){
      int ks;

      for(ks=0; ks<p->nspin; ks++)
	vrho_aux[ks] += vrho_til[is][ks]*rho[is]/dens;
      
      for(ks=0; ks<sigs; ks++)
	vsigma_aux[ks] = vsigma_til[is][ks]*rho[is]/dens;
    }
  }
  
  *f_PKZB  = f_PBE*(1 + C*z2) - (1.0 + C)*z2*f_aux;
  
  if(order > 0 ){
    *vz_PKZB = dens * 2.0*z * (C*f_PBE - (1.0 + C)*f_aux);
    
    for(is=0; is<p->nspin; is++){
      FLOAT dCdd;
      
      dCdd = dCdzeta*dzetadd[is] + dCdcsi*dcsidd[is];
      
      vrho_PKZB[is] = vrho_PBE[is]*(1.0 + C*z2) + dens*f_PBE*dCdd*z2;
      vrho_PKZB[is]-= vrho_aux[is]*(1.0 + C)*z2 + dens*f_aux*dCdd*z2;
    }
    for(is=0; is<sigs; is++){
      FLOAT dCdsigma;
      
      dCdsigma =  dCdcsi*dcsidsigma[is];

      vsigma_PKZB[is] = vsigma_PBE[is]*(1.0 + C*z2) + dens*f_PBE*dCdsigma*z2;
      vsigma_PKZB[is]-= vsigma_aux[is]*(1.0 + C)*z2 + dens*f_aux*dCdsigma*z2;
    }
  }
}


static void 
my_mgga_c_tpss(const void *p_, 
	       const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl, const FLOAT *tau,
	       FLOAT *zk, FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau,
	       FLOAT *v2rho2, FLOAT *v2sigma2, FLOAT *v2lapl2, FLOAT *v2tau2,
	       FLOAT *v2rhosigma, FLOAT *v2rholapl, FLOAT *v2rhotau, 
	       FLOAT *v2sigmalapl, FLOAT *v2sigmatau, FLOAT *v2lapltau)
{
  const XC(mgga_type) *p = (const XC(mgga_type) *) p_;

  int is, sigs, order;
  FLOAT dens, zeta, sigmat;
  FLOAT taut, tauw, z, z2, z3;
  FLOAT f_PKZB, vrho_PKZB[2], vsigma_PKZB[3], vz_PKZB;
  FLOAT dfdz, dzdd, dzdsigma[3], dzdtau;

  order = 0;
  if(vrho   != NULL) order = 1;
  if(v2rho2 != NULL) order = 2;  

  /* get spin-summed variables */
  XC(rho2dzeta)(p->nspin, rho, &dens, &zeta);
  taut   = tau[0];
  sigmat = sigma[0];
  if(p->nspin == XC_POLARIZED){
    taut   += tau[1];
    sigmat += 2.0*sigma[1] + sigma[2];
  }

  /* sometimes numerical errors create problems */
  sigmat = max(p->info->min_grad*p->info->min_grad, sigmat);

  tauw   = max(sigmat/(8.0*dens), 1.0e-12);
  taut   = max(taut, tauw);

  z  = tauw/taut;
  z2 = z*z;
  z3 = z2*z;

  /* Equation (12) */
  eq_12(p, order, rho, sigma, dens, zeta, z,
	&f_PKZB, vrho_PKZB, vsigma_PKZB, &vz_PKZB);
  
  /* Equation (11) */
  *zk  = f_PKZB*(1.0 + param_d*f_PKZB*z3);

  if(order < 1) return;

  dzdd        = -z/dens;
  dzdtau      = -z/taut;
  dzdsigma[0] = 1.0/(8.0*dens*taut);
  if(p->nspin == XC_POLARIZED){
    dzdsigma[1] = 2.0*dzdsigma[0];
    dzdsigma[2] =     dzdsigma[0];
  }

  dfdz = vz_PKZB*(1.0 + 2.0*param_d*f_PKZB*z3) +
    dens * f_PKZB*f_PKZB * param_d * 3.0*z2;

  for(is=0; is<p->nspin; is++){
    vrho[is]  = vrho_PKZB[is]*(1.0 + 2.0*param_d*f_PKZB*z3);
    vrho[is] -= f_PKZB*f_PKZB * param_d * z3;
    vrho[is] += dfdz*dzdd;

    vtau[is] = dfdz*dzdtau;
  }

  sigs = (p->nspin==XC_UNPOLARIZED) ? 1 : 3;
  for(is=0; is<sigs; is++){
    vsigma[is] = vsigma_PKZB[is] * (1.0 + 2.0*param_d*f_PKZB*z3);
    vsigma[is] += dfdz*dzdsigma[is];
  }
}

/* Warning: this is a workaround to support blocks while waiting for the next interface */
static void 
mgga_c_tpss(const void *p_, int np,
	    const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl, const FLOAT *tau,
	    FLOAT *zk, FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau,
	    FLOAT *v2rho2, FLOAT *v2sigma2, FLOAT *v2lapl2, FLOAT *v2tau2,
	    FLOAT *v2rhosigma, FLOAT *v2rholapl, FLOAT *v2rhotau, 
	    FLOAT *v2sigmalapl, FLOAT *v2sigmatau, FLOAT *v2lapltau)
{
  int ip;
  const XC(mgga_type) *p = (const XC(mgga_type) *) p_;

  for(ip=0; ip<np; ip++){
    my_mgga_c_tpss(p_, rho, sigma, lapl, tau,
		   zk, vrho, vsigma, vlapl, vtau,
		   v2rho2, v2sigma2, v2lapl2, v2tau2, v2rhosigma, v2rholapl, v2rhotau,
		   v2sigmalapl, v2sigmatau, v2lapltau);

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


XC(func_info_type) XC(func_info_mgga_c_tpss) = {
  XC_MGGA_C_TPSS,
  XC_CORRELATION,
  "Tao, Perdew, Staroverov & Scuseria",
  XC_FAMILY_MGGA,
  "J Tao, JP Perdew, VN Staroverov, and G Scuseria, Phys. Rev. Lett. 91, 146401 (2003)\n"
  "JP Perdew, J Tao, VN Staroverov, and G Scuseria, J. Chem. Phys. 120, 6898 (2004)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_c_tpss_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  mgga_c_tpss,
};
