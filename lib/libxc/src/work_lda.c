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
  This file is to be included in LDA functionals. As often these
  functionals are written as a function of rs and zeta, this
  routine performs the necessary conversions between this and a functional
  of rho.
************************************************************************/

#ifndef XC_DIMENSIONS
#define XC_DIMENSIONS 3
#endif

static void 
work_lda(const XC(func_type) *p, int np, const FLOAT *rho, 
	 FLOAT *zk, FLOAT *vrho, FLOAT *v2rho2, FLOAT *v3rho3)
{
  XC(lda_work_t) r;
  int is, ip;
  FLOAT dens, drs, d2rs, d3rs;

  /* Wigner radius */
# if   XC_DIMENSIONS == 1
  const FLOAT cnst_rs = 0.5;
# elif XC_DIMENSIONS == 2
  const FLOAT cnst_rs = 0.56418958354775627928; /* = 1.0/sqrt(M_PI) */
# else /* three dimensions */
  const FLOAT cnst_rs = 0.6203504908994000866;  /* = POW(3.0/(4*M_PI), 1.0/3.0)*/
# endif

  r.order = -1;
  if(zk     != NULL) r.order = 0;
  if(vrho   != NULL) r.order = 1;
  if(v2rho2 != NULL) r.order = 2;
  if(v3rho3 != NULL) r.order = 3;
  if(r.order < 0) return;

  for(ip = 0; ip < np; ip++){
    XC(rho2dzeta)(p->nspin, rho, &dens, &r.zeta);

    if(dens < p->info->min_dens) goto end_ip_loop;

    r.rs[1] = cnst_rs*POW(dens, -1.0/XC_DIMENSIONS);
    r.rs[0] = SQRT(r.rs[1]);
    r.rs[2] = r.rs[1]*r.rs[1];

    func(p, &r);

    if(zk != NULL && (p->info->flags & XC_FLAGS_HAVE_EXC))
      *zk = r.zk;

    if(r.order < 1) goto end_ip_loop;

    drs = -r.rs[1]/(XC_DIMENSIONS*dens);
    
    if(vrho != NULL && (p->info->flags & XC_FLAGS_HAVE_VXC)){
      vrho[0] = r.zk + dens*r.dedrs*drs;

      if(p->nspin == XC_POLARIZED){
	vrho[1] = vrho[0] - (r.zeta + 1.0)*r.dedz;
	vrho[0] = vrho[0] - (r.zeta - 1.0)*r.dedz;
      }
    }
  
    if(r.order < 2) goto end_ip_loop;
    
    d2rs = -drs*(1.0 + XC_DIMENSIONS)/(XC_DIMENSIONS*dens);
    
    if(v2rho2 != NULL && (p->info->flags & XC_FLAGS_HAVE_FXC)){
      v2rho2[0] = r.dedrs*(2.0*drs + dens*d2rs) + dens*r.d2edrs2*drs*drs;
      
      if(p->nspin == XC_POLARIZED){
	FLOAT sign[3][2] = {{-1.0, -1.0}, {-1.0, +1.0}, {+1.0, +1.0}};
	
	for(is=2; is>=0; is--){
	  v2rho2[is] = v2rho2[0] - r.d2edrsz*(2.0*r.zeta + sign[is][0] + sign[is][1])*drs
	    + (r.zeta + sign[is][0])*(r.zeta + sign[is][1])*r.d2edz2/dens;
	}
      }
    }
    
    if(r.order < 3) goto end_ip_loop;

    d3rs = -d2rs*(1.0 + 2.0*XC_DIMENSIONS)/(XC_DIMENSIONS*dens);
    
    if(v3rho3 != NULL && (p->info->flags & XC_FLAGS_HAVE_KXC)){
      v3rho3[0] = r.dedrs*(3.0*d2rs + dens*d3rs) + 
	3.0*r.d2edrs2*drs*(drs + dens*d2rs) + r.d3edrs3*dens*drs*drs*drs;
      
      if(p->nspin == XC_POLARIZED){
	FLOAT sign[4][3] = {{-1.0, -1.0, -1.0}, {-1.0, -1.0, +1.0}, {-1.0, +1.0, +1.0}, {+1.0, +1.0, +1.0}};
	
	for(is=3; is>=0; is--){
	  FLOAT ff;
	  
	  v3rho3[is]  = v3rho3[0] - (2.0*r.zeta  + sign[is][0] + sign[is][1])*(d2rs*r.d2edrsz + drs*drs*r.d3edrs2z);
	  v3rho3[is] += (r.zeta + sign[is][0])*(r.zeta + sign[is][1])*(-r.d2edz2/dens + r.d3edrsz2*drs)/dens;
	  
	  ff  = r.d2edrsz*(2.0*drs + dens*d2rs) + dens*r.d3edrs2z*drs*drs;
	  ff += -2.0*r.d2edrsz*drs - r.d3edrsz2*(2.0*r.zeta + sign[is][0] + sign[is][1])*drs;
	  ff += (r.zeta + sign[is][0])*(r.zeta + sign[is][1])*r.d3edz3/dens;
	  ff += (2.0*r.zeta  + sign[is][0] + sign[is][1])*r.d2edz2/dens;
	  
	  v3rho3[is] += -ff*(r.zeta + sign[is][2])/dens;
	}
      }
    }

  end_ip_loop:
    rho += p->n_rho;

    if(zk != NULL)
      zk += p->n_zk;
    
    if(vrho != NULL)
      vrho += p->n_vrho;

    if(v2rho2 != NULL)
      v2rho2 += p->n_v2rho2;

    if(v3rho3 != NULL)
      v3rho3 += p->n_v3rho3;

  } /* for(ip) */
}
