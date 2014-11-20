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
#define XC_MGGA_C_PKZB          239 /* Perdew, Kurth, Zupan, and Blaha */
#define XC_MGGA_C_REVTPSS       241 /* revised TPSS correlation */

typedef struct{
  FLOAT C0_c[4];
  FLOAT d, beta;
} mgga_c_pkzb_params;


static void 
mgga_c_pkzb_init(XC(func_type) *p)
{
  mgga_c_pkzb_params *params;

  assert(p != NULL && p->params == NULL);

  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_GGA_C_PBE, XC_POLARIZED);

  p->params = malloc(sizeof(mgga_c_pkzb_params));

  switch(p->info->number){
  case XC_MGGA_C_PKZB:
  case XC_MGGA_C_TPSS:
    XC(mgga_c_pkzb_set_params)(p, 0.06672455060314922, 2.8, 0.53, 0.87, 0.50, 2.26);
    break;
  case XC_MGGA_C_REVTPSS:
    XC(mgga_c_pkzb_set_params)(p, 0.06672455060314922, 2.8, 0.59, 0.9269, 0.6225, 2.1540);
    break;
  default:
    fprintf(stderr, "Internal error in mgga_c_tpss\n");
    exit(1);
  }
}

void
XC(mgga_c_pkzb_set_params)(XC(func_type) *p, FLOAT beta, FLOAT d, FLOAT C0_0, FLOAT C0_1, FLOAT C0_2, FLOAT C0_3)
{
  mgga_c_pkzb_params *params;

  assert(p != NULL && p->params != NULL);
  params = (mgga_c_pkzb_params *) (p->params);

  params->beta    = beta;
  params->d       = d;
  params->C0_c[0] = C0_0;
  params->C0_c[1] = C0_1;
  params->C0_c[2] = C0_2;
  params->C0_c[3] = C0_3;

  XC(gga_c_pbe_set_params) (p->func_aux[0], beta);
}


static void
tpss_eq_13_14(const FLOAT *C0_c, FLOAT zeta, FLOAT csi2, int order, FLOAT *C, FLOAT *dCdzeta, FLOAT *dCdcsi2)
{
  FLOAT fz, C0, dC0dz, dfzdz, aa, a4;
  FLOAT z2=zeta*zeta;
  
  if(zeta >= 1.0 || zeta <= -1.0){
    *C = 0.0;
    if(order > 0) *dCdcsi2 = *dCdzeta = 0.0;
    return;
  }

  /* Equation (13) */
  C0    = C0_c[0] + z2*(C0_c[1] + z2*(C0_c[2] + z2*C0_c[3]));
  fz    = 0.5*(POW(1.0 + zeta, -4.0/3.0) + POW(1.0 - zeta, -4.0/3.0));

  /* Equation (14) */
  aa = 1.0 + csi2*fz;
  a4 = POW(aa, 4);
  
  *C =  C0 / a4;

  if(order > 0){
    /* Equation (13) */
    dC0dz = zeta*(2.0*C0_c[1] + z2*(4.0*C0_c[2] + z2*6.0*C0_c[3]));
    dfzdz = 0.5*(POW(1.0 + zeta, -7.0/3.0) - POW(1.0 - zeta, -7.0/3.0))*(-4.0/3.0);
  
    /* Equation (14) */
    *dCdcsi2 = -4.0*C0*fz/(aa*a4);
    *dCdzeta = (dC0dz*aa - C0*4.0*csi2*dfzdz)/(aa*a4);
  }
}


static void 
func(const XC(func_type) *pt, XC(mgga_work_c_t) *r)
{
  static const FLOAT tmin = 0.5e-10;

  XC(gga_work_c_t) PBE[3];
  mgga_c_pkzb_params *params;

  FLOAT opz, omz, opz13, omz13, opz23, omz23, taut, xtot, dd, dd2, ddt, ddt2;
  FLOAT C, dCdz, dCdxt, dCdxs[2];
  FLOAT dtautdz, dtautdts[2], dxtotdz, dxtotdxt, dxtotdxs[2];
  FLOAT ddddz, ddddxt, ddddxs[2], ddddts[2], dddtdz, dddtdxt, dddtdxs[2], dddtdts[2];
  int is, is_tpss;

  assert(pt!=NULL && pt->params != NULL);
  params = (mgga_c_pkzb_params *)pt->params;  

  /* first we get the parallel and perpendicular PBE */
  is_tpss = (pt->info->number == XC_MGGA_C_TPSS || pt->info->number == XC_MGGA_C_REVTPSS) ? 1 : 0;
  XC(pbe_c_stoll) (pt->func_aux[0], is_tpss, r, PBE);

  opz = 1.0 + r->zeta;
  omz = 1.0 - r->zeta;

  opz13 = CBRT(opz);
  omz13 = CBRT(omz);

  opz23 = opz13*opz13;
  omz23 = omz13*omz13;

  /* get value of C */
  if(is_tpss){
    FLOAT z2, cnst, aux, csi2;
    FLOAT dCdcsi2,dauxdz, dcsi2dz, dcsi2dxt, dcsi2dxs[2];

    z2  = r->zeta*r->zeta;
    cnst = 2.0*CBRT(3.0*M_PI*M_PI);

    aux  = -r->xt*r->xt + (r->xs[0]*r->xs[0]*opz*opz23 + r->xs[1]*r->xs[1]*omz*omz23)/(2.0*M_CBRT2*M_CBRT2);
    csi2 = (1.0 - z2)*aux/(cnst*cnst);

    tpss_eq_13_14(params->C0_c, r->zeta, csi2, r->order, &C, &dCdz, &dCdcsi2);

    if(r->order >= 1){
      dauxdz   = 5.0*(r->xs[0]*r->xs[0]*opz23 - r->xs[1]*r->xs[1]*omz23)/(6.0*M_CBRT2*M_CBRT2);
      dcsi2dz  = (-2.0*r->zeta*aux + (1.0 - z2)*dauxdz)/(cnst*cnst);

      dcsi2dxt = -2.0*(1.0 - z2)*r->xt/(cnst*cnst);
      dcsi2dxs[0] = (1.0 - z2)*r->xs[0]*opz*opz23/(cnst*cnst*M_CBRT2*M_CBRT2);
      dcsi2dxs[1] = (1.0 - z2)*r->xs[1]*omz*omz23/(cnst*cnst*M_CBRT2*M_CBRT2);

      dCdz    += dCdcsi2*dcsi2dz;
      dCdxt    = dCdcsi2*dcsi2dxt;
      dCdxs[0] = dCdcsi2*dcsi2dxs[0];
      dCdxs[1] = dCdcsi2*dcsi2dxs[1];
    }

  }else{
    C = 0.53;
    if(r->order >= 1) dCdz = dCdxt = dCdxs[0] = dCdxs[1] = 0.0;
  }

  /* we get the spin compensated part */
  taut = (r->ts[0]*opz*opz23 + r->ts[1]*omz*omz23)/(2.0*M_CBRT2*M_CBRT2);

  if(is_tpss)
    xtot = r->xt*r->xt;
  else
    xtot = (r->xs[0]*r->xs[0]*opz*opz23 + r->xs[1]*r->xs[1]*omz*omz23)/(2.0*M_CBRT2*M_CBRT2);

  ddt = (taut > tmin) ? xtot/(8.0*taut) : 0.0;
  ddt2 = ddt*ddt;
  
  /* the functional */
  r->f = (1.0 + C*ddt2)*PBE[2].f;

  /* and the derivative */
  if(r->order >= 1){
    if(taut > tmin){
      dtautdz     = 5.0/3.0 * (r->ts[0]*opz23 - r->ts[1]*omz23)/(2.0*M_CBRT2*M_CBRT2);
      dtautdts[0] = opz*opz23/(2.0*M_CBRT2*M_CBRT2);
      dtautdts[1] = omz*omz23/(2.0*M_CBRT2*M_CBRT2);

      if(is_tpss){
	dxtotdz     = 0.0;
	dxtotdxt    = 2.0*r->xt;
	dxtotdxs[0] = 0.0;
	dxtotdxs[1] = 0.0;
      }else{
	dxtotdz     = 5.0/3.0 * (r->xs[0]*r->xs[0]*opz23 - r->xs[1]*r->xs[1]*omz23)/(2.0*M_CBRT2*M_CBRT2);
	dxtotdxt    = 0.0;
	dxtotdxs[0] = 2.0*r->xs[0]*opz*opz23/(2.0*M_CBRT2*M_CBRT2);
	dxtotdxs[1] = 2.0*r->xs[1]*omz*omz23/(2.0*M_CBRT2*M_CBRT2);
      }
      
      dddtdz     = (dxtotdz*taut - xtot*dtautdz)/(8.0*taut*taut);
      dddtdxt    = dxtotdxt/(8.0*taut);
      dddtdxs[0] = dxtotdxs[0]/(8.0*taut);
      dddtdxs[1] = dxtotdxs[1]/(8.0*taut);
      dddtdts[0] = -xtot*dtautdts[0]/(8.0*taut*taut);
      dddtdts[1] = -xtot*dtautdts[1]/(8.0*taut*taut);
    }else{
      dddtdz = dddtdxs[0] = dddtdxs[1] = dddtdts[0] = dddtdts[1] = 0.0;
    }

    r->dfdrs    = (1.0 + C*ddt2)*PBE[2].dfdrs;
    r->dfdz     = (1.0 + C*ddt2)*PBE[2].dfdz     + (dCdz    *ddt2 + 2.0*C*ddt*dddtdz    )*PBE[2].f;
    r->dfdxt    = (1.0 + C*ddt2)*PBE[2].dfdxt    + (dCdxt   *ddt2 + 2.0*C*ddt*dddtdxt   )*PBE[2].f;
    r->dfdxs[0] = (1.0 + C*ddt2)*PBE[2].dfdxs[0] + (dCdxs[0]*ddt2 + 2.0*C*ddt*dddtdxs[0])*PBE[2].f;
    r->dfdxs[1] = (1.0 + C*ddt2)*PBE[2].dfdxs[1] + (dCdxs[1]*ddt2 + 2.0*C*ddt*dddtdxs[1])*PBE[2].f;
    r->dfdts[0] = 2.0*C*ddt*dddtdts[0]*PBE[2].f;
    r->dfdts[1] = 2.0*C*ddt*dddtdts[1]*PBE[2].f;
  }

  if(r->order >= 2){
  }

  /* now the spin-resolved part */
  for(is = 0; is < 2; is++){
    int js = (is == 0) ? 1 : 0;

    if(is_tpss){
      dd  = ddt;
      dd2 = ddt2;
    }else{
      dd  = (r->ts[is] > tmin) ? r->xs[is]*r->xs[is]/(8.0*r->ts[is]) : 0.0;
      dd2 = dd*dd;
    }

    r->f += -(1.0 + C)*dd2*PBE[is].f;

    if(r->order < 1) continue;

    if(is_tpss){
      ddddz     = dddtdz;
      ddddxt    = dddtdxt;
      ddddxs[0] = dddtdxs[0];
      ddddxs[1] = dddtdxs[1];
      ddddts[0] = dddtdts[0];
      ddddts[1] = dddtdts[1];
    }else{
      ddddz = ddddxt = ddddxs[js] = ddddts[js] = 0.0;
      if(r->ts[is] > tmin){
	ddddxs[is] = 2.0*r->xs[is]/(8.0*r->ts[is]);
	ddddts[is] = -r->xs[is]*r->xs[is]/(8.0*r->ts[is]*r->ts[is]);
      }else
	ddddxs[is] = ddddts[is] = 0.0;
    }

    r->dfdrs    += -(1.0 + C)*dd2*PBE[is].dfdrs;
    r->dfdz     += -(1.0 + C)*dd*(2.0*ddddz    *PBE[is].f + dd*PBE[is].dfdz)     - dCdz    *dd2*PBE[is].f;
    r->dfdxt    += -(1.0 + C)*dd*(2.0*ddddxt   *PBE[is].f + dd*PBE[is].dfdxt)    - dCdxt   *dd2*PBE[is].f;
    r->dfdxs[0] += -(1.0 + C)*dd*(2.0*ddddxs[0]*PBE[is].f + dd*PBE[is].dfdxs[0]) - dCdxs[0]*dd2*PBE[is].f;
    r->dfdxs[1] += -(1.0 + C)*dd*(2.0*ddddxs[1]*PBE[is].f + dd*PBE[is].dfdxs[1]) - dCdxs[1]*dd2*PBE[is].f;
    r->dfdts[0] += -(1.0 + C)*2.0*dd*ddddts[0]*PBE[is].f;
    r->dfdts[1] += -(1.0 + C)*2.0*dd*ddddts[1]*PBE[is].f;
  }

  if(is_tpss){
    r->f = r->f*(1.0 + params->d*r->f*ddt*ddt2);

    if(r->order >= 1){
      r->dfdrs = r->dfdrs*(1.0 + 2.0*params->d*r->f*ddt*ddt2);
      r->dfdz  = r->dfdz *(1.0 + 2.0*params->d*r->f*ddt*ddt2) + 3.0*r->f*r->f*params->d*dddtdz *ddt2;
      r->dfdxt = r->dfdxt*(1.0 + 2.0*params->d*r->f*ddt*ddt2) + 3.0*r->f*r->f*params->d*dddtdxt*ddt2;
      r->dfdxs[0] = r->dfdxs[0]*(1.0 + 2.0*params->d*r->f*ddt*ddt2) + 3.0*r->f*r->f*params->d*dddtdxs[0]*ddt2;
      r->dfdxs[1] = r->dfdxs[1]*(1.0 + 2.0*params->d*r->f*ddt*ddt2) + 3.0*r->f*r->f*params->d*dddtdxs[1]*ddt2;
      r->dfdts[0] = r->dfdts[0]*(1.0 + 2.0*params->d*r->f*ddt*ddt2) + 3.0*r->f*r->f*params->d*dddtdts[0]*ddt2;
      r->dfdts[1] = r->dfdts[1]*(1.0 + 2.0*params->d*r->f*ddt*ddt2) + 3.0*r->f*r->f*params->d*dddtdts[1]*ddt2;
    }
  }
}


#include "work_mgga_c.c"


XC(func_info_type) XC(func_info_mgga_c_pkzb) = {
  XC_MGGA_C_PKZB,
  XC_CORRELATION,
  "Perdew, Kurth, Zupan, and Blaha",
  XC_FAMILY_MGGA,
  "JP Perdew, S Kurth, A Zupan, and P. Blaha, Phys. Rev. Lett. 82, 2544-2547 (1999)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-26, 1e-32, 1e-32, 1e-32, /* densities smaller than 1e-26 give NaNs */
  mgga_c_pkzb_init,
  NULL, NULL, NULL,
  work_mgga_c,
};

XC(func_info_type) XC(func_info_mgga_c_tpss) = {
  XC_MGGA_C_TPSS,
  XC_CORRELATION,
  "Tao, Perdew, Staroverov & Scuseria",
  XC_FAMILY_MGGA,
  "J Tao, JP Perdew, VN Staroverov, and G Scuseria, Phys. Rev. Lett. 91, 146401 (2003)\n"
  "JP Perdew, J Tao, VN Staroverov, and G Scuseria, J. Chem. Phys. 120, 6898 (2004)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-26, 1e-32, 1e-32, 1e-32, /* densities smaller than 1e-26 give NaNs */
  mgga_c_pkzb_init,
  NULL, NULL, NULL,
  work_mgga_c,
};

XC(func_info_type) XC(func_info_mgga_c_revtpss) = {
  XC_MGGA_C_REVTPSS,
  XC_CORRELATION,
  "revised TPSS correlation",
  XC_FAMILY_MGGA,
  "JP Perdew, A Ruzsinszky, GI Csonka, LA Constantin1, and J Sun, Phys. Rev. Lett. 103, 026403 (2009)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-26, 1e-32, 1e-32, 1e-32, /* densities smaller than 1e-26 give NaNs */
  mgga_c_pkzb_init,
  NULL, NULL, NULL,
  work_mgga_c,
};
