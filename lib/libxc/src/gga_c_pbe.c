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

/************************************************************************
 Implements Perdew, Burke & Ernzerhof Generalized Gradient Approximation
 correlation functional.

 I based this implementation on a routine from L.C. Balbas and J.M. Soler
************************************************************************/

#define XC_GGA_C_PBE          130 /* Perdew, Burke & Ernzerhof correlation              */
#define XC_GGA_C_PBE_SOL      133 /* Perdew, Burke & Ernzerhof correlation SOL          */
#define XC_GGA_C_XPBE         136 /* xPBE reparametrization by Xu & Goddard             */
#define XC_GGA_C_PBE_JRGX     138 /* JRGX reparametrization by Pedroza, Silva & Capelle */
#define XC_GGA_C_RGE2         143 /* Regularized PBE                                    */
#define XC_GGA_C_APBE         186 /* mu fixed from the semiclassical neutral atom       */
#define XC_GGA_C_SPBE          89 /* PBE correlation to be used with the SSB exchange   */
#define XC_GGA_C_VPBE          83 /* variant PBE                                        */

typedef struct{
  FLOAT beta;
} gga_c_pbe_params;


static void gga_c_pbe_init(XC(func_type) *p)
{
  static const FLOAT beta[7]  = {
    0.06672455060314922,                /* original PBE */
    0.046,                              /* PBE sol      */
    0.089809,                           /* xPBE         */
    3.0*10.0/(81.0*M_PI*M_PI),          /* PBE_JRGX     */
    0.053,                              /* RGE2         */
    3.0*0.260/(M_PI*M_PI),              /* APBE (C)     */
    0.06672455060314922                 /* sPBE         */
  };

  assert(p!=NULL && p->params == NULL);
  p->params = malloc(sizeof(gga_c_pbe_params));

  p->n_func_aux  = 1;
  p->func_aux    = (XC(func_type) **) malloc(1*sizeof(XC(func_type) *));
  p->func_aux[0] = (XC(func_type) *)  malloc(  sizeof(XC(func_type)));

  XC(func_init)(p->func_aux[0], XC_LDA_C_PW_MOD, p->nspin);

  switch(p->info->number){
  case XC_GGA_C_PBE:      p->func = 0; break;    
  case XC_GGA_C_PBE_SOL:  p->func = 1; break;
  case XC_GGA_C_XPBE:     p->func = 2; break;
  case XC_GGA_C_PBE_JRGX: p->func = 3; break;
  case XC_GGA_C_RGE2:     p->func = 4; break;
  case XC_GGA_C_APBE:     p->func = 5; break;
  case XC_GGA_C_SPBE:     p->func = 6; break;
  case XC_GGA_C_VPBE:     p->func = 7; break;
  default:
    fprintf(stderr, "Internal error in gga_c_pbe\n");
    exit(1);
  }

  XC(gga_c_pbe_set_params)(p, beta[p->func]);
}


void 
XC(gga_c_pbe_set_params)(XC(func_type) *p, FLOAT beta)
{
  gga_c_pbe_params *params;

  assert(p != NULL && p->params != NULL);
  params = (gga_c_pbe_params *) (p->params);

  params->beta = beta;
}


static inline void 
pbe_eq8(int order, FLOAT beta, FLOAT gamm, FLOAT ecunif, FLOAT phi, 
	FLOAT *A, FLOAT *dbeta, FLOAT *dec, FLOAT *dphi,
	FLOAT *dec2, FLOAT *decphi, FLOAT *dphi2)
{
  FLOAT phi3, f1, df1dphi, d2f1dphi2, f2, f3, dx, d2x;

  phi3 = POW(phi, 3);
  f1   = ecunif/(gamm*phi3);
  f2   = exp(-f1);
  f3   = f2 - 1.0;

  *A   = beta/(gamm*f3);

  if(order < 1) return;

  df1dphi = -3.0*f1/phi;
  dx      = (*A)*f2/f3;

  *dbeta  = 1.0/(gamm*f3);
  *dec    = dx/(gamm*phi3);
  *dphi   = dx*df1dphi;

  if(order < 2) return;

  d2f1dphi2 = -4.0*df1dphi/phi;
  d2x       = dx*(2.0*f2 - f3)/f3;
  *dphi2    = d2x*df1dphi*df1dphi + dx*d2f1dphi2;
  *decphi   = (d2x*df1dphi*f1 + dx*df1dphi)/ecunif;
  *dec2     = d2x/(gamm*gamm*phi3*phi3);
}


static inline void 
pbe_eq7(int order, FLOAT beta, FLOAT gamm, FLOAT phi, FLOAT t, FLOAT A, FLOAT B,
	FLOAT *H, FLOAT *dbeta, FLOAT *dphi, FLOAT *dt, FLOAT *dA,
	FLOAT *d2phi, FLOAT *d2phit, FLOAT *d2phiA, FLOAT *d2t2, FLOAT *d2tA, FLOAT *d2A2)
{
  FLOAT t2, phi3, f1, f2, f3;
  FLOAT df2dbeta, df1dt, df2dt, df1dA, df2dA;
  FLOAT d2f1dt2, d2f2dt2, d2f2dA2, d2f1dtA, d2f2dtA;

  t2   = t*t;
  phi3 = POW(phi, 3);

  f1 = t2 + B*A*t2*t2;
  f3 = 1.0 + A*f1;
  f2 = beta*f1/(gamm*f3);

  *H = gamm*phi3*log(1.0 + f2);

  if(order < 1) return;

  *dphi  = 3.0*(*H)/phi;
    
  df2dbeta = f2/beta;
  *dbeta   = gamm*phi3*df2dbeta/(1.0 + f2);

  df1dt    = t*(2.0 + 4.0*B*A*t2);
  df2dt    = beta/(gamm*f3*f3) * df1dt;
  *dt      = gamm*phi3*df2dt/(1.0 + f2);
    
  df1dA    = B*t2*t2;
  df2dA    = beta/(gamm*f3*f3) * (df1dA - f1*f1);
  *dA      = gamm*phi3*df2dA/(1.0 + f2);

  if(order < 2) return;

  *d2phi  = 2.0*(*dphi)/phi;
  *d2phit = 3.0*(*dt)/phi;
  *d2phiA = 3.0*(*dA)/phi;

  d2f1dt2 = 2.0 + 4.0*3.0*B*A*t2;
  d2f2dt2 = beta/(gamm*f3*f3) * (d2f1dt2 - 2.0*A/f3*df1dt*df1dt);
  *d2t2   = gamm*phi3*(d2f2dt2*(1.0 + f2) - df2dt*df2dt)/((1.0 + f2)*(1.0 + f2));

  d2f1dtA = 4.0*B*t*t2;
  d2f2dtA = beta/(gamm*f3*f3) * 
    (d2f1dtA - 2.0*df1dt*(f1 + A*df1dA)/f3);
  *d2tA   = gamm*phi3*(d2f2dtA*(1.0 + f2) - df2dt*df2dA)/((1.0 + f2)*(1.0 + f2));

  d2f2dA2 = beta/(gamm*f3*f3*f3) *(-2.0)*(2.0*f1*df1dA - f1*f1*f1 + A*df1dA*df1dA);
  *d2A2   = gamm*phi3*(d2f2dA2*(1.0 + f2) - df2dA*df2dA)/((1.0 + f2)*(1.0 + f2));
}


inline void 
XC(gga_c_pbe_func) (const XC(func_type) *p, XC(gga_work_c_t) *r)
{
  /* parameter for beta of VPBE */
  static FLOAT b_1 = 0.066725, b_2 = 0.1, b_3 = 0.1778;

  FLOAT cnst_beta, phi, t;

  FLOAT A, dAdbeta, dAdec, dAdphi, d2Adec2, d2Adecphi, d2Adphi2;
  FLOAT H, dHdbeta, dHdphi, dHdt, dHdA, d2Hdphi2, d2Hdphit, d2HdphiA, d2Hdt2, d2HdtA, d2HdA2;
  FLOAT dfdbeta, dfdphi, dfdec, dfdt, dtdrs, dtdxt, dtdphi, dphidz;
  FLOAT d2fdphi2, d2fdphit, d2fdphiec, d2fdt2, d2fdtec, d2fdec2;
  FLOAT d2tdrs2, d2tdrsxt, d2tdphi2, d2tdrsphi, d2tdxtphi, d2phidz2;
  FLOAT B;

  XC(lda_work_t) pw;
  FLOAT tconv, auxp, auxm, beta, gamm, beta_den, dbetadrs;

  assert(p->params != NULL);
  cnst_beta = ((gga_c_pbe_params *) (p->params))->beta;

  pw.order = r->order;
  pw.rs[0] = SQRT(r->rs);
  pw.rs[1] = r->rs;
  pw.rs[2] = r->rs*r->rs;
  pw.zeta  = r->zeta;

  XC(lda_c_pw_func)(p->func_aux[0], &pw);

  tconv = 4.0*M_CBRT2;

  auxp = CBRT(1.0 + r->zeta);
  auxm = CBRT(1.0 - r->zeta);

  phi  = 0.5*(auxp*auxp + auxm*auxm);
  t    = r->xt/(tconv*phi*pw.rs[0]);

  if(p->func == 2)
    gamm = cnst_beta*cnst_beta/(2.0*0.197363); /* for xPBE */
  else
    gamm = (1.0 - log(2.0))/(M_PI*M_PI);

  if(p->func == 7){
    beta_den = (1.0 + b_3*r->rs);
    beta = b_1 * (1.0 + b_2*r->rs)/beta_den;
  }else
    beta = cnst_beta;

  pbe_eq8(r->order, beta, gamm, pw.zk, phi,
	  &A, &dAdbeta, &dAdec, &dAdphi, &d2Adec2, &d2Adecphi, &d2Adphi2);

  /* the sPBE functional contains one term less than the original PBE, so we set it to zero */
  B = (p->func == 6) ? 0.0 : 1.0;
  pbe_eq7(r->order, beta, gamm, phi, t, A, B,
	  &H, &dHdbeta, &dHdphi, &dHdt, &dHdA, &d2Hdphi2, &d2Hdphit, &d2HdphiA, &d2Hdt2, &d2HdtA, &d2HdA2);

  r->f = pw.zk + H;

  if(r->order < 1) return;

  if(p->func == 7){
    dbetadrs = b_1*(b_2 - b_3)/(beta_den*beta_den);
  }else
    dbetadrs = 0.0;

  /* full derivatives of functional */
  dfdbeta= dHdbeta + dHdA*dAdbeta;
  dfdphi = dHdphi + dHdA*dAdphi;
  dfdt   = dHdt;
  dfdec  = 1.0 + dHdA*dAdec;

  dphidz = 0.0;
  if(auxp > p->info->min_zeta) dphidz += 1/auxp;
  if(auxm > p->info->min_zeta) dphidz -= 1/auxm;
  dphidz *= 1.0/3.0;

  dtdrs  = -r->xt/(2.0*tconv*phi*r->rs*pw.rs[0]);
  dtdxt  =  t/r->xt;
  dtdphi = -t/phi;

  r->dfdrs   = dfdec*pw.dedrs + dfdt*dtdrs + dfdbeta*dbetadrs;
  r->dfdz    = dfdec*pw.dedz + (dfdphi + dfdt*dtdphi)*dphidz;
  r->dfdxt   = dfdt*dtdxt;
  r->dfdxs[0] = 0.0;
  r->dfdxs[1] = 0.0;

  if(r->order < 2) return;

  /* full derivatives of functional with respect to phi and zk */
  d2fdphi2  = d2Hdphi2 + 2.0*d2HdphiA*dAdphi + dHdA*d2Adphi2 + d2HdA2*dAdphi*dAdphi;
  d2fdphit  = d2Hdphit + d2HdtA*dAdphi;
  d2fdphiec = d2HdphiA*dAdec + d2HdA2*dAdphi*dAdec + dHdA*d2Adecphi;
  d2fdt2    = d2Hdt2;
  d2fdtec   = d2HdtA*dAdec;
  d2fdec2   = d2HdA2*dAdec*dAdec + dHdA*d2Adec2;

  d2phidz2 = 0.0;
  if(auxp > p->info->min_zeta) d2phidz2 += 1.0/((1.0 + r->zeta)*auxp);
  if(auxm > p->info->min_zeta) d2phidz2 += 1.0/((1.0 - r->zeta)*auxm);
  d2phidz2 *= -1.0/9.0;

  d2tdrs2   =  3.0*r->xt/(4.0*tconv*phi*pw.rs[2]*pw.rs[0]);
  d2tdrsxt  =  dtdrs/r->xt;
  d2tdphi2  = -2.0*dtdphi/phi;
  d2tdrsphi = -dtdrs/phi;
  d2tdxtphi =  dtdphi/r->xt;

  r->d2fdrs2     = dfdec*pw.d2edrs2 + d2fdec2*pw.dedrs*pw.dedrs + 2.0*d2fdtec*pw.dedrs*dtdrs + d2fdt2*dtdrs*dtdrs + dfdt*d2tdrs2;
  r->d2fdrsz     = dfdec*pw.d2edrsz + pw.dedrs*(d2fdec2*pw.dedz + dphidz*(d2fdtec*dtdphi + d2fdphiec))
    + dfdt*dphidz*d2tdrsphi + dtdrs*(d2fdtec*pw.dedz + dphidz*(d2fdt2*dtdphi + d2fdphit));
  r->d2fdrsxt    = dtdxt*(d2fdtec*pw.dedrs + d2fdt2*dtdrs) + dfdt*d2tdrsxt;
  r->d2fdrsxs[0] = 0.0;
  r->d2fdrsxs[1] = 0.0;
  r->d2fdz2      = dfdec*pw.d2edz2 + d2fdec2*pw.dedz*pw.dedz + dfdt*(dtdphi*d2phidz2 + d2tdphi2*dphidz*dphidz)
    + dfdphi*d2phidz2 + 2.0*dphidz*pw.dedz*(d2fdtec*dtdphi + d2fdphiec)
    + dphidz*dphidz*(d2fdt2*dtdphi*dtdphi + 2.0*d2fdphit*dtdphi + d2fdphi2);
  r->d2fdzxt     = dfdt*d2tdxtphi*dphidz + dtdxt*(d2fdtec*pw.dedz + dphidz*(d2fdt2*dtdphi + d2fdphit));
  r->d2fdzxs[0]  = 0.0;
  r->d2fdzxs[1]  = 0.0;
  r->d2fdxt2     = d2fdt2*dtdxt*dtdxt;
  r->d2fdxtxs[0] = 0.0;
  r->d2fdxtxs[1] = 0.0;
  r->d2fdxs2[0]  = 0.0;
  r->d2fdxs2[1]  = 0.0;
  r->d2fdxs2[2]  = 0.0;
}

#define func XC(gga_c_pbe_func)
#include "work_gga_c.c"

const XC(func_info_type) XC(func_info_gga_c_pbe) = {
  XC_GGA_C_PBE,
  XC_CORRELATION,
  "Perdew, Burke & Ernzerhof",
  XC_FAMILY_GGA,
  "JP Perdew, K Burke, and M Ernzerhof, Phys. Rev. Lett. 77, 3865 (1996)\n"
  "JP Perdew, K Burke, and M Ernzerhof, Phys. Rev. Lett. 78, 1396(E) (1997)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-12, 1e-32, 0.0, 1e-32,
  gga_c_pbe_init,
  NULL, NULL,
  work_gga_c,
};


const XC(func_info_type) XC(func_info_gga_c_pbe_sol) = {
  XC_GGA_C_PBE_SOL,
  XC_CORRELATION,
  "Perdew, Burke & Ernzerhof SOL",
  XC_FAMILY_GGA,
  "JP Perdew, et al, Phys. Rev. Lett. 100, 136406 (2008)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-12, 1e-32, 0.0, 1e-32,
  gga_c_pbe_init,
  NULL, NULL,
  work_gga_c,
};


const XC(func_info_type) XC(func_info_gga_c_xpbe) = {
  XC_GGA_C_XPBE,
  XC_CORRELATION,
  "Extended PBE by Xu & Goddard III",
  XC_FAMILY_GGA,
  "X Xu and WA Goddard III, J. Chem. Phys. 121, 4068 (2004)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-12, 1e-32, 0.0, 1e-32,
  gga_c_pbe_init,
  NULL, NULL,
  work_gga_c,
};

const XC(func_info_type) XC(func_info_gga_c_pbe_jrgx) = {
  XC_GGA_C_PBE_JRGX,
  XC_CORRELATION,
  "Reparametrized PBE by Pedroza, Silva & Capelle",
  XC_FAMILY_GGA,
  "LS Pedroza, AJR da Silva, and K. Capelle, Phys. Rev. B 79, 201106(R) (2009)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-12, 1e-32, 0.0, 1e-32,
  gga_c_pbe_init,
  NULL, NULL,
  work_gga_c,
};

const XC(func_info_type) XC(func_info_gga_c_rge2) = {
  XC_GGA_C_RGE2,
  XC_CORRELATION,
  "Regularized PBE",
  XC_FAMILY_GGA,
  "A Ruzsinszky, GI Csonka, and G Scuseria, J. Chem. Theory Comput. 5, 763 (2009)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-12, 1e-32, 0.0, 1e-32,
  gga_c_pbe_init,
  NULL, NULL,
  work_gga_c,
};

const XC(func_info_type) XC(func_info_gga_c_apbe) = {
  XC_GGA_C_APBE,
  XC_CORRELATION,
  "mu fixed from the semiclassical neutral atom",
  XC_FAMILY_GGA,
  "LA Constantin, E Fabiano, S Laricchia, and F Della Sala, Phys. Rev. Lett. 106, 186406 (2011)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-12, 1e-32, 0.0, 1e-32,
  gga_c_pbe_init,
  NULL, NULL,
  work_gga_c,
};

const XC(func_info_type) XC(func_info_gga_c_spbe) = {
  XC_GGA_C_SPBE,
  XC_CORRELATION,
  "PBE correlation to be used with the SSB exchange",
  XC_FAMILY_GGA,
  "M Swart, M Sola, and FM Bickelhaupt, J. Chem. Phys. 131, 094103 (2009)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  1e-12, 1e-32, 0.0, 1e-32,
  gga_c_pbe_init,
  NULL, NULL,
  work_gga_c,
};


const XC(func_info_type) XC(func_info_gga_c_vpbe) = {
  XC_GGA_C_VPBE,
  XC_CORRELATION,
  "variant PBE",
  XC_FAMILY_GGA,
  "JP Perdew, A Ruzsinszky, GI Csonka, LA Constantin, and J Sun, Phys. Rev. Lett. 103, 026403 (2009)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  1e-12, 1e-32, 0.0, 1e-32,
  gga_c_pbe_init,
  NULL, NULL,
  work_gga_c,
};

