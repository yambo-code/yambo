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

#define XC_LDA_X         1   /* Exchange                     */
#define XC_LDA_C_XALPHA  6   /* Slater Xalpha                */

/*  
    Slater's Xalpha functional (Exc = alpha Ex)
    
    Note: this is to be added to the exchange

    This correlation functional, added to the exchange functional, produces
    a total exchange-correlation functional, Exc, equal to 3/2 * alpha * Ex 
    Setting alpha equal to one gives the *usual* Slater Xalpha functional,
    whereas alpha equal to 2/3 just leaves the exchange functional unchanged.
*/

/* Relativistic corrections */
/*  A. K. Rajagopal, J. Phys. C 11, L943 (1978).
    A. H. MacDonald and S. H. Vosko, J. Phys. C 12, 2977 (1979).
    E. Engel, S. Keller, A. Facco Bonetti, H. Mueller, and R. M. Dreizler, Phys. Rev. A 52, 2750 (1995).
*/

typedef struct{
  FLOAT alpha;         /* parameter for Xalpha functional */
  int relativistic;  /* use the relativistic version of the functional or not */
} XC(lda_x_params);

static void 
lda_x_init(void *p_)
{
  XC(lda_type) *p = (XC(lda_type) *)p_;

  assert(p->params == NULL);
  p->params = malloc(sizeof(XC(lda_x_params)));

  /* exchange is equal to xalpha with a parameter of 4/3 */
  XC(lda_x_set_params_)(p, 4.0/3.0, XC_NON_RELATIVISTIC);
}

static void 
lda_c_xalpha_init(void *p_)
{
  XC(lda_type) *p = (XC(lda_type) *)p_;

  assert(p->params == NULL);
  p->params = malloc(sizeof(XC(lda_x_params)));

  /* This gives the usual Xalpha functional */
  XC(lda_x_set_params_)(p, 1.0, XC_NON_RELATIVISTIC);
}

static void 
lda_x_end(void *p_)
{
  XC(lda_type) *p = (XC(lda_type) *)p_;

  assert(p->params != NULL);
  free(p->params);
  p->params = NULL;
}


void 
XC(lda_c_xalpha_set_params)(XC(func_type) *p, FLOAT alpha)
{
  assert(p != NULL && p->lda != NULL);
  XC(lda_x_set_params_)(p->lda, alpha, XC_NON_RELATIVISTIC);
}

void 
XC(lda_x_set_params)(XC(func_type) *p, int relativistic)
{
  assert(p != NULL && p->lda != NULL);
  XC(lda_x_set_params_)(p->lda, 4.0/3.0, relativistic);
}

void 
XC(lda_x_set_params_)(XC(lda_type) *p, FLOAT alpha, int relativistic)
{
  XC(lda_x_params) *params;

  assert(p->params != NULL);
  params = (XC(lda_x_params) *) (p->params);

  params->alpha = 1.5*alpha - 1.0;
  params->relativistic = relativistic;
}


static inline void 
func(const XC(lda_type) *p, XC(lda_rs_zeta) *r)
{
  FLOAT ax, fz, dfz, d2fz, d3fz;
  FLOAT beta, beta2, beta4, beta6, f1, f1_3, f1_5, f2, f3;
  FLOAT phi, dphi, d2phi, d3phi, dphidbeta, d2phidbeta2, d3phidbeta3, dbetadrs, d2betadrs2, d3betadrs3;
  FLOAT zk_nr, dedrs_nr, dedz_nr, d2edrs2_nr, d2edrsz_nr, d2edz2_nr;
  XC(lda_x_params) *params;

  assert(p->params != NULL);
  params = (XC(lda_x_params) *) (p->params);  

  ax = -params->alpha*0.458165293283142893475554485052; /* -alpha * 3/4*POW(3/(2*M_PI), 2/3) */
  
  r->zk = ax/r->rs[1];

  if(p->nspin == XC_POLARIZED){
    fz  = 0.5*(POW(1.0 + r->zeta,  4.0/3.0) + POW(1.0 - r->zeta,  4.0/3.0));
    r->zk *= fz;
  }

  if(params->relativistic == XC_RELATIVISTIC){
    beta   = CBRT(9.0*M_PI/4.0)/(r->rs[1]*M_C);
    beta2  = beta*beta;
    f1     = SQRT(1.0 + beta2);
    f2     = asinh(beta);
    f3     = f1/beta - f2/beta2;
    phi    = 1.0 - 3.0/2.0*f3*f3;

    zk_nr  = r->zk;
    r->zk *= phi;
  }

  if(r->order < 1) return;
  
  r->dedrs = -ax/r->rs[2];

  if(p->nspin == XC_POLARIZED){
    dfz = 2.0/3.0*(CBRT(1.0 + r->zeta) - CBRT(1.0 - r->zeta));

    r->dedrs *= fz;
    r->dedz   = ax/r->rs[1]*dfz;
  }

  if(params->relativistic == XC_RELATIVISTIC){
    beta4 = beta2*beta2;
    dphidbeta = 6.0/(beta4*beta)*(beta2 - beta*(2 + beta2)*f2/f1 + f2*f2);
    dbetadrs = -beta/r->rs[1];

    dedrs_nr = r->dedrs;
    dphi     = dphidbeta*dbetadrs;

    r->dedrs = r->dedrs*phi + zk_nr*dphi;
    if(p->nspin == XC_POLARIZED){
      dedz_nr = r->dedz;
      r->dedz = r->dedz*phi;
    }
  }


  if(r->order < 2) return;
    
  r->d2edrs2 = 2.0*ax/(r->rs[1]*r->rs[2]);

  if(p->nspin == XC_POLARIZED){
    if(ABS(r->zeta) == 1.0)
      d2fz = FLT_MAX;
    else
      d2fz = 2.0/9.0*(POW(1.0 + r->zeta,  -2.0/3.0) + POW(1.0 - r->zeta,  -2.0/3.0));
    
    r->d2edrs2 *= fz;
    r->d2edrsz = -ax/r->rs[2]*dfz;
    r->d2edz2  =  ax/r->rs[1]*d2fz;
  }

  if(params->relativistic == XC_RELATIVISTIC){
    f1_3 = f1*f1*f1;
    d2phidbeta2 = -(beta2*f1*(5.0 + 4.0*beta2) - 
		    beta*(10.0 + 14.0*beta2 + 3.0*beta4)*f2 +
		    5.0*f1_3*f2*f2) * 6.0/(beta4*beta2*f1_3);
    d2betadrs2 = -2.0*dbetadrs/r->rs[1];

    d2edrs2_nr = r->d2edrs2;
    d2phi      = d2phidbeta2*dbetadrs*dbetadrs + dphidbeta*d2betadrs2;

    r->d2edrs2 = r->d2edrs2*phi + 2.0*dedrs_nr*dphi + zk_nr*d2phi;
    if(p->nspin == XC_POLARIZED){
      d2edz2_nr  = r->d2edz2;
      d2edrsz_nr = r->d2edrsz;

      r->d2edrsz = r->d2edrsz*phi + dedz_nr*dphi;
      r->d2edz2  = r->d2edz2*phi;
      
    }
  }

  if(r->order < 3) return;

  r->d3edrs3 = -6.0*ax/(r->rs[2]*r->rs[2]);

  if(p->nspin == XC_POLARIZED){
    if(ABS(r->zeta) == 1.0)
      d3fz = FLT_MAX;
    else
      d3fz = -4.0/27.0*(POW(1.0 + r->zeta,  -5.0/3.0) - POW(1.0 - r->zeta,  -5.0/3.0));

    r->d3edrs3 *= fz;
    r->d3edrs2z = 2.0*ax/(r->rs[1]*r->rs[2])*dfz;
    r->d3edrsz2 =    -ax/r->rs[2]           *d2fz;
    r->d3edz3   =     ax/r->rs[1]           *d3fz;
  }

  if(params->relativistic == XC_RELATIVISTIC){
    beta6 = beta4*beta2;
    f1_5  = f1_3*f1*f1;

    d3phidbeta3 = (beta2*f1*(30.0 + 52.0*beta2 + 19.0*beta4) -
		   beta*f2*(60.0 + 142.0*beta2 + 97.0*beta4 + 12.0*beta6) +
		   30.0*f1_5*f2*f2) * 6.0/(beta6*beta*f1_5);
    d3betadrs3 = -3.0*d2betadrs2/r->rs[1];

    d3phi = d3phidbeta3*dbetadrs*dbetadrs*dbetadrs + 3.0*d2phidbeta2*dbetadrs*d2betadrs2 +
      dphidbeta*d3betadrs3;

    r->d3edrs3 = r->d3edrs3*phi + 3.0*d2edrs2_nr*dphi + 3.0*dedrs_nr*d2phi + zk_nr*d3phi;
    if(p->nspin == XC_POLARIZED){
      r->d3edrs2z = r->d3edrs2z*phi + 2.0*d2edrsz_nr*dphi + dedz_nr*d2phi;
      r->d3edrsz2 = r->d3edrsz2*phi + d2edz2_nr*dphi;
      r->d3edz3   = r->d3edz3*phi;
    }
  }

}

#include "work_lda.c"

const XC(func_info_type) XC(func_info_lda_x) = {
  XC_LDA_X,
  XC_EXCHANGE,
  "Slater exchange",
  XC_FAMILY_LDA,
  "PAM Dirac, Proceedings of the Cambridge Philosophical Society 26, 376 (1930)\n"
  "F Bloch, Zeitschrift fuer Physik 57, 545 (1929)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  lda_x_init,
  lda_x_end,
  work_lda
};

const XC(func_info_type) XC(func_info_lda_c_xalpha) = {
  XC_LDA_C_XALPHA,
  XC_CORRELATION,
  "Slater's Xalpha",
  XC_FAMILY_LDA,
  "JC Slater, Phys. Rev. 81, 385 (1951)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, 0.0, 0.0, 0.0,
  lda_c_xalpha_init,
  lda_x_end,
  work_lda
};

