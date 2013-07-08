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

#ifndef _LDA_H
#define _LDA_H

#include <math.h>
#include <float.h>
#include "xc_config.h"

/* If strict ANSI, then some useful macros are not defined */
#if defined(__STRICT_ANSI__)
# define M_E            2.7182818284590452354   /* e */
# define M_PI           3.14159265358979323846  /* pi */
# define M_SQRT2        1.41421356237309504880  /* sqrt(2) */
double asinh (double x);
float  asinhf(float  x);
#endif

#define M_CBRT2         1.259921049894873164767210607278228350570

/* Very useful macros */
#define min(x,y)  ((x<y) ? (x) : (y))
#define max(x,y)  ((x<y) ? (y) : (x))

/* special functions */
double lambert_w(double z);
double bessi0(double x);
double bessi1(double x);
double bessk0(double x);
double bessk1(double x);
double expint(double x);

/* integration */
typedef void integr_fn(FLOAT *x, int n, void *ex);
FLOAT integrate(integr_fn func, void *ex, FLOAT a, FLOAT b);
void rdqagse(integr_fn f, void *ex, FLOAT *a, FLOAT *b, 
	     FLOAT *epsabs, FLOAT *epsrel, int *limit, FLOAT *result,
	     FLOAT *abserr, int *neval, int *ier, FLOAT *alist__,
	     FLOAT *blist, FLOAT *rlist, FLOAT *elist, int *iord, int *last);
  
typedef struct XC(functional_key_t) {
  char name[256];
  int  number;
} XC(functional_key_t);


#define M_C 137.0359996287515 /* speed of light */

#define RS(x)          (CBRT((3.0/(4*M_PI*x))))
#define X_FACTOR_C     0.9305257363491000250020102180716672510262     /* 3/8*cur(3/pi)*4^(2/3) */
#define X_FACTOR_2D_C  1.504505556127350098528211870828726895584      /* 8/(3*sqrt(pi))        */
#define K_FACTOR_C     4.557799872345597137288163759599305358515      /* 3/10*(6*pi^2)^(2/3)   */
#define X2S            0.1282782438530421943003109254455883701296     /* 1/(2*(6*pi^2)^(1/3))  */
#define X2S_2D         0.141047395886939071                           /* 1/(2*(4*pi)^(1/2)     */
#define FZETAFACTOR    0.519842099789746380
#define FZETA(x)       ((POW(1.0 + (x),  4.0/3.0) + POW(1.0 - (x),  4.0/3.0) - 2.0)/FZETAFACTOR)
#define DFZETA(x)      ((CBRT(1.0 + (x)) - CBRT(1.0 - (x)))*(4.0/3.0)/FZETAFACTOR)
#define D2FZETA(x)     ((4.0/9.0)/FZETAFACTOR)* \
  (ABS(x)==1.0 ? (FLT_MAX) : (pow(1.0 + (x), -2.0/3.0) + pow(1.0 - (x), -2.0/3.0)))
#define D3FZETA(x)     (-(8.0/27.0)/FZETAFACTOR)* \
  (ABS(x)==1.0 ? (FLT_MAX) : (pow(1.0 + (x), -5.0/3.0) - pow(1.0 - (x), -5.0/3.0)))

#define MIN_DENS             5.0e-13
#define MIN_GRAD             5.0e-13
#define MIN_TAU              5.0e-13
#define MIN_ZETA             5.0e-13

#include "xc.h"

/* The following inlines confuse the xlc compiler */
/* inline */ void XC(rho2dzeta)(int nspin, const FLOAT *rho, FLOAT *d, FLOAT *zeta);
/* inline */ void XC(fast_fzeta)(const FLOAT x, const int nspin, const int order, FLOAT * fz);

/* LDAs */
typedef struct XC(lda_rs_zeta) {
  int   order; /* to which order should I return the derivatives */
  FLOAT rs[3], zeta;

  FLOAT zk;
  FLOAT dedrs, dedz;                         /*  first derivatives of zk */
  FLOAT d2edrs2, d2edrsz, d2edz2;            /* second derivatives of zk */
  FLOAT d3edrs3, d3edrs2z, d3edrsz2, d3edz3; /*  third derivatives of zk */
} XC(lda_rs_zeta);

void XC(lda_fxc_fd)(const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *fxc);
void XC(lda_kxc_fd)(const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *kxc);

/* internal versions of set_params routines */
void XC(lda_x_1d_set_params_)     (XC(lda_type) *p, int interaction, FLOAT bb);
void XC(lda_c_1d_csc_set_params_) (XC(lda_type) *p, int interaction, FLOAT bb);
void XC(lda_x_set_params_)        (XC(lda_type) *p, FLOAT alpha, int relativistic);
void XC(lda_c_2d_prm_set_params_) (XC(lda_type) *p, FLOAT N);
void XC(lda_c_vwn_set_params_)    (XC(lda_type) *p, int spin_interpolation);

/* direct access to the internal functions */
void XC(lda_c_hl_func)(const XC(lda_type) *p, XC(lda_rs_zeta) *r);
void XC(lda_c_pw_func)(const XC(lda_type) *p, XC(lda_rs_zeta) *r);
void XC(lda_c_pz_func)(const XC(lda_type) *p, XC(lda_rs_zeta) *r);

/* GGAs */
void XC(gga_x_wc_enhance)  (const XC(gga_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);
void XC(gga_x_pbe_enhance) (const XC(gga_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);
void XC(gga_x_rpbe_enhance)(const XC(gga_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);
void XC(gga_x_htbs_enhance)(const XC(gga_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);

void XC(gga_init_mix)(XC(gga_type) *p, int n_funcs, const int *funcs_id, const FLOAT *mix_coef);

/* internal versions of set_params routines */
void XC(gga_x_b88_set_params_)  (XC(gga_type) *p, FLOAT beta, FLOAT gamma);
void XC(gga_x_pbe_set_params_)  (XC(gga_type) *p, FLOAT kappa, FLOAT mu);
void XC(gga_x_rpbe_set_params_) (XC(gga_type) *p, FLOAT kappa, FLOAT mu);
void XC(gga_x_optx_set_params_) (XC(gga_type) *p, FLOAT a, FLOAT b, FLOAT gamma);
void XC(gga_c_lyp_set_params_)  (XC(gga_type) *p, FLOAT A, FLOAT B, FLOAT c, FLOAT d);
void XC(gga_lb_set_params_)     (XC(gga_type) *p, int modified, FLOAT threshold, FLOAT ip, FLOAT qtot);
void XC(gga_k_tflw_set_params_) (XC(gga_type) *p, FLOAT gamma, FLOAT lambda, FLOAT N);


/* meta GGAs */
typedef struct XC(work_mgga_x_params) {
  int   order; /* to which order should I return the derivatives */
  FLOAT rs, zeta, x, t, u;

  FLOAT f;                                   /* enhancement factor       */
  FLOAT dfdrs, dfdx, dfdt, dfdu;             /* first derivatives of f  */
  FLOAT d2fdrs2, d2fdx2, d2fdt2, d2fdu2;     /* second derivatives of zk */
  FLOAT d2fdrsx, d2fdrst, d2fdrsu, d2fdxt, d2fdxu, d2fdtu;
} XC(work_mgga_x_params);


/* direct access to the internal functions */
void XC(mgga_x_gvt4_func)(int order, FLOAT x, FLOAT z, FLOAT alpha, const FLOAT *d, 
			  FLOAT *h, FLOAT *dhdx, FLOAT *dhdz);

/* internal versions of set_params routines */
void XC(mgga_x_tb09_set_params_)(XC(mgga_type) *p, FLOAT c);



#endif
