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
#define M_SQRTPI        1.772453850905516027298167483341145182798
#define M_SQRT3         1.732050807568877293527446341505872366943

/* Very useful macros */
#define min(x,y)  ((x<y) ? (x) : (y))
#define max(x,y)  ((x<y) ? (y) : (x))

/* some constants stolen from the GSL */
#define GSL_LOG_DBL_MIN   (-7.0839641853226408e+02)
#define GSL_LOG_DBL_MAX    7.0978271289338397e+02
#define GSL_SQRT_DBL_EPSILON   1.4901161193847656e-08
#define GSL_DBL_MIN        2.2250738585072014e-308

/* special functions */
double lambert_w(double z);

/* we define this function here, so it can be properly inlined by all compilers */
static inline double
cheb_eval(const double x, const double *cs, const int N)
{
  int i;
  double twox, b0, b1, b2;

  b2 = b1 = b0 = 0.0;

  twox = 2.0*x;
  for(i=N-1; i>=0; i--){
    b2 = b1;
    b1 = b0;
    b0 = twox*b1 - b2 + cs[i];
  }

  return 0.5*(b0 - b2);
}

double bessel_I0_scaled(const double x);
double bessel_I0(const double x);
double bessel_K0_scaled(const double x);
double bessel_K0(const double x);
double bessel_K1_scaled(const double x);
double bessel_K1(const double x);

double expint_e1_impl(const double x, const int scale);
static inline double expint_e1(const double x)         { return  expint_e1_impl( x, 0); }
static inline double expint_e1_scaled(const double x)  { return  expint_e1_impl( x, 1); }
static inline double expint_Ei(const double x)         { return -expint_e1_impl(-x, 0); }
static inline double expint_Ei_scaled(const double x)  { return -expint_e1_impl(-x, 1); }

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
void XC(rho2dzeta)(int nspin, const FLOAT *rho, FLOAT *d, FLOAT *zeta);
void XC(fast_fzeta)(const FLOAT x, const int nspin, const int order, FLOAT * fz);
void XC(mix_init)(XC(func_type) *p, int n_funcs, const int *funcs_id, const FLOAT *mix_coef);

/* LDAs */
typedef struct XC(lda_work_t) {
  int   order; /* to which order should I return the derivatives */
  FLOAT rs[3], zeta;

  FLOAT zk;
  FLOAT dedrs, dedz;                         /*  first derivatives of zk */
  FLOAT d2edrs2, d2edrsz, d2edz2;            /* second derivatives of zk */
  FLOAT d3edrs3, d3edrs2z, d3edrsz2, d3edz3; /*  third derivatives of zk */
} XC(lda_work_t);

void XC(lda_fxc_fd)(const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *fxc);
void XC(lda_kxc_fd)(const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *kxc);

void XC(lda_x_attenuation_function)(int interaction, int order, FLOAT aa, FLOAT *f, FLOAT *df, FLOAT *d2f, FLOAT *d3f);
void XC(lda_stoll)(const XC(func_type) *pw, FLOAT dens, FLOAT zeta, int order, XC(lda_work_t) res[3]);

/* direct access to the internal functions */
void XC(lda_c_hl_func)  (const XC(func_type) *p, XC(lda_work_t) *r);
void XC(lda_c_pw_func)  (const XC(func_type) *p, XC(lda_work_t) *r);
void XC(lda_c_pz_func)  (const XC(func_type) *p, XC(lda_work_t) *r);
void XC(lda_c_rc04_func)(const XC(func_type) *p, XC(lda_work_t) *r);

/* GGAs */
void work_gga_becke_init(XC(func_type) *p);

/* exchange enhancement factors: if you add one, please add it also to the gga_x_ityh.c */
void XC(gga_x_wc_enhance)  (const XC(func_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);
void XC(gga_x_pbe_enhance) (const XC(func_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);
void XC(gga_x_pw91_enhance)(const XC(func_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);
void XC(gga_x_rpbe_enhance)(const XC(func_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);
void XC(gga_x_htbs_enhance)(const XC(func_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);
void XC(gga_x_b88_enhance) (const XC(func_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);
void XC(gga_x_g96_enhance) (const XC(func_type) *p, int order, FLOAT x, FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2);

/* correlation functions */

typedef struct XC(gga_work_c_t) {
  int   order; /* to which order should I return the derivatives */

  FLOAT dens, ds[2], sigmat, sigmas[3];
  FLOAT rs, zeta, xt, xs[2];

  FLOAT f;
  FLOAT dfdrs, dfdz, dfdxt, dfdxs[2];
  FLOAT d2fdrs2, d2fdrsz, d2fdrsxt, d2fdrsxs[2], d2fdz2, 
    d2fdzxt, d2fdzxs[2], d2fdxt2, d2fdxtxs[2], d2fdxs2[3];
} XC(gga_work_c_t);

void XC(gga_c_pw91_func)(const XC(func_type) *p, XC(gga_work_c_t) *r);
void XC(gga_c_pbe_func) (const XC(func_type) *p, XC(gga_work_c_t) *r);

/* meta GGAs */
typedef struct XC(mgga_work_x_t) {
  int   order; /* to which order should I return the derivatives */
  FLOAT rs, zeta, x, t, u;

  FLOAT f;                                   /* enhancement factor       */
  FLOAT dfdrs, dfdx, dfdt, dfdu;             /* first derivatives of f  */
  FLOAT d2fdrs2, d2fdx2, d2fdt2, d2fdu2;     /* second derivatives of zk */
  FLOAT d2fdrsx, d2fdrst, d2fdrsu, d2fdxt, d2fdxu, d2fdtu;
} XC(mgga_work_x_t);

typedef struct XC(mgga_work_c_t) {
  int   order; /* to which order should I return the derivatives */

  FLOAT dens, ds[2], sigmat, sigmas[3];
  FLOAT rs, zeta, xt, xs[2], ts[2], us[2];

  FLOAT f;
  FLOAT dfdrs, dfdz, dfdxt, dfdxs[2], dfdts[2], dfdus[2];
  FLOAT d2fdrs2, d2fdrsz, d2fdrsxt, d2fdrsxs[2], d2fdrsts[2], d2fdrsus[2];
  FLOAT d2fdz2, d2fdzxt, d2fdzxs[2], d2fdzts[2], d2fdzus[2];
  FLOAT d2fdxt2, d2fdxtxs[2], d2fdxtts[2], d2fdxtus[2];
  FLOAT d2fdxs2[3], d2fxsts[4], d2fxsus[4];
  FLOAT d2dts2[3], d2fdtsus[4];
  FLOAT d2fdus2[3];
} XC(mgga_work_c_t);


void XC(pbe_c_stoll) (const XC(func_type) *pbe, int get_max, const XC(mgga_work_c_t) *in, XC(gga_work_c_t) out[3]);

void XC(mgga_series_w)(int order, int n, const FLOAT *a, FLOAT t, FLOAT *fw, FLOAT *dfwdt);
void XC(mgga_b97_func_g)(const FLOAT *cc, FLOAT gamma, FLOAT s, int order, FLOAT *g, FLOAT *dgds, FLOAT *d2gds2);
void XC(mgga_x_gvt4_func)(int order, FLOAT x, FLOAT z, FLOAT alpha, const FLOAT *d, FLOAT *h, FLOAT *dhdx, FLOAT *dhdz);

#endif
