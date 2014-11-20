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

#ifndef _XC_H
#define _XC_H

#ifdef __cplusplus
extern "C" {
#endif

#include "xc_version.h"
#include "xc_config.h"
  
#define XC_UNPOLARIZED          1
#define XC_POLARIZED            2

#define XC_NON_RELATIVISTIC     0
#define XC_RELATIVISTIC         1

#define XC_EXCHANGE             0
#define XC_CORRELATION          1
#define XC_EXCHANGE_CORRELATION 2
#define XC_KINETIC              3

#define XC_FAMILY_UNKNOWN      -1
#define XC_FAMILY_LDA           1
#define XC_FAMILY_GGA           2
#define XC_FAMILY_MGGA          4
#define XC_FAMILY_LCA           8
#define XC_FAMILY_OEP          16
#define XC_FAMILY_HYB_GGA      32
#define XC_FAMILY_HYB_MGGA     64

/* flags that can be used in info.flags */
#define XC_FLAGS_HAVE_EXC         (1 <<  0) /*    1 */
#define XC_FLAGS_HAVE_VXC         (1 <<  1) /*    2 */
#define XC_FLAGS_HAVE_FXC         (1 <<  2) /*    4 */
#define XC_FLAGS_HAVE_KXC         (1 <<  3) /*    8 */
#define XC_FLAGS_HAVE_LXC         (1 <<  4) /*   16 */
#define XC_FLAGS_1D               (1 <<  5) /*   32 */
#define XC_FLAGS_2D               (1 <<  6) /*   64 */
#define XC_FLAGS_3D               (1 <<  7) /*  128 */
#define XC_FLAGS_HYB_CAM          (1 <<  8) /*  256 */
#define XC_FLAGS_STABLE           (1 <<  9) /*  512 */
#define XC_FLAGS_DEVELOPMENT      (1 << 10) /* 1024 */

#define XC_TAU_EXPLICIT         0
#define XC_TAU_EXPANSION        1

  /* This value was redefined as XC_GGA_X_LB, we define it here to
     keep compatibility. */
#define XC_GGA_XC_LB          160

void XC(version)(int *major, int *minor);

struct XC(func_type);

typedef struct{
  int   number;   /* indentifier number */
  int   kind;     /* XC_EXCHANGE, XC_CORRELATION, or XC_EXCHANGE_CORRELATION */

  char *name;     /* name of the functional, e.g. "PBE" */
  int   family;   /* type of the functional, e.g. XC_FAMILY_GGA */
  char *refs;     /* references                       */

  int   flags;    /* see above for a list of possible flags */

  FLOAT min_dens;
  FLOAT min_grad;
  FLOAT min_tau;
  FLOAT min_zeta;

  void (*init)(struct XC(func_type) *p);
  void (*end) (struct XC(func_type) *p);
  void (*lda) (const struct XC(func_type) *p, int np, 
	       const FLOAT *rho, 
	       FLOAT *zk, FLOAT *vrho, FLOAT *v2rho2, FLOAT *v3rho3);
  void (*gga) (const struct XC(func_type) *p, int np, 
	       const FLOAT *rho, const FLOAT *sigma, 
	       FLOAT *zk, FLOAT *vrho, FLOAT *vsigma,
	       FLOAT *v2rho2, FLOAT *v2rhosigma, FLOAT *v2sigma2);
  void (*mgga)(const struct XC(func_type) *p, int np, 
	       const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl_rho, const FLOAT *tau,
	       FLOAT *zk, FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl_rho, FLOAT *vtau,
	       FLOAT *v2rho2, FLOAT *v2sigma2, FLOAT *v2tau2, FLOAT *v2lapl2,
	       FLOAT *v2rhosigma, FLOAT *v2rhotau, FLOAT *v2rholapl, 
	       FLOAT *v2sigmatau, FLOAT *v2sigmalapl, FLOAT *v2taulapl);
} XC(func_info_type);


struct XC(func_type){
  const XC(func_info_type) *info;       /* all the information concerning this functional */
  int nspin;                            /* XC_UNPOLARIZED or XC_POLARIZED  */
  
  int n_func_aux;                       /* how many auxiliary functions we need */
  struct XC(func_type) **func_aux;      /* most GGAs are based on a LDA or other GGAs  */
  FLOAT *mix_coef;                      /* coefficients for the mixing */

  FLOAT cam_omega;                      /* range separation parameter for hybrids */
  FLOAT cam_alpha;                      /* the Hartree-Fock mixing parameter for the hybrids */
  FLOAT cam_beta;                       /* idem, but for the screened hybrid */

  int func;                             /* Shortcut in case of several functionals sharing the same interface */
  int n_rho, n_sigma, n_tau, n_lapl;    /* spin dimensions of the arrays */
  int n_zk;
  int n_vrho, n_vsigma, n_vtau, n_vlapl;
  int n_v2rho2, n_v2sigma2, n_v2tau2, n_v2lapl2,
    n_v2rhosigma, n_v2rhotau, n_v2rholapl, 
    n_v2sigmatau, n_v2sigmalapl, n_v2lapltau;
  int n_v3rho3;
  
  void *params;                         /* this allows us to fix parameters in the functional */
};

typedef struct XC(func_type) XC(func_type);

/* functionals */
int   XC(functional_get_number)(const char *name);
char *XC(functional_get_name)(int number);
int   XC(family_from_id)(int id, int *family, int *number);
int   XC(func_init)(XC(func_type) *p, int functional, int nspin);
void  XC(func_end)(XC(func_type) *p);

#include "xc_funcs.h"

int  XC(lda_init)(XC(func_type) *p, const XC(func_info_type) *info, int nspin);
void XC(lda_end) (XC(func_type) *p);

void XC(lda)        (const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *zk, FLOAT *vrho, FLOAT *v2rho2, FLOAT *v3rho3);
void XC(lda_exc)    (const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *zk);
void XC(lda_exc_vxc)(const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *zk, FLOAT *vrho);
void XC(lda_vxc)    (const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *vrho);
void XC(lda_fxc)    (const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *v2rho2);
void XC(lda_kxc)    (const XC(func_type) *p, int np, const FLOAT *rho, FLOAT *v3rho3);

void XC(lda_x_set_params)        (XC(func_type) *p, FLOAT alpha, int relativistic, FLOAT omega);
void XC(lda_x_1d_set_params)     (XC(func_type) *p, int interaction, FLOAT bb);
void XC(lda_c_1d_csc_set_params) (XC(func_type) *p, int interaction, FLOAT bb);
void XC(lda_c_xalpha_set_params) (XC(func_type) *p, FLOAT alpha);
void XC(lda_c_2d_prm_set_params) (XC(func_type) *p, FLOAT N);
void XC(lda_c_vwn_set_params)    (XC(func_type) *p, int spin_interpolation);


int  XC(gga_init)(XC(func_type) *p, const XC(func_info_type) *info, int nspin);
void XC(gga_end) (XC(func_type) *p);
void XC(gga)     (const XC(func_type) *p, int np, const FLOAT *rho, const FLOAT *sigma, 
		  FLOAT *zk, FLOAT *vrho, FLOAT *vsigma,
		  FLOAT *v2rho2, FLOAT *v2rhosigma, FLOAT *v2sigma2);
void XC(gga_exc)(const XC(func_type) *p, int np, const FLOAT *rho, const FLOAT *sigma, 
		 FLOAT *zk);
void XC(gga_exc_vxc)(const XC(func_type) *p, int np, const FLOAT *rho, const FLOAT *sigma,
		     FLOAT *zk, FLOAT *vrho, FLOAT *vsigma);
void XC(gga_vxc)(const XC(func_type) *p, int np, const FLOAT *rho, const FLOAT *sigma,
		 FLOAT *vrho, FLOAT *vsigma);
void XC(gga_fxc)(const XC(func_type) *p, int np, const FLOAT *rho, const FLOAT *sigma,
		 FLOAT *v2rho2, FLOAT *v2rhosigma, FLOAT *v2sigma2);

void XC(gga_lb_modified)  (const XC(func_type) *p, int np, const FLOAT *rho, const FLOAT *sigma, 
			   FLOAT r, FLOAT *vrho);

void XC(gga_x_b88_set_params)     (XC(func_type) *p, FLOAT beta, FLOAT gamma);
void XC(gga_x_pbe_set_params)     (XC(func_type) *p, FLOAT kappa, FLOAT mu);
void XC(gga_c_pbe_set_params)     (XC(func_type) *p, FLOAT beta);
void XC(gga_x_pw91_set_params)    (XC(func_type) *p, FLOAT a, FLOAT b, FLOAT c, FLOAT d, FLOAT f, FLOAT alpha, FLOAT expo);
void XC(gga_x_pw91_set_params2)   (XC(func_type) *p, FLOAT bt, FLOAT alpha, FLOAT expo);
void XC(gga_x_rpbe_set_params)    (XC(func_type) *p, FLOAT kappa, FLOAT mu);
void XC(gga_x_optx_set_params)    (XC(func_type) *p, FLOAT a, FLOAT b, FLOAT gamma);
void XC(gga_c_lyp_set_params)     (XC(func_type) *p, FLOAT A, FLOAT B, FLOAT c, FLOAT d);
void XC(gga_lb_set_params)        (XC(func_type) *p, int modified, FLOAT threshold, FLOAT ip, FLOAT qtot);
void XC(gga_k_tflw_set_params)    (XC(func_type) *p, FLOAT gamma, FLOAT lambda, FLOAT N);
void XC(gga_x_2d_b88_set_params)  (XC(func_type) *p, FLOAT beta);
void XC(gga_x_wpbeh_set_params)   (XC(func_type) *p, FLOAT omega);
void XC(gga_x_hjs_set_params)     (XC(func_type) *p, FLOAT omega);
void XC(gga_x_ityh_set_params)    (XC(func_type) *p, int func_id, FLOAT omega);
void XC(gga_x_ssb_sw_set_params)  (XC(func_type) *p, FLOAT A, FLOAT B, FLOAT C, FLOAT D, FLOAT E);
void XC(gga_x_kt_set_params)      (XC(func_type) *p, FLOAT gamma, FLOAT delta);

FLOAT XC(hyb_exx_coef)(const XC(func_type) *p);
void  XC(hyb_cam_coef)(const XC(func_type) *p, FLOAT *omega, FLOAT *alpha, FLOAT *beta);

void XC(hyb_gga_xc_hse_set_params) (XC(func_type) *p, FLOAT alpha, FLOAT omega);
void XC(hyb_gga_xc_pbeh_set_params)(XC(func_type) *p, FLOAT alpha);

/* the meta-GGAs */
int  XC(mgga_init)(XC(func_type) *p, const XC(func_info_type) *info, int nspin);
void XC(mgga_end) (XC(func_type) *p);
void XC(mgga)        (const XC(func_type) *p, int np,
		      const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl, const FLOAT *tau,
		      FLOAT *zk, FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau,
		      FLOAT *v2rho2, FLOAT *v2sigma2, FLOAT *v2lapl2, FLOAT *v2tau2,
		      FLOAT *v2rhosigma, FLOAT *v2rholapl, FLOAT *v2rhotau, 
		      FLOAT *v2sigmalapl, FLOAT *v2sigmatau, FLOAT *v2lapltau);
void XC(mgga_exc)    (const XC(func_type) *p, int np,  
		      const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl, const FLOAT *tau, 
		      FLOAT *zk);
void XC(mgga_exc_vxc)(const XC(func_type) *p, int np, 
		      const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl, const FLOAT *tau,
		      FLOAT *zk, FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau);
void XC(mgga_vxc)    (const XC(func_type) *p, int np,
		      const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl, const FLOAT *tau,
		      FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau);
void XC(mgga_fxc)    (const XC(func_type) *p, int np, 
		      const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl, const FLOAT *tau,
		      FLOAT *v2rho2, FLOAT *v2sigma2, FLOAT *v2lapl2, FLOAT *v2tau2,
		      FLOAT *v2rhosigma, FLOAT *v2rholapl, FLOAT *v2rhotau, 
		      FLOAT *v2sigmalapl, FLOAT *v2sigmatau, FLOAT *v2lapltau);

void XC(mgga_x_tb09_set_params)(XC(func_type) *p, FLOAT c);
void XC(mgga_x_tpss_set_params)(XC(func_type) *p, FLOAT b, FLOAT c, FLOAT e, FLOAT kappa, FLOAT mu);
void XC(mgga_c_bc95_set_params)(XC(func_type) *p, FLOAT css, FLOAT copp);
void XC(mgga_c_pkzb_set_params)(XC(func_type) *p, FLOAT beta, FLOAT d, FLOAT C0_0, FLOAT C0_1, FLOAT C0_2, FLOAT C0_3);

/* Functionals that are defined as mixtures of others */
void XC(mix_func)
  (const XC(func_type) *func, int np,
   const FLOAT *rho, const FLOAT *sigma, const FLOAT *lapl, const FLOAT *tau,
   FLOAT *zk, FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau,
   FLOAT *v2rho2, FLOAT *v2sigma2, FLOAT *v2lapl2, FLOAT *v2tau2,
   FLOAT *v2rhosigma, FLOAT *v2rholapl, FLOAT *v2rhotau, 
   FLOAT *v2sigmalapl, FLOAT *v2sigmatau, FLOAT *v2lapltau);
  
#ifdef __cplusplus
}
#endif

#endif

