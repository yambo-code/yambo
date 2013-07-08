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

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "config.h"

#ifdef HAVE_FORTRAN

#include "xc.h"
#include "string_f.h"

/* info */

CC_FORTRAN_INT XC_FC_FUNC(f90_info_number, F90_INFO_NUMBER)
     (void **info)
{
  return (CC_FORTRAN_INT) ((XC(func_info_type) *)(*info))->number;
}


CC_FORTRAN_INT XC_FC_FUNC(f90_info_kind, F90_INFO_KIND)
     (void **info)
{
  return (CC_FORTRAN_INT) ((XC(func_info_type) *)(*info))->kind;
}


void XC_FC_FUNC(f90_info_name, F90_INFO_NAME)
     (void **info, STR_F_TYPE s STR_ARG1)
{
  TO_F_STR1(((XC(func_info_type) *)(*info))->name, s);
}


CC_FORTRAN_INT  XC_FC_FUNC(f90_info_family, F90_INFO_FAMILY)
     (void **info)
{
  return (CC_FORTRAN_INT) ((XC(func_info_type) *)(*info))->family;
}

CC_FORTRAN_INT  XC_FC_FUNC(f90_info_flags, F90_INFO_FLAGS)
     (void **info)
{
  return (CC_FORTRAN_INT) ((XC(func_info_type) *)(*info))->flags;
}

void XC_FC_FUNC(f90_info_refs, F90_INFO_REFS)
  (void **info, CC_FORTRAN_INT *number, char **s, STR_F_TYPE ref_f STR_ARG1)
{
  char *c, ref[256]; /* hopefully no ref is longer than 256 characters ;) */
  XC(func_info_type) *func_p = (XC(func_info_type) *)(*info);

  if(*number == 0) *s = func_p->refs;

  if(*s == NULL || **s == '\0'){
    *number = -1;
    return;
  }

  for(c=ref; **s!='\0' && **s!='\n'; (*s)++, c++)
    *c = **s;
  *c = '\0';
  if(**s=='\n') (*s)++;

  TO_F_STR1(ref, ref_f);

  (*number)++;
  fflush(stdout);
}

/* functionals */
CC_FORTRAN_INT  XC_FC_FUNC(f90_family_from_id, F90_FAMILY_FROM_ID)
  (CC_FORTRAN_INT  *functional)
{
  return (CC_FORTRAN_INT) XC(family_from_id)((int) (*functional), NULL, NULL);
}

/* Standard initialization */
void XC_FC_FUNC(f90_func_init, F90_FUNC_INIT)
     (void **p, void **info, CC_FORTRAN_INT *functional, CC_FORTRAN_INT *nspin)
{
  XC(func_type) *func_p;
  
  func_p = (XC(func_type) *)malloc(sizeof(XC(func_type)));
  XC(func_init)(func_p, (int) (*functional), (int) (*nspin));

  *p    = (void *) func_p;
  *info = (void *)(func_p->info);
}

void XC_FC_FUNC(f90_func_end, F90_FUNC_END)
     (void **p)
{
  XC(func_end)((XC(func_type) *)(*p));
  free(*p);
  *p = NULL;
}


/* LDAs */

void XC_FC_FUNC(f90_lda, F90_LDA)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho, 
      FLOAT *zk, FLOAT *vrho, FLOAT *v2rho2, FLOAT *v3rho3)
{
  XC(lda)((XC(func_type) *)(*p), *np, rho, zk, vrho, v2rho2, v3rho3);
}

void XC_FC_FUNC(f90_lda_exc, F90_LDA_EXC)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho,
      FLOAT *zk)
{
  XC(lda)((XC(func_type) *)(*p), *np, rho, zk, NULL, NULL, NULL);
}

void XC_FC_FUNC(f90_lda_exc_vxc, F90_LDA_EXC_VXC)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho, 
      FLOAT *zk, FLOAT *vrho)
{
  XC(lda)((XC(func_type) *)(*p), *np, rho, zk, vrho, NULL, NULL);
}

void XC_FC_FUNC(f90_lda_vxc, F90_LDA_VXC)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho, 
      FLOAT *vrho)
{
  XC(lda)((XC(func_type) *)(*p), *np, rho, NULL, vrho, NULL, NULL);
}

void XC_FC_FUNC(f90_lda_fxc, F90_LDA_FXC)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho,
      FLOAT *v2rho2)
{
  XC(lda)((XC(func_type) *)(*p), *np, rho, NULL, NULL, v2rho2, NULL);
}

void XC_FC_FUNC(f90_lda_kxc, F90_LDA_KXC)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho,
      FLOAT *v3rho3)
{
  XC(lda)((XC(func_type) *)(*p), *np, rho, NULL, NULL, NULL, v3rho3);
}


/* Now come some special initializations */

/* parameter of LDA_1D */
void XC_FC_FUNC(f90_lda_x_1d_set_par, F90_LDA_X_1D_SET_PAR)
  (void **p, CC_FORTRAN_INT *interaction, FLOAT *bb)
{
  XC(lda_x_1d_set_params)((XC(func_type) *)(*p), *interaction, *bb);
}

/* parameter of Xalpha */
void XC_FC_FUNC(f90_lda_c_xalpha_set_par, F90_LDA_C_XALPHA_SET_PAR)
  (void **p, FLOAT *alpha)
{
  XC(lda_c_xalpha_set_params)((XC(func_type) *)(*p), *alpha);
}

/* relativistic option of LDA_X */
void XC_FC_FUNC(f90_lda_x_set_par, F90_LDA_X_SET_PAR)
  (void **p, CC_FORTRAN_INT *relativistic)
{
  XC(lda_x_set_params)((XC(func_type) *)(*p), *relativistic);
}

/* parameter of CSC */
void XC_FC_FUNC(f90_lda_c_1d_csc_set_par, F90_LDA_C_1D_CSC_SET_PAR)
  (void **p, CC_FORTRAN_INT *interaction, FLOAT *bb)
{
  XC(lda_c_1d_csc_set_params)((XC(func_type) *)(*p), *interaction, *bb);
}

/* parameter of PRM */
void XC_FC_FUNC(f90_lda_c_2d_prm_set_par, F90_LDA_C_2D_PRM_SET_PAR)
  (void **p, FLOAT *N)
{
  XC(lda_c_2d_prm_set_params)((XC(func_type) *)(*p), *N);
}


/* GGAs */

void XC_FC_FUNC(f90_gga, F90_GGA)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho, FLOAT *sigma, 
      FLOAT *zk, FLOAT *vrho, FLOAT *vsigma,
      FLOAT *v2rho2, FLOAT *v2rhosigma, FLOAT *v2sigma2)
{
  XC(gga)((XC(func_type) *)(*p), *np, rho, sigma, zk, vrho, vsigma, v2rho2, v2rhosigma, v2sigma2);
}

void XC_FC_FUNC(f90_gga_exc, F90_GGA_EXC)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho, FLOAT *sigma, 
      FLOAT *zk)
{
  XC(gga)((XC(func_type) *)(*p), *np, rho, sigma, zk, NULL, NULL, NULL, NULL, NULL);
}

void XC_FC_FUNC(f90_gga_exc_vxc, F90_GGA_EXC_VXC)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho, FLOAT *sigma, 
      FLOAT *zk, FLOAT *vrho, FLOAT *vsigma)
{
  XC(gga)((XC(func_type) *)(*p), *np, rho, sigma, zk, vrho, vsigma, NULL, NULL, NULL);
}

void XC_FC_FUNC(f90_gga_vxc, F90_GGA_VXC)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho, FLOAT *sigma, 
      FLOAT *vrho, FLOAT *vsigma)
{
  XC(gga)((XC(func_type) *)(*p), *np, rho, sigma, NULL, vrho, vsigma, NULL, NULL, NULL);
}

void XC_FC_FUNC(f90_gga_fxc, F90_GGA_FXC)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho, FLOAT *sigma, 
      FLOAT *v2rho2, FLOAT *v2rhosigma, FLOAT *v2sigma2)
{
  XC(gga)((XC(func_type) *)(*p), *np, rho, sigma, NULL, NULL, NULL, v2rho2, v2rhosigma, v2sigma2);
}

/* the van Leeuwen & Baerends functional is special */
void XC_FC_FUNC(f90_gga_lb_set_par, F90_GGA_LB_SET_PAR)
  (void **p, CC_FORTRAN_INT *modified, FLOAT *threshold, FLOAT *ip, FLOAT *qtot)
{
  XC(gga_lb_set_params)((XC(func_type) *)(*p), *modified, *threshold, *ip, *qtot);
}

void XC_FC_FUNC(f90_gga_lb_modified, F90_GGA_LB_MODIFIED)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho, FLOAT *sigma, FLOAT *r, FLOAT *vrho)
{
  const XC(gga_type) *gga = ((XC(func_type) *)(*p))->gga;
  assert(gga != NULL);

  XC(gga_lb_modified)(gga, *np, rho, sigma, *r, vrho);
}


void XC_FC_FUNC(f90_hyb_gga_exx_coef, F90_HYB_GGA_EXX_COEF)
   (void **p, FLOAT *coef)
{
   const XC(gga_type) *gga = ((XC(func_type) *)(*p))->gga;
   assert(gga != NULL);

   *coef = XC(hyb_gga_exx_coef)(gga);
}


/* meta-GGAs */

void XC_FC_FUNC(f90_mgga, F90_MGGA)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho, FLOAT *sigma, FLOAT *lapl, FLOAT *tau,
      FLOAT *zk, FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau,
      FLOAT *v2rho2, FLOAT *v2sigma2, FLOAT *v2lapl2, FLOAT *v2tau2,
      FLOAT *v2rhosigma, FLOAT *v2rholapl, FLOAT *v2rhotau, 
      FLOAT *v2sigmalapl, FLOAT *v2sigmatau, FLOAT *v2lapltau)
{
  XC(mgga)((XC(func_type) *)(*p), *np, rho, sigma, lapl, tau, 
	   zk, vrho, vsigma, vlapl, vtau,
	   v2rho2, v2sigma2, v2lapl2, v2tau2, v2rhosigma, v2rholapl, v2rhotau, 
	   v2sigmalapl, v2sigmatau, v2lapltau);

}

void XC_FC_FUNC(f90_mgga_exc, F90_MGGA_EXC)
     (void **p, CC_FORTRAN_INT *np, FLOAT *rho, FLOAT *sigma, FLOAT *lapl, FLOAT *tau, 
      FLOAT *zk)
{
  XC(mgga)((XC(func_type) *)(*p), *np, rho, sigma, lapl, tau, 
	   zk, NULL, NULL, NULL, NULL, 
	   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
}

void XC_FC_FUNC(f90_mgga_exc_vxc, F90_MGGA_EXC_VXC)
  (void **p, CC_FORTRAN_INT *np, FLOAT *rho, FLOAT *sigma, FLOAT *lapl, FLOAT *tau,
   FLOAT *zk, FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau)
{
  XC(mgga)((XC(func_type) *)(*p), *np, rho, sigma, lapl, tau, 
	   zk, vrho, vsigma, vlapl, vtau, 
	   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
}

void XC_FC_FUNC(f90_mgga_vxc, F90_MGGA_VXC)
  (void **p, CC_FORTRAN_INT *np, FLOAT *rho, FLOAT *sigma, FLOAT *lapl, FLOAT *tau,
   FLOAT *vrho, FLOAT *vsigma, FLOAT *vlapl, FLOAT *vtau)
{
  XC(mgga)((XC(func_type) *)(*p), *np, rho, sigma, lapl, tau, 
	   NULL, vrho, vsigma, vlapl, vtau, 
	   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
}

void XC_FC_FUNC(f90_mgga_fxc, F90_MGGA_FXC)
  (void **p, CC_FORTRAN_INT *np, FLOAT *rho, FLOAT *sigma, FLOAT *lapl, FLOAT *tau,
      FLOAT *v2rho2, FLOAT *v2sigma2, FLOAT *v2lapl2, FLOAT *v2tau2,
      FLOAT *v2rhosigma, FLOAT *v2rholapl, FLOAT *v2rhotau, 
      FLOAT *v2sigmalapl, FLOAT *v2sigmatau, FLOAT *v2lapltau)
{
  XC(mgga)((XC(func_type) *)(*p), *np, rho, sigma, lapl, tau, 
	   NULL, NULL, NULL, NULL, NULL, 
	   v2rho2, v2sigma2, v2lapl2, v2tau2, v2rhosigma, v2rholapl, v2rhotau, 
	   v2sigmalapl, v2sigmatau, v2lapltau);
}

/* parameter of TP09 */
void XC_FC_FUNC(f90_mgga_x_tb09_set_par, F90_MGGA_X_TB09_SET_PAR)
  (void **p, FLOAT *cc)
{
  XC(mgga_x_tb09_set_params)((XC(func_type) *)(*p), *cc);
}


#endif
