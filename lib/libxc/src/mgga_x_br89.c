/*
 Copyright (C) 2006-2009 M.A.L. Marques

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
#include <math.h>

#include "util.h"

#define XC_MGGA_X_BR89         206 /* Becke-Roussel 89  */
#define XC_MGGA_X_BJ06         207 /* Becke & Johnson correction to Becke-Roussel 89  */
#define XC_MGGA_X_TB09         208 /* Tran & Blaha correction to Becke & Johnson  */
#define XC_MGGA_X_RPP09        209 /* Rasanen, Pittalis, and Proetto correction to Becke & Johnson  */

typedef struct{
  FLOAT c;
} mgga_x_tb09_params;

static FLOAT br89_gamma = 0.8;


static void 
mgga_x_tb09_init(XC(func_type) *p)
{
  assert(p->params == NULL);

  switch(p->info->number){
  case XC_MGGA_X_BR89:  p->func = 0; break;
  case XC_MGGA_X_BJ06:  p->func = 1; break;
  case XC_MGGA_X_TB09:  p->func = 2; break;
  case XC_MGGA_X_RPP09: p->func = 3; break;
  }

  p->params = malloc(sizeof(mgga_x_tb09_params));

  /* value of c in Becke-Johnson */
  XC(mgga_x_tb09_set_params)(p, 1.0);
}


void XC(mgga_x_tb09_set_params)(XC(func_type) *p, FLOAT c)
{
  mgga_x_tb09_params *params;

  assert(p != NULL && p->params != NULL);
  params = (mgga_x_tb09_params *) (p->params);

  params->c = c;
}

/* This code follows the inversion done in the PINY_MD package */
static FLOAT
br_newt_raph(FLOAT a, FLOAT tol,  FLOAT * res, int *ierr)
{
  int count;
  double x, f;
  static int max_iter = 50;

   *ierr = 1;
   if(a == 0.0)
     return 0.0;
   
   /* starting point */
   x = (a < 0.0) ? -1.0 : 1.0;

   count = 0;
   do {
     double arg, eee, xm2, fp;

     xm2 = x - 2.0;
     arg = 2.0*x/3.0;
     eee = exp(-arg)/a;

     f  = x*eee - xm2;
     fp = eee*(1.0 - 2.0/3.0*x) - 1.0;

     x -= f/fp;
     x  = fabs(x);

     count ++;
     *res = fabs(f);
   } while((*res > tol) && (count < max_iter));

   if(count == max_iter) *ierr=0; 
   return x;
}

static FLOAT
br_bisect(FLOAT a, FLOAT tol, int *ierr) { 
  int count; 
  FLOAT f, x, x1, x2; 
  static int max_iter = 500; 
 	 
  *ierr = 1; 
  if(a == 0.0) 
    return 0.0; 
		   
  /* starting interval */ 
  if(a > 0.0) { 
    x1 = 2.0 + tol; 
    x2 = 1.0/a + 2.0;
  }else{ 
    x2 = 2.0 - tol; 
    x1 = 0.0; 
  } 
	 	 
  /* bisection */ 
  count = 0; 
  do{ 
    FLOAT arg, eee, xm2; 
    x   = 0.5*(x1 + x2); 
    xm2 = x - 2.0; 
    arg = 2.0*x/3.0; 
    eee = exp(-arg); 
    f   = x*eee - a*xm2; 
	 	 
    if(f > 0.0) x1 = x; 
    if(f < 0.0) x2 = x; 
	 	 
    count++; 
  }while((fabs(f) > tol)  && (count < max_iter)); 
 	 
  if(count == max_iter) *ierr=0;  
  return x; 
} 
	 	 
FLOAT XC(mgga_x_br89_get_x)(FLOAT Q)
{
  FLOAT rhs, br_x, tol, res;
  int ierr;

#if SINGLE_PRECISION
  tol = 1e-6;
#else
  tol = 5e-12;
#endif

  /* build right-hand side of the non-linear equation 
     Remember we use a different definition of tau */
  rhs = 2.0/3.0*POW(M_PI, 2.0/3.0)/Q;

  br_x = br_newt_raph(rhs, tol, &res, &ierr);
  if(ierr == 0){
    br_x = br_bisect(rhs, tol, &ierr);
    if(ierr == 0){
      fprintf(stderr, 
	      "Warning: Convergence not reached in Becke-Roussel functional\n"
	      "For rhs = %e (residual = %e)\n", rhs, res);
    }
  }

  return br_x;
}

static void 
func(const XC(func_type) *pt, XC(mgga_work_x_t) *r)
{
  FLOAT Q, br_x, v_BR, dv_BRdbx, d2v_BRdbx2, dxdQ, d2xdQ2, ff, dffdx, d2ffdx2;
  FLOAT cnst, c_TB09, c_HEG, exp1, exp2;

  Q = (r->u - 4.0*br89_gamma*r->t + 0.5*br89_gamma*r->x*r->x)/6.0;
  if(abs(Q) < MIN_DENS) Q = (Q < 0) ? -MIN_DENS : MIN_DENS;

  br_x = XC(mgga_x_br89_get_x)(Q);

  cnst = -2.0*CBRT(M_PI)/X_FACTOR_C;
  exp1 = exp(br_x/3.0);
  exp2 = exp(-br_x);

  v_BR = (ABS(br_x) > pt->info->min_tau) ?
    exp1*(1.0 - exp2*(1.0 + br_x/2.0))/br_x :
    1.0/2.0 + br_x/6.0 - br_x*br_x/18.0;

  v_BR *= cnst;

  if(pt->func == 0){ /* XC_MGGA_X_BR89 */
    /* we have also to include the factor 1/2 from Eq. (9) */
    r->f = - v_BR / 2.0;
  }else{ /* XC_MGGA_X_BJ06 & XC_MGGA_X_TB09 */
    r->f = 0.0;
  }

  if(r->order < 1) return;

  if(pt->func == 0 || r->order > 1){
    dv_BRdbx = (ABS(br_x) > pt->info->min_tau) ?
      (3.0 + br_x*(br_x + 2.0) + (br_x - 3.0)/exp2) / (3.0*exp1*exp1*br_x*br_x) :
      1.0/6.0 - br_x/9.0;
    dv_BRdbx *= cnst;
    
    ff    = br_x*exp(-2.0/3.0*br_x)/(br_x - 2);
    dffdx = ff*(-2.0/3.0 + 1.0/br_x - 1.0/(br_x - 2.0));
    dxdQ  = -ff/(Q*dffdx);
  }

  if(pt->func == 0){ /* XC_MGGA_X_BR89 */
    r->dfdx = -r->x*br89_gamma*dv_BRdbx*dxdQ/12.0;
    r->dfdt =   4.0*br89_gamma*dv_BRdbx*dxdQ/12.0;
    r->dfdu =                 -dv_BRdbx*dxdQ/12.0;

  }else{
    assert(pt->params != NULL);
    c_TB09 = ((mgga_x_tb09_params *) (pt->params))->c;

    r->dfdrs = -c_TB09*v_BR;

    c_HEG  = (3.0*c_TB09 - 2.0)*SQRT(5.0/12.0)/(X_FACTOR_C*M_PI);
    
    if(pt->func == 1 || pt->func == 2) /* XC_MGGA_X_BJ0 & XC_MGGA_X_TB09 */
      r->dfdrs -= c_HEG*SQRT(2.0*r->t);
    else /* XC_MGGA_X_RPP09 */
      r->dfdrs -= c_HEG*SQRT(max(2.0*r->t - r->x*r->x/4.0, 0.0));

    r->dfdrs /= -r->rs; /* due to the definition of dfdrs */
  }

  if(r->order < 2) return;
  
  if(pt->func == 0 || r->order > 2){
    d2v_BRdbx2 = (ABS(br_x) > pt->info->min_tau) ?
      ((18.0 + (br_x - 6.0)*br_x)/exp2 - 2.0*(9.0 + br_x*(6.0 + br_x*(br_x + 2.0)))) 
      / (9.0*exp1*exp1*br_x*br_x*br_x) :
      -1.0/9.0;
    d2v_BRdbx2 *= cnst;

    d2ffdx2 = dffdx*dffdx/ff + ff*(-1.0/(br_x*br_x) + 1.0/((br_x - 2.0)*(br_x - 2.0)));
    d2xdQ2 = -(2.0*dxdQ/Q + d2ffdx2*dxdQ*dxdQ/dffdx);
  }

  if(pt->func == 0){ /* XC_MGGA_X_BR89 */
    FLOAT aux1 = d2v_BRdbx2*dxdQ*dxdQ + dv_BRdbx*d2xdQ2;

    r->d2fdx2 = -(aux1*br89_gamma*r->x*r->x/6.0 + dv_BRdbx*dxdQ)*br89_gamma/12.0;
    r->d2fdxt =  aux1*br89_gamma*br89_gamma*r->x/18.0;
    r->d2fdxu = -aux1*br89_gamma*r->x/72.0;
    r->d2fdt2 = -aux1*2.0*br89_gamma*br89_gamma/9.0;
    r->d2fdtu =  aux1*br89_gamma/18.0;
    r->d2fdu2 = -aux1/72.0;
  }else{
    
  }

}

#include "work_mgga_x.c"

const XC(func_info_type) XC(func_info_mgga_x_br89) = {
  XC_MGGA_X_BR89,
  XC_EXCHANGE,
  "Becke-Roussel 89",
  XC_FAMILY_MGGA,
  "AD Becke and MR Roussel, Phys. Rev. A 39, 3761 (1989)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  NULL, NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_x,
};

const XC(func_info_type) XC(func_info_mgga_x_bj06) = {
  XC_MGGA_X_BJ06,
  XC_EXCHANGE,
  "Becke & Johnson 06",
  XC_FAMILY_MGGA,
  "AD Becke and ER Johnson, J. Chem. Phys. 124, 221101 (2006)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_VXC,
  1e-22, 1e-32, 1e-22, 1e-22,
  mgga_x_tb09_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_x,
};

const XC(func_info_type) XC(func_info_mgga_x_tb09) = {
  XC_MGGA_X_TB09,
  XC_EXCHANGE,
  "Tran & Blaha 09",
  XC_FAMILY_MGGA,
  "F Tran and P Blaha, Phys. Rev. Lett. 102, 226401 (2009)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  mgga_x_tb09_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_x,
};

const XC(func_info_type) XC(func_info_mgga_x_rpp09) = {
  XC_MGGA_X_RPP09,
  XC_EXCHANGE,
  "Rasanen, Pittalis & Proetto 09",
  XC_FAMILY_MGGA,
  "E Rasanen, S Pittalis & C Proetto, J. Chem. Phys. 132, 044112 (2010)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_VXC,
  1e-22, 1e-22, 1e-22, 1e-22,
  mgga_x_tb09_init,
  NULL,
  NULL, NULL,        /* this is not an LDA                   */
  work_mgga_x,
};
