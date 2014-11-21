/*
 Copyright (C) 2006-2009 J.I.J. Ojajarvi

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

#define XC_MGGA_X_2D_PRHG07         210   /* Pittalis, Rasanen, Helbig, Gross Exchange Functional */
#define XC_MGGA_X_2D_PRHG07_PRP10   211   /* PRGH07 with PRP10 correction */


/* Standard Newton's method */
static FLOAT
prhg_newt(FLOAT c, FLOAT tol, FLOAT * res, int *ierr)
{
  int count;
  long double y, f, yf;
  long double ey, fp, step;
  static int max_iter = 50;

   *ierr = 1;
   if(c < -1.0)
     return 0.0;
     
   count = 0;
   
   /** We need to calculate y in different ways in different regions
   because of numerical problems. (y-1)*exp(y) is very nasty at high y
   and log(y-1)+y is very nasty at low y. **/
   if (c < 4.0) {
     y = 2.0;
     do {
       ey = exp(y);
       yf = (y-1.0)*ey;
       f = yf - c;
       fp = ey*y;
       
       step = f/fp;
       
       y -= fabs(step) < 1.0 ? step : (step)/fabs(step);
       y  = fabs(y);
       
       count ++;
       *res = fabs(f);
     } while((*res > tol) && (count < max_iter));
   }
   else {
     y = 6.0;
     c = log(c);
     do {
       yf = log(y-1.0)+y;
       f = yf - c;
       fp = 1.0 + 1.0/(-1.0 + y);
       
       step = f/fp;
       
       y -= fabs(step) < 1.0 ? step : (step)/fabs(step);
       y  = fabs(y);
       
       count ++;
       *res = fabs(f);
     } while((*res > tol) && (count < max_iter));
   }
   
   if(count == max_iter) *ierr=0;
   
   return y;
}

FLOAT XC(mgga_x_2d_prhg_get_y)(FLOAT C)
{
  FLOAT rhs, res, y, tol;
  int ierr;

#if SINGLE_PRECISION
  tol = 1e-6;
#else
  tol = 5e-12;
#endif

  rhs = C/M_PI;

  y = prhg_newt(rhs, tol, &res, &ierr);
  if(ierr == 0){
    fprintf(stderr, 
	    "Warning: Convergence not reached in PRHG functional\n"
	    "For c = %e (residual = %e)\n", C, res);
  }

  return y;
}

static void 
func(const XC(func_type) *p, XC(mgga_work_x_t) *r)
{
  FLOAT y;
  FLOAT v_PRHG, C;

  assert(p != NULL);
  
  C = 0.25*(r->u - 4.0*r->t + 0.5*r->x*r->x);
  
  y = XC(mgga_x_2d_prhg_get_y)(C);
  
  v_PRHG = M_PI*bessel_I0(y/2.0);
  v_PRHG /= X_FACTOR_2D_C;

  if (p->info->number == XC_MGGA_X_2D_PRHG07) {
    r->dfdrs = v_PRHG*(1.0 / 3.0); // This factor is here in order to get the correct potential through work_mgga_x.c
    r->f = v_PRHG / 2.0;
  }else if (p->info->number == XC_MGGA_X_2D_PRHG07_PRP10) {
    r->dfdrs = (v_PRHG - ((2.0*M_SQRT2)/(3.0*M_PI))*SQRT(max(2.0*r->t - 0.25*r->x*r->x, 0.0))/X_FACTOR_2D_C)*(1.0 / 3.0);
    r->f = r->dfdrs * (3.0 / 2.0);
  }

  r->dfdrs /= -r->rs; /* due to the definition of dfdrs */
  
  return;
}
#define XC_DIMENSIONS 2
#include "work_mgga_x.c"

const XC(func_info_type) XC(func_info_mgga_x_2d_prhg07) = {
  XC_MGGA_X_2D_PRHG07,
  XC_EXCHANGE,
  "Pittalis-Rasanen-Helbig-Gross 2007",
  XC_FAMILY_MGGA,
  "S. Pittalis, E. Rasanen, N. Helbig, and E. K. U. Gross, Phys. Rev. B 76, 235314 (2007)",
  XC_FLAGS_2D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  NULL, NULL, 
  NULL, NULL,
  work_mgga_x,
};

const XC(func_info_type) XC(func_info_mgga_x_2d_prhg07_prp10) = {
  XC_MGGA_X_2D_PRHG07_PRP10,
  XC_EXCHANGE,
  "PRHG07 with Pittalis-Rasanen-Proetto 2010 correction",
  XC_FAMILY_MGGA,
  "S. Pittalis, E. Rasanen, N. Helbig, and E. K. U. Gross, Phys. Rev. B 76, 235314 (2007)\n"
  "S. Pittalis, E. Rasanen, C.R. Proetto, Phys. Rev. B. 81, 115108 (2010)",
  XC_FLAGS_2D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC,
  MIN_DENS, MIN_GRAD, MIN_TAU, MIN_ZETA,
  NULL,
  NULL,
  NULL, NULL,
  work_mgga_x,
};

