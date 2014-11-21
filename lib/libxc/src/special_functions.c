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
#include "util.h"

/*
  Lambert W function. 
  adapted from the Fortran code of Rickard Armiento

  Corless, Gonnet, Hare, Jeffrey, and Knuth (1996), 
         Adv. in Comp. Math. 5(4):329-359. 
*/

double lambert_w(double z)
{
  double result;
  int i;

  /* If z too low, go with the first term of the power expansion, z */
  if(z < 1.0e-20)
    return z;

  /* Inital guess */
  if(fabs(z + 1.0/M_E) > 1.45){
    /* Asymptotic expansion at 0 and Inf */
    result = log(z);
    result = result - log(result);
  }else{
    /* Series expansion about -1/e to first order */
    result = sqrt(2.0*M_E*z + 2.0) - 1.0;
  }

  /* Find result through iteration */
  for(i=0; i<10; i++){
    double p, t;

    p = exp(result);
    t = result*p - z;
    if( result != -1.0 )
      t = t/(p*(result + 1.0) - 0.5*(result + 2.0)*t/(result + 1.0));
    else
      t = 0.0;

    result = result - t;
    if(fabs(t) < (2.48e-14)*(1.0 + fabs(result)))
      return result;
  }

  /* This should never happen! */
  fprintf(stderr, "%s\n%s\n", "lambert_w: iteration limit reached",
	  "Should never happen: execution aborted");
  exit(1);
}


struct cheb_series_struct {
  double * c;   /* coefficients                */
  int order;    /* order of expansion          */
  double a;     /* lower interval point        */
  double b;     /* upper interval point        */
  int order_sp; /* effective single precision order */
};
typedef struct cheb_series_struct cheb_series;

/* cheb_eval is defined in util.h */
