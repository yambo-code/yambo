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


/*
  BESSI0: modified Bessel function I0(x).  See A&S 9.8.1, 9.8.2.
  from nemo_3.0.7/src/kernel/misc/besselfunc.c
*/

double bessi0(double x)
{
  double t, tt, ti, u;
  
  t = ABS(x) / 3.75;
  tt = t * t;
  if (tt < 1.0) {
    u = 1.0 +
      tt * (3.5156229 +
	    tt * (3.0899424 +
		  tt * (1.2067492 +
			tt * (0.2659732 +
			      tt * (0.0360768 +
				    tt * 0.0045813)))));
    return (u);
  } else {
    ti = 1.0 / t;
    u = 0.39894228 +
      ti * (0.01328592 +
	    ti * (0.00225319 +
		  ti * (-0.00157565 +
			ti * (0.00916281 +
			      ti * (-0.02057706 +
                                          ti * (0.02635537 +
                                                ti * (-0.01647633 +
                                                      ti * 0.00392377)))))));
    return (u * exp(ABS(x)) / sqrt(ABS(x)));
  }
}

/*
  BESSI1: modified Bessel function I1(x).  See A&S 9.8.3, 9.8.4.
  from nemo_3.0.7/src/kernel/misc/besselfunc.c
*/

double bessi1(double x)
{
  double t, tt, ti, u;
  
  t = x / 3.75;
  tt = t * t;
  if (tt < 1.0) {
    u = 0.5 +
      tt * (0.87890594 +
	    tt * (0.51498869 +
		  tt * (0.15084934 +
			tt * (0.02658733 +
			      tt * (0.00301532 +
				    tt * 0.00032411)))));
    return (u * x);
  } else {
    if (t < 0.0){
      fprintf(stderr, "bessi1(%g): negative argument", x);
      exit(1);
    }

    ti = 1.0 / t;
    u = 0.39894228 +
      ti * (-0.03988024 +
	    ti * (-0.00362018 +
		  ti * (0.00163801 +
			ti * (-0.01031555 +
			      ti * (0.02282967 +
				    ti * (-0.02895312 +
					  ti * (0.01787654 +
						ti * -0.00420059)))))));
    return (u * exp(ABS(x)) / sqrt(ABS(x)));
  }
}

/*
  BESSK0: modified Bessel function K0(x).  See A&S 9.8.5, 9.8.6.
  from nemo_3.0.7/src/kernel/misc/besselfunc.c
*/

double bessk0(double x)
{
  double t, tt, ti, u;
  
  if (x < 0.0){
    fprintf(stderr, "bessk0(%g): negative argument", x);
    exit(1);
  }

  t = x / 2.0;
  if (t < 1.0) {
    tt = t * t;
    u = -0.57721566 +
      tt * (0.42278420 +
	    tt * (0.23069756 +
		  tt * (0.03488590 +
			tt * (0.00262698 +
			      tt * (0.00010750 +
				    tt * 0.00000740)))));
    return (u - log(t) * bessi0(x));
  } else {
    ti = 1.0 / t;
    u = 1.25331414 +
      ti * (-0.07832358 +
	    ti * (0.02189568 +
		  ti * (-0.01062446 +
			ti * (0.00587872 +
			      ti * (-0.00251540 +
				    ti * 0.00053208)))));
    return (u * exp(-x) / sqrt(x));
  }
}


/*
  BESSK1: modified Bessel function K1(x).  See A&S 9.8.7, 9.8.8.
  from nemo_3.0.7/src/kernel/misc/besselfunc.c
*/

double bessk1(double x)
{
  double t, tt, ti, u;

  if (x < 0.0){
    fprintf(stderr, "bessk1(%g): negative argument", x);
    exit(1);
  }

  t = x / 2.0;
  if (t < 1.0) {
    tt = t * t;
    u = 1.0 +
      tt * (0.15443144 +
	    tt * (-0.67278579 +
		  tt * (-0.18156897 +
			tt * (-0.01919402 +
			      tt * (-0.00110404 +
				    tt * -0.00004686)))));
    return (u / x + log(t) * bessi1(x));
  } else {
    ti = 1.0 / t;
    u = 1.25331414 +
      ti * (0.23498619 +
	    ti * (-0.03655620 +
		  ti * (0.01504268 +
			ti * (-0.00780353 +
			      ti * (0.00325614 +
				    ti * -0.00068245)))));
    return (u * exp(-x) / sqrt(x));
  }
}


/* Exponential integral
   adapted from Abramowitz and Stegun, sec 5.1
   adapted from nemo_3.2.4/usr/boily/magalie/magalie.f
*/

double expint(double x)
{
  if(x <= 0.0){
    fprintf(stderr, "bad arguments in %s (x = %lf)\n", __FUNCTION__, x);
    exit(1);
  }

  if(x <= 1.0){
    return -log(x) - 0.57721566 + 
      x * (0.99999193 +
	   x * (-0.24991055 +
		x * (0.05519968 +
		     x * (-0.00976004 + 
			  x * 0.00107857))));
  }else{
    double aux1, aux2;

    aux1 = 0.2677737343 + 
      x * (8.6347608925 + 
	   x * (18.0590169730 +
		x * (8.5733287401 + 
		     x)));
      
    aux2 = 3.9584969228 +
      x * (21.0996530827 +
	   x * (25.6329561486 +
		x * (9.5733223454 +
		     x)));

    return aux1/(aux2*exp(x)*x);
  }
}
