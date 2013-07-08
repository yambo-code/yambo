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

/* for a review on the values of lambda and gamma, please see EV
Ludeña and VV Karasiev, in "Reviews of Modern Quantum Chemistry: a
Celebration of the Contributions of Robert G. Parr, edited by KD Sen
(World Scientific, Singapore, 2002), p. 612.
 */

#define XC_GGA_K_VW            500 /* von Weiszaecker functional */
#define XC_GGA_K_GE2           501 /* Second-order gradient expansion (l = 1/9) */
#define XC_GGA_K_GOLDEN        502 /* TF-lambda-vW form by Golden (l = 13/45) */
#define XC_GGA_K_YT65          503 /* TF-lambda-vW form by Yonei and Tomishima (l = 1/5) */
#define XC_GGA_K_BALTIN        504 /* TF-lambda-vW form by Baltin (l = 5/9) */
#define XC_GGA_K_LIEB          505 /* TF-lambda-vW form by Lieb (l = 0.185909191) */
#define XC_GGA_K_ABSR1         506 /* gamma-TFvW form by Acharya et al [g = 1 - 1.412/N^(1/3)] */
#define XC_GGA_K_ABSR2         507 /* gamma-TFvW form by Acharya et al [g = 1 - 1.332/N^(1/3)] */
#define XC_GGA_K_GR            508 /* gamma-TFvW form by Gázquez and Robles */
#define XC_GGA_K_LUDENA        509 /* gamma-TFvW form by Ludeña */
#define XC_GGA_K_GP85          510 /* gamma-TFvW form by Ghosh and Parr */

typedef struct{
  FLOAT gamma, lambda;
} gga_k_tflw_params;


static void 
gga_k_tflw_init(void *p_)
{
  XC(gga_type) *p = (XC(gga_type) *)p_;

  assert(p->params == NULL);
  p->params = malloc(sizeof(gga_k_tflw_params));

  /* This automatically sets gamma and lambda depending on the functional chosen.
     We put by default N = 1.0 */
  XC(gga_k_tflw_set_params_)(p, -1.0, -1.0, 1.0);
}

void 
XC(gga_k_tflw_set_params)(XC(func_type) *p, FLOAT gamma, FLOAT lambda, FLOAT N)
{
  assert(p != NULL && p->gga != NULL);
  XC(gga_k_tflw_set_params_)(p->gga, gamma, lambda, N);
}

/* for automatically assigning lambda and gamma set them to -1 */
void 
XC(gga_k_tflw_set_params_)(XC(gga_type) *p, FLOAT gamma, FLOAT lambda, FLOAT N)
{
  gga_k_tflw_params *params;

  assert(p->params != NULL);
  params = (gga_k_tflw_params *) (p->params);

  params->gamma = 1.0;
  if(gamma > 0.0){
    params->gamma = gamma;
  }else if(N > 0.0){
    switch(p->info->number){
    case XC_GGA_K_VW:
      params->gamma = 0.0;
      break;
    case XC_GGA_K_ABSR1:      /* Ref. 79 */
      params->gamma = 1.0 - 1.412/CBRT(N);
      break;
    case XC_GGA_K_ABSR2:      /* Ref. 79 */
      params->gamma = 1.0 - 1.332/CBRT(N);
      break;
    case XC_GGA_K_GR:         /* Ref. 80 */
      params->gamma = (1.0 - 2.0/N)*(1.0 - 1.015/CBRT(N) + 0.150*CBRT(N*N));
      break;
    case XC_GGA_K_LUDENA:     /* Ref. 82 */
      params->gamma = CBRT(6.0*M_PI)*M_PI*M_PI*(1.0 - 1.0/(N*N));
	break;
    case XC_GGA_K_GP85:       /* Ref. 86 */
      params->gamma = CBRT(6.0*M_PI*M_PI)*M_PI*M_PI/4.0*
	(1.0 - 1.0/N)*(1.0 + 1.0/N + 6.0/(N*N));
      break;
    }
  }

  params->lambda = 1.0;
  if(lambda > 0.0){
    params->lambda  = lambda;
  }else{
    switch(p->info->number){
    case XC_GGA_K_GE2:
      params->lambda = 1.0/9.0;
      break;
    case XC_GGA_K_GOLDEN:     /* Ref. 33 */
      params->lambda = 13.0/45.0;
      break;
    case XC_GGA_K_YT65:       /* Ref. 57 */
      params->lambda = 1.0/5.0;
      break;
    case XC_GGA_K_BALTIN:     /* Ref. 66 */
      params->lambda = 5.0/9.0;
      break;
    case XC_GGA_K_LIEB:       /* Ref. 12 */
      params->lambda = 0.185909191;   /* 1/5.37897... */
      break;
    }
  }
}


static inline void 
func(const XC(gga_type) *p, int order, FLOAT x, 
     FLOAT *f, FLOAT *dfdx, FLOAT *d2fdx2)
{
  FLOAT lambda, gamma;

  assert(p->params != NULL);
  lambda  = ((gga_k_tflw_params *) (p->params))->lambda;
  gamma   = ((gga_k_tflw_params *) (p->params))->gamma;

  lambda /= 8.0; /* the von Weiszaecker coefficient */
  
  *f = gamma + lambda*x*x/K_FACTOR_C;

  if(order < 1) return;

  *dfdx = 2.0*lambda*x/K_FACTOR_C;
  
  if(order < 2) return;

  *d2fdx2 = 2.0*lambda/K_FACTOR_C;
}

#define XC_KINETIC_FUNCTIONAL
#include "work_gga_x.c"

const XC(func_info_type) XC(func_info_gga_k_vw) = {
  XC_GGA_K_VW,
  XC_KINETIC,
  "von Weiszaecker correction to Thomas-Fermi",
  XC_FAMILY_GGA,
  "CF von Weiszaecker, Z. Phys. 96, 431 (1935)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_k_tflw_init, 
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_ge2) = {
  XC_GGA_K_GE2,
  XC_KINETIC,
  "Second-order gradient expansion of the kinetic energy density",
  XC_FAMILY_GGA,
  "AS Kompaneets and ES Pavlovskii, Zh. Eksp. Teor. Fiz. 31, 427 (1956) [Sov. Phys. JETP 4, 328 (1957)]"
  "DA Kirznits, Zh. Eksp. Teor. Fiz. 32, 115 (1957) [Sov. Phys. JETP 5, 64 (1957)]",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_k_tflw_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_golden) = {
  XC_GGA_K_GOLDEN,
  XC_KINETIC,
  "TF-lambda-vW form by Golden (l = 13/45)",
  XC_FAMILY_GGA,
  "S Golden, Phys. Rev. 105, 604–615 (1957)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_k_tflw_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_yt65) = {
  XC_GGA_K_YT65,
  XC_KINETIC,
  "TF-lambda-vW form by Yonei and Tomishima (l = 1/5)",
  XC_FAMILY_GGA,
  "K. Yonei and Y. Tomishima, J. Phys. Soc. Jpn. 20, 1051-1057 (1965)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_k_tflw_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_baltin) = {
  XC_GGA_K_BALTIN,
  XC_KINETIC,
  "TF-lambda-vW form by Baltin (l = 5/9)",
  XC_FAMILY_GGA,
  "R Baltin, Z. Naturforsch. 27, 1176 (1972)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_k_tflw_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_lieb) = {
  XC_GGA_K_LIEB,
  XC_KINETIC,
  "TF-lambda-vW form by Lieb (l = 0.185909191)",
  XC_FAMILY_GGA,
  "EH Lieb, Rev. Mod. Phys. 53, 603–641 (1981)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_k_tflw_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_absr1) = {
  XC_GGA_K_ABSR1,
  XC_KINETIC,
  "gamma-TFvW form by Acharya et al [g = 1 - 1.412/N^(1/3)]",
  XC_FAMILY_GGA,
  "PK Acharya, LJ Bartolotti, SB Sears, and RG Parr, Proc. Natl. Acad. Sci. USA 77 6978-6982 (1980)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_k_tflw_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_absr2) = {
  XC_GGA_K_ABSR2,
  XC_KINETIC,
  "gamma-TFvW form by Acharya et al [g = 1 - 1.332/N^(1/3)]",
  XC_FAMILY_GGA,
  "PK Acharya, LJ Bartolotti, SB Sears, and RG Parr, Proc. Natl. Acad. Sci. USA 77 6978-6982 (1980)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_k_tflw_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_gr) = {
  XC_GGA_K_GR,
  XC_KINETIC,
  "gamma-TFvW form by Gázquez and Robles",
  XC_FAMILY_GGA,
  "JL Gázquez and J Robles, J. Chem. Phys. 76, 1467 (1982)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_k_tflw_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_ludena) = {
  XC_GGA_K_LUDENA,
  XC_KINETIC,
  "gamma-TFvW form by Ludeña",
  XC_FAMILY_GGA,
  "EV Ludeña, in Cond. Matt. Theor. Vol 1, ed. by FB Malik (Plenum, New York, 1986), p. 183",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_k_tflw_init,
  NULL, NULL,
  work_gga_k
};

const XC(func_info_type) XC(func_info_gga_k_gp85) = {
  XC_GGA_K_GP85,
  XC_KINETIC,
  "gamma-TFvW form by Ghosh and Parr",
  XC_FAMILY_GGA,
  "SK Ghosh and RG Parr, J. Chem. Phys. 82, 3307 (1985)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC,
  MIN_DENS, MIN_GRAD, 0.0, MIN_ZETA,
  gga_k_tflw_init,
  NULL, NULL,
  work_gga_k
};
