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
#include <math.h>
#include <string.h>

#include <xc.h>

/* array terminated with 666 */
static double trial_rho[]   = {0.12E-02,  0.88E-01,  0.17E+01,  0.82E+02,  0.18E+04,  0.53E+05};
static double trial_sigma[] = {     0.0,  0.81E-11,  0.87E-01,  0.17E+01,  0.86E+04,  0.29E+10};
static double trial_lapl[]  = {     0.0, -0.18E+04, -0.17E+01, -0.12E-02,  0.12E-02,  0.17E+01,  0.18E+04};
static double trial_tau[]   = {     0.0,  0.12E-02,  0.88E-01,  0.17E+01,  0.82E+02,  0.18E+04,  0.53E+05};
/* static int n_rho=6, n_sigma=6, n_lrho=7, n_tau=7; */
static int n_rho=2, n_sigma=2, n_lapl=2, n_tau=2;

typedef struct values_t {
  double rho[2], sigma[3], lapl[2], tau[2];

  double zk, vrho[2], vsigma[3], vlapl[2], vtau[2];
  double v2rho2[3], v2tau2[3], v2lapl2[3];
  double v2rhotau[3], v2rholapl[3], v2lapltau[3];
  double v2sigma2[6], v2rhosigma[6], v2sigmatau[6], v2sigmalapl[6];
} values_t;


void get_point(xc_func_type *func, values_t *p)
{
  double *zk, *vrho, *v2rho2;

  zk     = (func->info->flags & XC_FLAGS_HAVE_EXC) ? &(p->zk)  : NULL;
  vrho   = (func->info->flags & XC_FLAGS_HAVE_VXC) ? p->vrho   : NULL;
  v2rho2 = (func->info->flags & XC_FLAGS_HAVE_FXC) ? p->v2rho2 : NULL;

  switch(func->info->family)
    {
    case XC_FAMILY_LDA:
      xc_lda(func, 1, p->rho, zk, vrho, v2rho2, NULL);
      break;
    case XC_FAMILY_GGA:
    case XC_FAMILY_HYB_GGA:
      xc_gga(func, 1, p->rho, p->sigma, zk, vrho, p->vsigma, v2rho2, p->v2rhosigma, p->v2sigma2);
      break;
    case XC_FAMILY_MGGA:
    case XC_FAMILY_HYB_MGGA:
      xc_mgga(func, 1, p->rho, p->sigma, p->lapl, p->tau, zk, vrho, p->vsigma, p->vlapl, p->vtau,
	      v2rho2, p->v2sigma2, p->v2lapl2, p->v2tau2, p->v2rhosigma, p->v2rholapl, p->v2rhotau, 
	      p->v2sigmalapl, p->v2sigmatau, p->v2lapltau);
    }
}

static unsigned int mylog2 (unsigned int val) {
  unsigned int ret = -1;
  while (val != 0) {
    val >>= 1;
    ret++;
  }
  return ret;
}


void values_t_copy(values_t *dest, values_t *src)
{
  memset(dest, 0, sizeof(values_t));

  dest->rho[0] = src->rho[0];
  dest->rho[1] = src->rho[1];
  dest->sigma[0] = src->sigma[0];
  dest->sigma[1] = src->sigma[1];
  dest->sigma[2] = src->sigma[2];
  dest->lapl[0] = src->lapl[0];
  dest->lapl[1] = src->lapl[1];
  dest->tau[0] = src->tau[0];
  dest->tau[1] = src->tau[1];
}

void add_values(int *n_p1, values_t **p1, int n_add, int where, double *trial_add)
{
  int n_p2, i1, i2;
  values_t *p2;

  n_p2 = (*n_p1)*n_add;
  p2 = (values_t *) malloc(n_p2*sizeof(values_t));
  for(i1=0; i1<(*n_p1); i1++){
    for(i2=0; i2<n_add; i2++){
      values_t_copy(&(p2[i1*n_add + i2]), &((*p1)[i1]));
      *((double *)(&p2[i1*n_rho + i2]) + where) = trial_add[i2];
    }
  }
  free(*p1); *n_p1=n_p2; *p1=p2;
}

void print_values(char *desc, int nspin, int n_val, double *val)
{
  int i, n = (nspin == 1) ? 1 : n_val;

  printf("%s", desc);
  for(i=0; i<n; i++)
    printf(" %#19.12E", val[i]);
  printf("\n");
}


/*----------------------------------------------------------*/
int main(int argc, char *argv[])
{
  int func_id, nspin;

  char *label_nspin[] = {"UNPOLARIZED", "POLARIZED"};
  char *label_kind[]  = {"EXCHANGE", "CORRELATION", "EXCHANGE_CORRELATION", "KINETIC"};
  char *label_family[]= {"LDA", "GGA", "MGGA", "LCA", "OEP", "HYB_GGA", "HYB_MGGA"};
  char *label_flags[] = {"HAVE_EXC", "HAVE_VXC", "HAVE_FXC", "HAVE_KXC", "HAVE_LXC", "1D", "2D", "3D", "STABLE", "DEVELOPMENT"};

  if(argc != 2){
    fprintf(stderr, "Usage:\n%s funct\n", argv[0]);
    exit(1);
  }
  
  func_id = XC(functional_get_number)(argv[1]);
  if(func_id <= 0){
    fprintf(stderr, "Functional '%s' not found\n", argv[1]);
    exit(1);
  }
    
  for(nspin=1; nspin<=2; nspin++){
    xc_func_type func;
    const xc_func_info_type *info;

    /* initialize functional */
    if(xc_func_init(&func, func_id, nspin) != 0){
      fprintf(stderr, "Functional '%d' not found\n", func_id);
      exit(1);    
    }
    info = func.info;

    /* print some information */
    if(nspin == 1){
      char flags[256] = "";      
      int ii;

      for(ii=0; ii<32; ii++)
	if(info->flags & 1<<ii){
	  if(flags[0] != '\0')
	    strcat(flags, " | ");
	  strcat(flags, label_flags[ii]);
	}

      printf("Number: %d\n", info->number);
      printf("Kind  : %s\n", label_kind[info->kind]);
      printf("Name  : %s\n", info->name);
      printf("Family: %s\n", label_family[mylog2(info->family)]);
      printf("Flags : %s\n", flags);
      printf("Refs  :\n%s\n", info->refs);
    }

    printf("\n--------------------------------------------------------------\n");
    printf("Nspin : %s\n", label_nspin[nspin-1]);

    /* print now all values */
    {
      int i1, i2, n_p1, n_p2;
      values_t *p, *p1, *p2;
      
      /* first we add rho */
      n_p1 = n_rho;
      p1 = (values_t *) malloc(n_p1*sizeof(values_t));
      memset(p1, 0, n_p1*sizeof(values_t));
      for(i1=0; i1<n_rho; i1++)
	p1[i1].rho[0] = trial_rho[i1];
      
      /* add all other variables */
      if(nspin == XC_POLARIZED)
	add_values(&n_p1, &p1, n_rho, 1, trial_rho);

      if(info->family & (XC_FAMILY_GGA | XC_FAMILY_HYB_GGA | XC_FAMILY_MGGA | XC_FAMILY_HYB_MGGA)){
	add_values(&n_p1, &p1, n_sigma, 2, trial_sigma);
	if(nspin == XC_POLARIZED){
	  add_values(&n_p1, &p1, n_sigma, 3, trial_sigma);
	  add_values(&n_p1, &p1, n_sigma, 4, trial_sigma);
	}
      }

      if(info->family & (XC_FAMILY_MGGA | XC_FAMILY_HYB_MGGA)){
	add_values(&n_p1, &p1, n_lapl, 5, trial_lapl);
	if(nspin == XC_POLARIZED)
	  add_values(&n_p1, &p1, n_lapl, 6, trial_lapl);

	add_values(&n_p1, &p1, n_tau, 7, trial_tau);
	if(nspin == XC_POLARIZED)
	  add_values(&n_p1, &p1, n_tau, 8, trial_tau);
      }

      /* calculate points and print */
      for(i1=0, p=p1; i1<n_p1; i1++, p++){
	get_point(&func, p);

	printf("\n----\n");

	print_values("rho    :", nspin, 2, p->rho);
	if(info->family & (XC_FAMILY_GGA | XC_FAMILY_HYB_GGA | XC_FAMILY_MGGA | XC_FAMILY_HYB_MGGA))
	  print_values("sigma  :", nspin, 3, p->sigma);
	if(info->family & (XC_FAMILY_MGGA | XC_FAMILY_HYB_MGGA)){
	  print_values("lapl   :", nspin, 2, p->lapl);
	  print_values("tau    :", nspin, 2, p->tau);
	}
	printf("\n");

	/* energy */
	if(info->flags & XC_FLAGS_HAVE_EXC)
	  print_values("zk     :", nspin, 1, &(p->zk));

	/* first order derivatives */
	if(info->flags & XC_FLAGS_HAVE_VXC){
	  print_values("vrho   :", nspin, 2, p->vrho);
	  if(info->family & (XC_FAMILY_GGA | XC_FAMILY_HYB_GGA | XC_FAMILY_MGGA | XC_FAMILY_HYB_MGGA))
	    print_values("vsigma :", nspin, 3, p->vsigma);
	  if(info->family & (XC_FAMILY_MGGA | XC_FAMILY_HYB_MGGA)){
	    print_values("vlapl  :", nspin, 2, p->vlapl);
	    print_values("vtau   :", nspin, 2, p->vtau);
	  }
	  printf("\n");
	}

	/* second-order derivatives */
	if(info->flags & XC_FLAGS_HAVE_FXC){
	  print_values("v2rho2     :", nspin, 3, p->v2rho2);
	  if(info->family & (XC_FAMILY_GGA | XC_FAMILY_HYB_GGA | XC_FAMILY_MGGA | XC_FAMILY_HYB_MGGA)){
	    print_values("v2sigma2   :", nspin, 6, p->v2sigma2);
	    print_values("v2rhosigma :", nspin, 6, p->v2rhosigma);
	  }

	  if(info->family & (XC_FAMILY_MGGA | XC_FAMILY_HYB_MGGA)){
	    print_values("v2lapl2    :", nspin, 3, p->v2lapl2);
	    print_values("v2tau2     :", nspin, 3, p->v2tau2);
	    print_values("v2rholapl  :", nspin, 3, p->v2rholapl);
	    print_values("v2rhotau   :", nspin, 3, p->v2rhotau);
	    print_values("v2lapltau  :", nspin, 3, p->v2lapltau);
	    print_values("v2sigmatau :", nspin, 6, p->v2sigmatau);
	    print_values("v2sigmalapl:", nspin, 6, p->v2sigmalapl);
	  }
	  printf("\n");
	}
      }

      /* clean up */
      free(p1); p1 = NULL;
    }
    xc_func_end(&func);
  }
}
