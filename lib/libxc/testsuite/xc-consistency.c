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

static double xc_trial_points[][5] = {
  /* rhoa      rhob    sigmaaa   sigmaab   sigmabb */
  {0.17E+01, 0.17E+01, 0.81E-11, 0.81E-11, 0.81E-11},
  {0.17E+01, 0.17E+01, 0.17E+01, 0.17E+01, 0.17E+01},
  {0.15E+01, 0.15E+01, 0.36E+02, 0.36E+02, 0.36E+02},
  {0.88E-01, 0.88E-01, 0.87E-01, 0.87E-01, 0.87E-01},
  {0.18E+04, 0.18E+04, 0.55E+00, 0.55E+00, 0.55E+00},
  {0.18E+04, 0.18E+04, 0.86E+04, 0.86E+04, 0.86E+04},
  {0.16E+04, 0.16E+04, 0.37E+10, 0.37E+10, 0.37E+10},
  {0.26E+00, 0.26E+00, 0.28E+00, 0.28E+00, 0.28E+00},
  {0.53E+05, 0.53E+05, 0.96E+05, 0.96E+05, 0.96E+05},
  {0.47E+05, 0.47E+05, 0.29E+14, 0.29E+14, 0.29E+14},
  {0.15E+00, 0.15E+00, 0.16E+00, 0.16E+00, 0.16E+00},
  {0.35E+01, 0.00E+00, 0.46E-10, 0.00E+00, 0.00E+00},
  {0.35E+01, 0.00E+00, 0.34E+01, 0.00E+00, 0.00E+00},
  {0.30E+01, 0.00E+00, 0.20E+03, 0.00E+00, 0.00E+00},
  {0.58E-01, 0.00E+00, 0.47E-01, 0.00E+00, 0.00E+00},
  {0.82E+02, 0.81E+02, 0.49E+07, 0.49E+07, 0.49E+07},
  {0.39E+02, 0.38E+02, 0.81E+06, 0.82E+06, 0.82E+06},
  {0.13E+00, 0.95E-01, 0.15E+00, 0.18E+00, 0.22E+00},
  {0.78E-01, 0.31E-01, 0.41E-02, 0.38E-02, 0.36E-02},
  {0.50E+02, 0.49E+02, 0.11E+06, 0.11E+06, 0.11E+06},
  {0.40E+02, 0.40E+02, 0.99E+05, 0.98E+05, 0.98E+05},
  {0.12E+00, 0.10E+00, 0.12E+00, 0.13E+00, 0.14E+00},
  {0.48E-01, 0.25E-01, 0.46E-02, 0.44E-02, 0.41E-02},
  {0.0, 0.0, 0.0, 0.0, 0.0}
};


int nspin;

void get_val(double point[5], double val[5])
{
  if(nspin == 1){
    val[0]  = point[0] + point[1];
    val[1]  = 0;
    val[2]  = point[2] + 2*point[3] + point[4];
    val[3]  = 0;
    val[4]  = 0;
  }else{
    int i;
    for(i=0; i<5; i++){
      val[i] = point[i];
    }
  }
}

double get_point(xc_func_type *func, double point[5], double *e, double der[5], int which)
{
  switch(func->info->family)
    {
    case XC_FAMILY_LDA:
      xc_lda_exc_vxc(func, 1, &(point[0]), e, &(der[0]));
      break;
    case XC_FAMILY_GGA:
    case XC_FAMILY_HYB_GGA:
      xc_gga_exc_vxc(func, 1, &(point[0]), &(point[2]),
		     e, &(der[0]), &(der[2]));
      break;
    }

  if(which == 0)
    return (*e)*(point[0] + point[1]);
  else
    return der[which-1];
}

void get_vxc(xc_func_type *func, double point[5], double *e, double der[5])
{
  get_point(func, point, e, der, 0);
}

void get_fxc(xc_func_type *func, double point[5], double der[5][5])
{
  double v2rho[3], v2rhosigma[6], v2sigma[6];
  int i, j;

  for(i=0; i<5; i++)
    for(j=0; j<5; j++)
      der[i][j] = 0.0;

  for(i=0; i<3; i++) v2rho[i] = 0.0;
  for(i=0; i<6; i++){
    v2rhosigma[i] = 0.0;
    v2sigma[i]    = 0.0;
  }

  switch(func->info->family)
    {
    case XC_FAMILY_LDA:
      xc_lda_fxc(func, 1, &(point[0]), v2rho);
      break;
    case XC_FAMILY_GGA:
    case XC_FAMILY_HYB_GGA:
      xc_gga_fxc(func, 1, &(point[0]), &(point[2]),
		 v2rho, v2rhosigma, v2sigma);
      break;
    }

  der[0][0] = v2rho[0];
  der[0][1] = der[1][0] = v2rho[1];
  der[1][1] = v2rho[2];
  der[0][2] = der[2][0] = v2rhosigma[0];
  der[0][3] = der[3][0] = v2rhosigma[1];
  der[0][4] = der[4][0] = v2rhosigma[2];
  der[1][2] = der[2][1] = v2rhosigma[3];
  der[1][3] = der[3][1] = v2rhosigma[4];
  der[1][4] = der[4][1] = v2rhosigma[5];
  der[2][2] = v2sigma[0];
  der[2][3] = der[3][2] = v2sigma[1];
  der[2][4] = der[4][2] = v2sigma[2];
  der[3][3] = v2sigma[3];
  der[3][4] = der[4][3] = v2sigma[4];
  der[4][4] = v2sigma[5];
}

void first_derivative(xc_func_type *func, double point[5], double der[5], int which)
{
  int i;

  for(i=0; i<5; i++){
    const double delta = 5e-10;

    double dd, p[5], v[5];
    int j;
    
    if(nspin==1 && (i!=0 && i!=2)){
      der[i] = 0.0;
      continue;
    }

    dd = point[i]*delta;
    if(dd < delta) dd = delta;

    for(j=0; j<5; j++) p[j] = point[j];

    if(point[i]>=3.0*dd){ /* centered difference */
      double e, em1, em2, ep1, ep2;

      p[i] = point[i] + dd;
      ep1 = get_point(func, p, &e, v, which);

      p[i] = point[i] + 2*dd;
      ep2 = get_point(func, p, &e, v, which);

      p[i] = point[i] - dd;  /* backward point */
      em1 = get_point(func, p, &e, v, which);

      p[i] = point[i] - 2*dd;  /* backward point */
      em2 = get_point(func, p, &e, v, which);

      der[i]  = 1.0/2.0*(ep1 - em1);
      der[i] += 1.0/12.0*(em2 - 2*em1 + 2*ep1 - ep2);

      der[i] /= dd;

    }else{  /* we use a 5 point forward difference */
      double e, e1, e2, e3, e4, e5;

      p[i] = point[i];
      e1 = get_point(func, p, &e, v, which);

      p[i] = point[i] + dd;
      e2 = get_point(func, p, &e, v, which);

      p[i] = point[i] + 2.0*dd;
      e3 = get_point(func, p, &e, v, which);

      p[i] = point[i] + 3.0*dd;
      e4 = get_point(func, p, &e, v, which);

      p[i] = point[i] + 4.0*dd;
      e5 = get_point(func, p, &e, v, which);

      der[i]  =          (-e1 + e2);
      der[i] -=  1.0/2.0*( e1 - 2*e2 + e3);
      der[i] +=  1.0/3.0*(-e1 + 3*e2 - 3*e3 + e4);
      der[i] -=  1.0/4.0*( e1 - 4*e2 + 6*e3 - 4*e4 + e5);

      der[i] /= dd;
    }
  }
}

void second_derivatives(xc_func_type *func, double point[5], double der[5][5])
{
  int i;

  for(i=0; i<5; i++){
    first_derivative(func, point, der[i], i+1);
  }
}


void print_error(char *type, char *what, double diff, xc_func_type *func, double *p)
{
  static char *red="\033[31;1m", *norm="\033[0m";
  char *color;

  color = (diff > 5e-4) ? red : norm;
  
  printf("%s error %s: %s%g%s\n", type, what, color, diff, norm);

  if(func == NULL) return;

  printf("   point (% 8.2e, % 8.2e, % 8.2e, % 8.2e, % 8.2e)\n", 
	 p[0], p[1], p[2], p[3], p[4]);

  if(strcmp(what, "vrho")==0 || strcmp(what, "vsig")==0){
    double e, v_an[5], v_fd[5];
    int j;

    for(j=0; j<5; j++)
      v_fd[j] = v_an[j] = 0.0;

    get_vxc(func, p, &e, v_an);
    first_derivative(func, p, v_fd, 0);

    if(strcmp(what, "vrho") == 0){
      printf("  analyt (% 8.2e, % 8.2e)\n", v_an[0], v_an[1]);
      printf("      fd (% 8.2e, % 8.2e)\n", v_fd[0], v_fd[1]);
    }

    if(strcmp(what, "vsig") == 0){
      printf("  analyt (% 8.2e, % 8.2e, % 8.2e)\n", v_an[2], v_an[3], v_an[4]);
      printf("      fd (% 8.2e, % 8.2e, % 8.2e)\n", v_fd[2], v_fd[3], v_fd[4]);
    }
  }

  if(strcmp(what, "v2rho2")==0 || strcmp(what, "v2rhosig")==0 || strcmp(what, "v2sig2")==0){
    double f_an[5][5], f_fd[5][5];
    int i, j;

    get_fxc(func, p, f_an);
    second_derivatives(func, p, f_fd);

    if(strcmp(what, "v2rho2") == 0){
      printf("  analyt (% 8.2e, % 8.2e, % 8.2e)\n", f_an[0][0], f_an[0][1], f_an[1][1]);
      printf("      fd (% 8.2e, % 8.2e, % 8.2e)\n", f_fd[0][0], f_fd[0][1], f_fd[1][1]);
    }

    if(strcmp(what, "v2rhosig") == 0){
      printf("  analyt (% 8.2e, % 8.2e, % 8.2e, % 8.2e, % 8.2e, % 8.2e)\n", 
	     f_an[2][0], f_an[3][0], f_an[4][0], f_an[2][1], f_an[3][1], f_an[4][1]);
      printf("      fd (% 8.2e, % 8.2e, % 8.2e, % 8.2e, % 8.2e, % 8.2e)\n", 
	     f_fd[2][0], f_fd[3][0], f_fd[4][0], f_fd[2][1], f_fd[3][1], f_fd[4][1]);
    }

    if(strcmp(what, "v2sig2") == 0){
      printf("  analyt (% 8.2e, % 8.2e, % 8.2e, % 8.2e, % 8.2e, % 8.2e)\n", 
	     f_an[2][2], f_an[3][2], f_an[4][2], f_an[3][3], f_an[4][3], f_an[4][4]);
      printf("      fd (% 8.2e, % 8.2e, % 8.2e, % 8.2e, % 8.2e, % 8.2e)\n", 
	     f_fd[2][2], f_fd[3][2], f_fd[4][2], f_fd[3][3], f_fd[4][3], f_fd[4][4]);
    }
  }

}


void test_functional(int functional)
{
  xc_func_type func;
  const xc_func_info_type *info;
  int i, j, k, p_max[6][5];
  double max_diff[6][5], avg_diff[6][5], val[5];

  /* initialize functional */
  if(xc_func_init(&func, functional, nspin) != 0){
    fprintf(stderr, "Functional '%d' not found\n", functional);
    exit(1);    
  }

  info = func.info;

  if(functional == XC_LDA_C_2D_PRM)
    xc_lda_c_2d_prm_set_params(&func, 10.0);
  
  for(k=0; k<6; k++)
    for(j=0; j<5; j++){
      avg_diff[k][j] = 0.0;
      
      p_max[k][j]    = 0;
      max_diff[k][j] = -1.0;
    }

  for(i=0; xc_trial_points[i][0]!=0.0; i++){
    double e, v_fd[5], f_fd[5][5], v_an[5], f_an[5][5];

    for(j=0; j<5; j++)
      v_fd[j] = v_an[j] = 0.0;

    get_val(xc_trial_points[i], val);

    /* first, get the analytic gradients */
    get_vxc(&func, val, &e, v_an);

    /* now get the numerical gradients */
    first_derivative(&func, val, v_fd, 0);

    if(info->flags & XC_FLAGS_HAVE_FXC){
      int i, j;
      
      /* initialize */
      for(i=0; i<5; i++)
	for(j=0; j<5; j++)
	  f_an[i][j] = f_fd[i][j] = 0.0;

      /* now get the second derivatives */
      second_derivatives(&func, val, f_fd);
      get_fxc(&func, val, f_an);
    }

    /* make statistics */
    for(j=0; j<5; j++){
      double diff = fabs(v_an[j] - v_fd[j]);

      /* do not test in case of spin unpolarized or if spin down is zero */
      if((nspin==1 || val[1]==0.0) && (j!=0 && j!=2))
	continue;

      avg_diff[0][j] += diff;
      if(diff > max_diff[0][j]){
	max_diff[0][j] = diff;
	p_max[0][j] = i;
      }

      if(info->flags & XC_FLAGS_HAVE_FXC){
	for(k=0; k<5; k++){
	  /* do not test in case of spin unpolarized or if spin down is zero */
	  if((nspin==1 || val[1]==0.0) && (k!=0 && k!=2))
	    continue;

	  diff = fabs(f_an[k][j] - f_fd[k][j]);

	  avg_diff[k+1][j] += diff;
	  if(diff > max_diff[k+1][j]){
	    max_diff[k+1][j] = diff;
	    p_max[k+1][j] = i;
	  }
	}
      }
    }

  }

  for(k=0; k<6; k++)
    for(j=0; j<5; j++){
      avg_diff[k][j] /= i;
    }

  /* print statistics */
  {
    double diff;
    int i, j;

    printf("Functional: %s\n", info->name);
    print_error("Avg.", "vrho", (avg_diff[0][0] + avg_diff[0][1])/2.0, NULL, NULL);
    j = (max_diff[0][0] > max_diff[0][1]) ? 0 : 1;
    get_val(xc_trial_points[p_max[0][j]], val);
    print_error("Max.", "vrho", max_diff[0][j], &func, val);

    if(info->family > XC_FAMILY_LDA){
      print_error("Avg.", "vsig", (avg_diff[0][2] + avg_diff[0][3] + avg_diff[0][4])/3.0, NULL, NULL);
      j = (max_diff[0][2] > max_diff[0][3]) ? 2 : 3;
      j = (max_diff[0][j] > max_diff[0][4]) ? j : 4;
      get_val(xc_trial_points[p_max[0][j]], val);
      print_error("Max.", "vsig", max_diff[0][j], &func, val);
    }

    if(info->flags & XC_FLAGS_HAVE_FXC){
      diff = avg_diff[1][0] + avg_diff[1][1] + avg_diff[2][1];
      diff = diff/3.0;
      print_error("Avg.", "v2rho2", diff, NULL, NULL);
      if(max_diff[1][0] > max_diff[1][1]) {i=1; j=0;} else {i=1; j=1;}
      if(max_diff[2][1] > max_diff[i][j]) {i=2; j=1;}
      get_val(xc_trial_points[p_max[i][j]], val);
      print_error("Max.", "v2rho2", max_diff[i][j], &func, val);

      if(info->family > XC_FAMILY_LDA){
	diff = avg_diff[3][0] + avg_diff[4][0] + avg_diff[5][0] + avg_diff[3][1] + avg_diff[4][1] + avg_diff[5][1];
	diff = diff/6.0;
	print_error("Avg.", "v2rhosig", diff, NULL, NULL);
	if(max_diff[3][0] > max_diff[4][0]) {i=3; j=0;} else {i=4; j=0;}
	if(max_diff[5][0] > max_diff[i][j]) {i=5; j=0;}
	if(max_diff[3][1] > max_diff[i][j]) {i=3; j=1;}
	if(max_diff[4][1] > max_diff[i][j]) {i=4; j=1;}
	if(max_diff[5][1] > max_diff[i][j]) {i=5; j=1;}
	get_val(xc_trial_points[p_max[i][j]], val);
	print_error("Max.", "v2rhosig", max_diff[i][j], &func, val);

	diff = avg_diff[3][2] + avg_diff[4][2] + avg_diff[5][2] + avg_diff[4][3] + avg_diff[5][3] + avg_diff[5][4];
	diff = diff/6.0;
	print_error("Avg.", "v2sig2", diff, NULL, NULL);
	if(max_diff[3][2] > max_diff[4][2]) {i=3; j=2;} else {i=4; j=2;}
	if(max_diff[5][2] > max_diff[i][j]) {i=5; j=2;}
	if(max_diff[4][3] > max_diff[i][j]) {i=4; j=3;}
	if(max_diff[5][3] > max_diff[i][j]) {i=5; j=3;}
	if(max_diff[5][4] > max_diff[i][j]) {i=5; j=4;}
	get_val(xc_trial_points[p_max[i][j]], val);
	print_error("Max.", "v2sig2", max_diff[i][j], &func, val);
      }
    }
  }
}

/*----------------------------------------------------------*/
int main(int argc, char *argv[])
{
  if(argc != 2){
    printf("Usage:\n%s funct\n", argv[0]);
    return 1;
  }
  
  printf("------------------------\nNspin = 1\n");
  nspin = 1;
  test_functional(atoi(argv[1]));

  printf("------------------------\nNspin = 2\n");
  nspin = 2;
  test_functional(atoi(argv[1]));

  return 0;
}
