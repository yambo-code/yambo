/*
  License-Identifier: GPL
 
  Copyright (C) 2020 The Yambo Team
 
  Authors (see AUTHORS file for details): AM
*/

#include <stdlib.h>
#include <stdio.h>
#include <wrapper.h>
#include <kind.h>
#include <driver.h>
#include <string.h>

char *running_libraries()
{
 int i_str,str_len,i_c;
 char strings[20][20], *c;
 i_str=0;
#if defined _MPI
 strcpy(strings[i_str], "MPI");
#else
 strcpy(strings[i_str], "Serial");
#endif
#if defined _GPU
 i_str++;
 strcpy(strings[i_str], "GPU");
#endif
#if defined _CUDA
 i_str++;
 strcpy(strings[i_str], "CUDA");
#endif
#if defined _CUDAF
 i_str++;
 strcpy(strings[i_str], "CUDAF");
#endif
#if defined _OPENACC
 i_str++;
 strcpy(strings[i_str], "OpenACC");
#endif
#if defined _OPENMP
 i_str++;
 strcpy(strings[i_str], "OpenMP");
#endif
#if defined _OPENMP_GPU
 i_str++;
 strcpy(strings[i_str], "OpenMP-GPU");
#endif
#if defined _SCALAPACK
 i_str++;
 strcpy(strings[i_str], "SLK");
#endif
#if defined _SLEPC
 i_str++;
 strcpy(strings[i_str], "SLEPC");
#endif
#if defined _PAR_IO
 i_str++;
 strcpy(strings[i_str], "HDF5_MPI_IO");
#elif defined _HDF5_IO
 i_str++;
 strcpy(strings[i_str], "HDF5_IO");
#elif defined _HDF5_LIB
 i_str++;
 strcpy(strings[i_str], "HDF5_LIB");
#endif 
 str_len=0;
 for(i_c=0;i_c<=i_str;i_c++) {
  str_len=str_len+sizeof(strings[i_c]);
 }
 c = malloc(str_len+1);
 strcpy(c,"");
 for(i_c=0;i_c<=i_str;i_c++) {
  if (i_c>0) strcat(c,"+");
  strcat(c,strings[i_c]);
 }
 return c;
}
void C_FUNC(get_libraries, GET_LIBRARIES)(char *libraries)
{
 char *c = running_libraries();
 int len = strlen(c);
 strcpy(libraries, c);
 libraries[len] = libraries[len + 1];
}

