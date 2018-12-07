/*
         Copyright (C) 2000-2018 the YAMBO team
               http://www.yambo-code.org
 
  Authors (see AUTHORS file for details): AM
  
  This file is distributed under the terms of the GNU 
  General Public License. You can redistribute it and/or 
  modify it under the terms of the GNU General Public 
  License as published by the Free Software Foundation; 
  either version 2, or (at your option) any later version.
 
  This program is distributed in the hope that it will 
  be useful, but WITHOUT ANY WARRANTY; without even the 
  implied warranty of MERCHANTABILITY or FITNESS FOR A 
  PARTICULAR PURPOSE.  See the GNU General Public License 
  for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
  MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <getopt.h>
#include <options_kind.h>

void usage(Ldes *opts,int verbose, char *tool, char *codever, char *tool_desc)
{
 int i,j,nr=0;
 while(opts[nr].ln!=NULL) {nr++;};
 if (verbose==1) {
#if defined _MPI
  char* MPI_string="MPI";
#else
  char* MPI_string="Serial";
#endif
#if defined _OPENMP
  char* OMP_string="+OpenMP";
#else
  char* OMP_string="";
#endif
#if defined _CUDA
  char* CUDA_string="+CUDA";
#else
  char* CUDA_string="";
#endif
#if defined _SCALAPACK
  char* SLK_string="+SLK";
#else
  char* SLK_string="";
#endif
#if defined _SLEPC
  char* SLEPC_string="+SLEPC";
#else
  char* SLEPC_string="";
#endif
#if defined _PAR_IO
  char* HDF5_string="+HDF5_MPI_IO";
#elif defined _HDF5_IO
  char* HDF5_string="+HDF5_IO";
#elif defined _HDF5_LIB
  char* HDF5_string="+HDF5_LIB";
#else
  char* HDF5_string="";
#endif
  fprintf(stderr,"\nThis is %s %s - %s%s%s%s%s%s -\n",tool,codever,MPI_string,OMP_string,CUDA_string,SLK_string,SLEPC_string,HDF5_string); 
  fprintf(stderr,"Usage: %s",tool); 
  for(j=0;j<=nr-1;j++)
  {if (strcmp(opts[j].ln,"DESC")!=0) 
   {fprintf(stderr," -%s",opts[j].sn);
   for(i=1;i<=opts[j].ni;i++) {fprintf(stderr," %s","<int>");};
   for(i=1;i<=opts[j].nr;i++) {fprintf(stderr," %s","<real>");};
   for(i=1;i<=opts[j].nc;i++) {fprintf(stderr," %s","<opt>");};
   };
  };
  fprintf(stderr,"\n%s%s%s\n","Try `",tool," -H' for more information");exit(0);
 };
 /*if (verbose==2) {title(stderr,"",tool,codever,tool_desc);*/
 for(j=0;j<=nr-1;j++)
  {if (strcmp(opts[j].ln,"DESC")==0) 
   {
    fprintf(stderr,"\t\t %s\n",opts[j].d);
   }
   else
   {
    fprintf(stderr," -%s",opts[j].sn);
    for(i=1;i<=opts[j].ni;i++) {fprintf(stderr," %s","<int>");};
    for(i=1;i<=opts[j].nr;i++) {fprintf(stderr," %s","<real>");};
    for(i=1;i<=opts[j].nc;i++) {fprintf(stderr," %s","<opt>");};
    if (opts[j].ni==0 && opts[j].nr==0 && opts[j].nc==0) {fprintf(stderr,"\t");};
    fprintf(stderr,"\t:%s\n",opts[j].d);
   };
  };
  fprintf(stderr,"\n");
  fprintf(stderr,"%s\n\n"," YAMBO developers group (http://www.yambo-code.org)");
};
