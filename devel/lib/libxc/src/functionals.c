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
#include <string.h>
#include <strings.h>
#include <assert.h>

#include "xc.h"
#include "funcs_key.c"

extern XC(func_info_type) 
  *XC(lda_known_funct)[], 
  *XC(gga_known_funct)[],
  *XC(hyb_gga_known_funct)[],
  *XC(mgga_known_funct)[];


/*------------------------------------------------------*/
int XC(functional_get_number)(char *name)
{
  int ii;

  for(ii=0;;ii++){
    if(XC(functional_keys)[ii].number == -1)
      return -1;
    if(strncasecmp(XC(functional_keys)[ii].name, name, 256) == 0) 
      return XC(functional_keys)[ii].number;
  }
}


/*------------------------------------------------------*/
char *XC(functional_get_name)(int number)
{
  int ii;

  for(ii=0;;ii++){
    if(XC(functional_keys)[ii].number == -1)
      return NULL;
    if(XC(functional_keys)[ii].number == number)
      /* return duplicated: caller has the responsability to dealloc string */
      return strdup(XC(functional_keys)[ii].name);
  }
}


/*------------------------------------------------------*/
int XC(family_from_id)(int id, int *family, int *number)
{
  int ii;

  /* first let us check if it is an LDA */
  for(ii=0; XC(lda_known_funct)[ii]!=NULL; ii++){
    if(XC(lda_known_funct)[ii]->number == id){
      if(family != NULL) *family = XC_FAMILY_LDA;
      if(number != NULL) *number = ii;
      return XC_FAMILY_LDA;
    }
  }

  /* or is it a GGA? */
  for(ii=0; XC(gga_known_funct)[ii]!=NULL; ii++){
    if(XC(gga_known_funct)[ii]->number == id){
      if(family != NULL) *family = XC_FAMILY_GGA;
      if(number != NULL) *number = ii;
      return XC_FAMILY_GGA;
    }
  }

  /* or is it a hybrid GGA? */
  for(ii=0; XC(hyb_gga_known_funct)[ii]!=NULL; ii++){
    if(XC(hyb_gga_known_funct)[ii]->number == id){
      if(family != NULL) *family = XC_FAMILY_HYB_GGA;
      if(number != NULL) *number = ii;
      return XC_FAMILY_HYB_GGA;
    }
  }

  /* or is it a meta GGA? */
  for(ii=0; XC(mgga_known_funct)[ii]!=NULL; ii++){
    if(XC(mgga_known_funct)[ii]->number == id){
      if(family != NULL) *family = XC_FAMILY_MGGA;
      if(number != NULL) *number = ii;
      return XC_FAMILY_MGGA;
    }
  }

  return XC_FAMILY_UNKNOWN;
}


/*------------------------------------------------------*/
int XC(func_init)(XC(func_type) *p, int functional, int nspin)
{
  int number;

  assert(p != NULL);
  assert(nspin==XC_UNPOLARIZED || nspin==XC_POLARIZED);

  p->nspin = nspin;

  switch(XC(family_from_id)(functional, NULL, &number)){
  case(XC_FAMILY_LDA):
    p->lda  = (XC(lda_type) *) malloc(sizeof(XC(lda_type)));
    p->info = XC(lda_known_funct)[number];
    return XC(lda_init)(p, p->info, nspin);

  case(XC_FAMILY_GGA):
    p->gga = (XC(gga_type) *) malloc(sizeof(XC(gga_type)));
    p->info = XC(gga_known_funct)[number];
    return XC(gga_init)(p, p->info, nspin);

  case(XC_FAMILY_HYB_GGA):
    p->gga = (XC(gga_type) *) malloc(sizeof(XC(gga_type)));
    p->info = XC(hyb_gga_known_funct)[number];
    return XC(gga_init)(p, p->info, nspin);

  case(XC_FAMILY_MGGA):
    p->mgga = (XC(mgga_type) *) malloc(sizeof(XC(mgga_type)));
    p->info = XC(mgga_known_funct)[number];
    return XC(mgga_init)(p, p->info, nspin);

  default:
    return -2; /* family not found */
  }
}


/*------------------------------------------------------*/
void XC(func_end)(XC(func_type) *p)
{
  assert(p != NULL && p->info != NULL);

  switch(p->info->family){
  case(XC_FAMILY_LDA):
    XC(lda_end)(p);
    free(p->lda);
    break;

  case(XC_FAMILY_GGA):
  case(XC_FAMILY_HYB_GGA):
    XC(gga_end)(p);
    free(p->gga);
    break;

  case(XC_FAMILY_MGGA):
    XC(mgga_end)(p);
    free(p->mgga);
    break;
  }

  p->info = NULL;  
}
