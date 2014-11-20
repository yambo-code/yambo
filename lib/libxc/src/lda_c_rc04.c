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

#define XC_LDA_C_RC04          27 /* Ragot-Cortona */

void 
XC(lda_c_rc04_func)(const XC(func_type) *p, XC(lda_work_t) *r)
{
  static FLOAT AA=-0.655868, BB=4.888270, CC=3.177037, DD=0.897889;

  FLOAT ek, dek, d2ek, d3ek, aux, aux2;
  FLOAT opz, omz, opz13, omz13, _opz13, _omz13, _opz43, _omz43, _opz73, _omz73, fz, fz2, dfz, d2fz, d3fz;

  aux = BB + CC*r->rs[1];
  ek  = (AA*atan(aux) + DD)/r->rs[1];

  if(p->nspin == XC_POLARIZED){
    opz = 1.0 + r->zeta;
    omz = 1.0 - r->zeta;

    opz13 = CBRT(opz);
    omz13 = CBRT(omz);

    fz  = 0.5*(opz13*opz13 + omz13*omz13);
    fz2 = fz*fz;

    r->zk = ek*fz2*fz;
  }else
    r->zk = ek;

  if(r->order < 1) return;

  aux2 = 1.0 + aux*aux;
  dek  = AA*CC/(r->rs[1]*aux2) - ek/r->rs[1];

  if(p->nspin == XC_POLARIZED){
    _opz13 = (opz == 0.0) ? 0.0 : 1.0/opz13;
    _omz13 = (omz == 0.0) ? 0.0 : 1.0/omz13;

    dfz = (_opz13 - _omz13)/3.0;

    r->dedrs = dek*fz2*fz;
    r->dedz  = ek*3.0*fz2*dfz;
  }else{
    r->dedrs = dek;
    r->dedz  = 0.0;
  }

  if(r->order < 2) return;

  d2ek = -2.0*AA*CC*CC*aux/(r->rs[1]*aux2*aux2) - 2.0*dek/r->rs[1];

  if(p->nspin == XC_POLARIZED){
    _opz43 = (opz == 0.0) ? 0.0 : 1.0/(opz*opz13);
    _omz43 = (omz == 0.0) ? 0.0 : 1.0/(omz*omz13);

    d2fz = -(_opz43 + _omz43)/9.0;
    
    r->d2edrs2 = d2ek*fz2*fz;
    r->d2edrsz =  dek*3.0*fz2*dfz;
    r->d2edz2  =   ek*3.0*fz*(2.0*dfz*dfz + fz*d2fz);
  }else{
    r->d2edrs2 = d2ek;
    r->d2edrsz = 0.0;
    r->d2edz2  = 0.0;
  }
  
  if(r->order < 3) return;

  d3ek = 2.0*AA*CC*CC*CC/(r->rs[1]*aux2*aux2)*(4.0*aux*aux/aux2 - 1.0) - 3.0*d2ek/r->rs[1];

  if(p->nspin == XC_POLARIZED){
    _opz73 = (opz == 0.0) ? 0.0 : 1.0/(opz*opz*opz13);
    _omz73 = (omz == 0.0) ? 0.0 : 1.0/(omz*omz*omz13);

    d3fz = (_opz73 - _omz73)*4.0/27.0;

    r->d3edrs3  = d3ek*fz2*fz;
    r->d3edrs2z = d2ek*3.0*fz2*dfz;
    r->d3edrsz2 =  dek*3.0*fz*(2.0*dfz*dfz + fz*d2fz);
    r->d3edz3   =   ek*3.0*(dfz*dfz*dfz + 6.0*fz*dfz*d2fz + fz2*d3fz);
  }else{
    r->d3edrs3  = d3ek;
    r->d3edrs2z = 0.0;
    r->d3edrsz2 = 0.0;
    r->d3edz3   = 0.0;
  }
}

#define func XC(lda_c_rc04_func)
#include "work_lda.c"
#undef func

const XC(func_info_type) XC(func_info_lda_c_rc04) = {
  XC_LDA_C_RC04,
  XC_CORRELATION,
  "Ragot-Cortona",
  XC_FAMILY_LDA,
  "S Ragot and P Cortona, J. Chem. Phys. 121, 7671 (2004)",
  XC_FLAGS_3D | XC_FLAGS_HAVE_EXC | XC_FLAGS_HAVE_VXC | XC_FLAGS_HAVE_FXC | XC_FLAGS_HAVE_KXC,
  1e-25, 0.0, 0.0, 1e-32,
  NULL,     /* init */
  NULL,     /* end  */
  work_lda, /* lda  */
};
