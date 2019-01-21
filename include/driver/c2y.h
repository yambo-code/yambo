/*
         Copyright (C) 2000-2019 the YAMBO team
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
/*
 Command line structure
*/
 static short_options_struct short_options[] = { /* Int Real Ch (dummy) Parallel_option*/
#include "common_options.h"
  {"nodbfr","U","Do not fragment the DataBases (only for serial runs)",0,0,0,0,0},
  {"alat_f","a","Lattice constants rescaling factor",0,1,0,0,0},
  {"dupl",  "d","States duplication (artificial spin polarization)",0,0,0,0,0},
  {"notr",  "t","Force use of spatial Inv. instead of Time Rev.",0,0,0,0,0},
  {"nosy",  "n","Force no symmetries",0,0,0,0,0},
  {"nowf",  "w","Force no wavefunctions",0,0,0,0,0},
  {NULL,NULL,NULL,0,0,0,0,0}
 };
