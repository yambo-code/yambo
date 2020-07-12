/*
         Copyright (C) 2000-2020 the YAMBO team
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
#include <kind.h>

void options_interfaces(struct options_struct options[],int *i_opt)
{
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Do not fragment the DataBases (only for serial runs)";
 options[*i_opt].long_opt="nofrag";
 options[*i_opt].short_opt='U';
 options[*i_opt].bin="p2y a2y";
 options[*i_opt].yambo_string="nodbfr";
 options[*i_opt].serial_var=1;
 options[*i_opt].section="Interface";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Number of bands for each fragment";
 options[*i_opt].long_opt=  "fragbands";
 options[*i_opt].short_opt='b';
 options[*i_opt].bin="p2y a2y c2y";
 options[*i_opt].yambo_string="fragnb";
 options[*i_opt].int_var=1;
 options[*i_opt].section="Interface";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Lattice constants rescaling factor";
 options[*i_opt].long_opt=  "alat_factor";
 options[*i_opt].short_opt='a';
 options[*i_opt].float_var=1;
 options[*i_opt].bin="p2y a2y c2y";
 options[*i_opt].yambo_string="alat_f";
 options[*i_opt].section="Interface";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Force use of spatial Inv. instead of Time Rev.";
 options[*i_opt].long_opt="notr";
 options[*i_opt].short_opt='t';
 options[*i_opt].bin="p2y a2y c2y";
 options[*i_opt].yambo_string="notr";
 options[*i_opt].section="Interface";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Force no symmetries";
 options[*i_opt].long_opt=  "nosym";
 options[*i_opt].short_opt='n';
 options[*i_opt].bin="p2y a2y c2y";
 options[*i_opt].yambo_string="nosy";
 options[*i_opt].section="Interface";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Force no wavefunctions";
 options[*i_opt].long_opt="nowf";
 options[*i_opt].short_opt='w';
 options[*i_opt].bin="p2y a2y c2y";
 options[*i_opt].yambo_string="nowf";
 options[*i_opt].section="Interface";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="Verbose wfc I/O reporting";
 options[*i_opt].long_opt="verbio";
 options[*i_opt].short_opt='v';
 options[*i_opt].bin="p2y";
 options[*i_opt].yambo_string="verb";
 options[*i_opt].section="Interface";
 *i_opt=*i_opt+1;
 options[*i_opt].short_desc="States duplication (artificial spin polarization)";
 options[*i_opt].long_opt="duplicate";
 options[*i_opt].short_opt='d';
 options[*i_opt].bin="a2y c2y";
 options[*i_opt].yambo_string="dupl";
 options[*i_opt].section="Interface";
};
