#
#        Copyright (C) 2000-2018 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM
#
# This file is distributed under the terms of the GNU
# General Public License. You can redistribute it and/or
# modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation;
# either version 2, or (at your option) any later version.
#
# This program is distributed in the hope that it will
# be useful, but WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
# MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
#
use experimental 'smartmatch';
#
sub have_ID
#===========
{
 if (exists($RUN_material[@_]))
 {
   return @_
 }
 return 0;
}
sub have_father
#===============
{
 $irun_fat=0;
 while ($irun_fat<$N_fathers) {
  $irun_fat++;
  if ("$father[$irun_fat]" =~ @_) {
   return 1;
  }
 } 
 return 0;
}
sub have_material
#=================
{
 $irun_mat=0;
 while ($irun_mat<$runs) {
  $irun_mat++;
  if ("$RUN_material[$irun_mat]" =~ @_) {
   return $irun_mat;
  }
 } 
 return 0;
}
sub have_run
#============
{
 $i_have_run=0;
 while ($i_have_run<$runs) 
 {
  $n_tags=0;
  $matches=0;
  $i_have_run++;
  if ($user_tags) {
   for($ik = 1; $ik < 100; $ik++) {
    if (exists($RUN_tag[$i_have_run][$ik])){
     $n_tags++;
     if ( "$RUN_tag[$i_have_run][$ik]" ~~ @tags) {$matches++};
    }
   }
  }
  if ($material and "$RUN_material[$i_have_run]" =~ "$material" and $n_tags eq  $matches){return $i_have_run;};
  if ("@_[0]" eq "ID" and $ID[$i_have_run] eq @_[1]) {return $i_have_run;}; 
  if ("@_[0]" eq "FATHER" and $RUN_father[$i_have_run] eq @_[1]) {return $i_have_run;}; 
 }
 return 0;
}
1;
