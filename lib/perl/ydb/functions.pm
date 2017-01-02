#
#        Copyright (C) 2000-2016 the YAMBO team
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
no warnings 'experimental::smartmatch';
#
sub have_ID
#===========
{
 if (exists($RUN_description[@_]))
 {
   return @_
 }
 return 0;
}
sub have_material
#=================
{
 $irun=0;
 while ($irun<$runs) {
  $irun++;
  if ("$RUN_material[$irun]" =~ @_) {
   return $irun;
  }
 } 
 return 0;
}
sub have_run
#============
{
 $irun=0;
 while ($irun<$runs) 
 {
  $n_tags=0;
  $matches=0;
  $irun++;
  if ($user_tags) {
   for($ik = 1; $ik < 100; $ik++) {
    if (exists($RUN_tag[$irun][$ik])){
     $n_tags++;
     if ( "$RUN_tag[$irun][$ik]" ~~ @tags) {$matches++};
    }
   }
  }
  if ($material and "$RUN_material[$irun]" =~ "$material" and $n_tags eq  $matches){return $irun;};
  if ($ID_in and $ID[$irun] eq $ID_in) {return $irun;};
 }
 return 0;
}
1;
