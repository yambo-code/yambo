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
sub load_db
{
open(DB,"<","$DB_file");
$runs=0;
$N_fathers=0;
while(<DB>) { 
 @element = split(' ',$_);
 if ($element[1] eq "father")    { 
  $ID=0;
  $n_tags=0;
  $n_outs=0;
  $n_dbs=0;
  $n_ins=0;
  $runs++;
 }
 $ID[$runs]=$element[0];
 if ($element[1] eq "running")     { $RUN_run[$runs]=$element[2] } ;
 if ($element[1] eq "material")    { $RUN_material[$runs]=$element[2] } ;
 if ($element[1] eq "father")      { 
  $RUN_father[$runs]=$element[2];
  $F_found="no";
  for($if_here = 1; $if_here <= $N_fathers; $if_here++) {
    if ("$element[2]" =~ "$father[$if_here]") {$F_found="yes"};
  } 
  if ("$F_found" eq "no") {
   $N_fathers++;
   $father[$N_fathers]=$element[2];
  }
 }
 if ($element[1] eq "date")        { $RUN_date[$runs]=$element[2] } ;
 if ($element[1] eq "description") { 
  $desc_line  = substr $_, index($_,"description")+length("description")+1 ; 
  chomp($desc_line);
  @DESCs = split(/NEWLINE/,$desc_line);
  $id=0;
  foreach $single_desc (@DESCs) {
   chomp($single_desc);
   $id++;
   $RUN_description[$runs][$id]=$single_desc;
  }
 }
 if ($element[1] eq "tag")         { 
  for($ik = 2; $ik < 100; $ik++) {
   if ($element[$ik]){
     $n_tags++;
     $RUN_tag[$runs][$n_tags]=$element[$ik];
   }
  };
 };
 if ($element[1] eq "output")         { 
   $n_outs++;
   $RUN_out[$runs][$n_outs]=$element[2];
   if ($element[3]) {$RUN_out_tag[$runs][$n_outs]=$element[3]};
 } ;
 if ($element[1] eq "input")         { 
   $n_ins++;
   $RUN_in[$runs][$n_ins]=$element[2];
   if ($element[3]) {$RUN_in_tag[$runs][$n_ins]=$element[3]};
 } ;
 if ($element[1] eq "database")         { 
   $n_dbs++;
   $RUN_db[$runs][$n_dbs]=$element[2];
   if ($element[3]) {$RUN_db_tag[$runs][$n_dbs]=$element[3]};
 } ;
};
close(DB);
}
1;
