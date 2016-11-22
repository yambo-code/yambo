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
sub load_db
{
open(DB,"<","$DB_file");
$runs=0;
while(<DB>) { 
 @element = split(' ',$_);
 if ($element[1] eq "material")    { 
  $ID=0;
  $n_keys=0;
  $n_outs=0;
  $n_dbs=0;
  $n_ins=0;
  $runs++;
  $RUN_material[$runs]=$element[2];
 }
 $ID[$runs]=$element[0];
 if ($element[1] eq "date")        { $RUN_date[$ID[$runs]]=$element[2] } ;
 if ($element[1] eq "description") { $RUN_description[$ID[$runs]]=$element[2] } ;
 if ($element[1] eq "key")         { 
   $n_keys++;
   $RUN_key[$runs][$n_keys]=$element[2];
 } ;
 if ($element[1] eq "output")         { 
   $n_outs++;
   $RUN_out[$runs][$n_outs]=$element[2];
 } ;
 if ($element[1] eq "input")         { 
   $n_ins++;
   $RUN_in[$runs][$n_ins]=$element[2];
 } ;
 if ($element[1] eq "database")         { 
   $n_dbs++;
   $RUN_db[$runs][$n_dbs]=$element[2];
 } ;
};
close(DB);
}
1;
